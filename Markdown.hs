{-# LANGUAGE OverloadedStrings #-}

{-

PERFORMANCE

Faster than pandoc and with better memory usage.
Both compiled with -O2.  Latest dev pandoc 1.10.
Benchmarked on 178K markdown file.

                          Markdown       pandoc -f markdown_strict
max bytes residency        2045484                         5752636
total cpu time              0.293s                          0.542s

Still not up with the C implementations.

TODO

_ explore a line-based approach?
  maybe don't need parser combinators, except for the scanners?

QUESTIONS

* nested quotes in link title?  seems silly, but some impls do?
* limit html blocks to list of html block tags?
* markdown=1 attribute?
* allow blank lines not to separate two code blocks?
   Markdown.pl fuses them. is this desirable?
   CURRENTLY THEY ARE TREATED AS SEPARATE.
* do we want the linebreak at end of code block?
  YES. TENTATIVELY. FOR CONFORMITY.
* in numbered lists, do we store list style?
  YES.
* in bullet lists, does new style start new list?
  YES. TENTATIVELY.
* is a 1-item list tight or loose?
  TIGHT.
* does a tight list end once we start getting loose items?
  YES - NOT YET VICE VERSA
* two blank lines end a list?
   YES
* two blockquotes w blank line between
   YES
* store entities as chars or entities?
   CURRENTLY AS ENTITIES
* how exactly do html blocks work?
* char encoding?
* tab handling?
* should we retain user line breaks?

-}

module Markdown (parseMarkdown, renderBlocks) where
import qualified Data.Map as M
import Data.List (partition, intercalate)
import Data.Char (isAscii, isSpace, isPunctuation, isSymbol)
import Network.URI (parseURI, URI(..), isAllowedInURI, escapeURIString)
import Data.Monoid ((<>))
import Data.Foldable (foldMap, toList)
import Control.Monad
import Control.Applicative hiding ((<|>),many,optional,empty)
import Text.Parsec hiding (sepBy, sepBy1)
import Text.Parsec.Text
import Data.Sequence (Seq, singleton, empty, (<|))
import qualified Data.Sequence as Seq

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text ( Text )

-- for HTML rendering
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.Text as BT
import Text.Blaze.Html hiding(contents)
import qualified Data.ByteString.Lazy as BL

-- for main loop
import System.Environment (getArgs)

-- for debugging
-- import Debug.Trace

-- Replacement for Parsec's 'sepBy1', which does not have the @try@
-- and so does not behave as I want it to.
sepBy1 :: P a -> P b -> P [a]
sepBy1 p sep = do
  first <- p
  rest <- many $ try $ sep >> p
  return (first:rest)

sepBy :: P a -> P b -> P [a]
sepBy p sep = sepBy1 p sep <|> return []

-- Structured representation of a document.

data CodeAttr = CodeAttr { codeLang :: Maybe Text }
              deriving Show

data Block = Para Inlines
           | Header Int Inlines
           | HRule
           | Blockquote Blocks
           | List Bool ListType [Blocks]
           | CodeBlock CodeAttr Text
           | HtmlBlock Text
           deriving Show
-- TODO raw html

type Blocks = Seq Block

data Inline = Str Text
            | Space
            | SoftBreak
            | LineBreak
            | Emph Inlines
            | Strong Inlines
            | Code Text
            | Link Inlines Text Text
            | Image Inlines Text Text
            | Entity Text
            | RawHtml Text
            | Markdown Text
            | Err Text Text
            deriving Show

type Inlines = Seq Inline

data NumWrapper = PeriodFollowing | ParenFollowing | ParensAround
                deriving (Eq,Show)

data ListType = Bullet Char | Numbered NumWrapper Int deriving Show

-- Defining the parser.

type ReferenceMap = M.Map Text (Text, Text)

addLinkReference :: Text -> (Text, Text) -> P ()
addLinkReference key (url,tit) = updateState $ \st ->
  st{ linkReferences = M.insert (T.toUpper key) (url,tit) (linkReferences st) }

lookupLinkReference :: Text -> P (Maybe (Text, Text))
lookupLinkReference key = do
  refmap <- linkReferences <$> getState
  return $ M.lookup (T.toUpper key) refmap

data ParserState = ParserState{
         beginLineScanners  :: [P ()]
       , beginBlockScanners :: [P ()]
       , linkReferences     :: ReferenceMap
       }

startingState :: ParserState
startingState = ParserState{
         beginLineScanners = []
       , beginBlockScanners = []
       , linkReferences = M.empty
       }

type P = GenParser ParserState

parseBlocks :: Text -> Either ParseError (Blocks, ReferenceMap)
parseBlocks = runP withRefs startingState "input"
  where withRefs = do x <- pDoc
                      y <- linkReferences <$> getState
                      return (x,y)

-- Functions to maintain two stacks of scanners:
--
-- * The "begin line scanners" are run at the beginning
--   of a line that continues an existing block.
-- * The "begin block scanners" are run at the beginning
--   of a new block.

withBeginLineScanner :: P () -> P a -> P a
withBeginLineScanner scanner p = try $ do
  updateState $ \st -> st{ beginLineScanners =
                           beginLineScanners st ++ [scanner] }
  result <- p
  updateState $ \st -> st{ beginLineScanners =
        case reverse (beginLineScanners st) of
                 (_:xs) -> reverse xs
                 []     -> [] }
  return result

withBeginBlockScanner :: P () -> P a -> P a
withBeginBlockScanner scanner p = try $ do
  updateState $ \st -> st{ beginBlockScanners =
                           beginBlockScanners st ++ [scanner] }
  result <- p
  updateState $ \st -> st{ beginBlockScanners =
        case reverse (beginBlockScanners st) of
                 (_:xs) -> reverse xs
                 []     -> [] }
  return result

-- When we begin parsing a block container, such as a blockquote
-- or list item, we push new scanners onto the begin line and
-- begin block stacks.  We then parse blocks until we can't
-- parse any more.  At that point we pop the scanners we added
-- and return the result.
--
-- Note: the scanners are applied in the order they were added
-- to the stack.  We apply the first one that was added, then
-- the second, etc.
--
-- After a newline, we try the begin line parsers.  If they succeed,
-- the next line is considered part of the current block.
-- Otherwise, we consider the block closed and try to parse
-- another block.

-- | Parse one or more blocks with new begin line and begin block
-- scanners.
pBlocks :: P () -> P () -> P Blocks
pBlocks beginLineScanner beginBlockScanner =
  withBeginLineScanner beginLineScanner $
    withBeginBlockScanner beginBlockScanner $
      msum <$> (pBlock `sepBy1` pBlockSep)

-- | Parse optional blanklines separating blocks, plus whatever
-- the begin block scanners require at the beginning of a new block.
pBlockSep :: P Int
pBlockSep = try $ do
  num <- length <$> many (try $ pBeginBlock >> pBlankline)
  pBeginBlock
  return num

-- | Parses line beginning for new block.
pBeginBlock :: P ()
pBeginBlock = try $ getState >>= sequence_ . beginBlockScanners

pBeginLine :: P ()
pBeginLine = try $ getState >>= sequence_ . beginLineScanners

-- Utility parsers.

-- | Applies a parser and returns the raw text that was parsed,
-- along with the value produced by the parser.
withRaw :: P a -> P (a, Text)
withRaw parser = do
  pos1 <- getPosition
  inp <- getInput
  result <- parser
  pos2 <- getPosition
  let (l1,c1) = (sourceLine pos1, sourceColumn pos1)
  let (l2,c2) = (sourceLine pos2, sourceColumn pos2)
  let inplines = take ((l2 - l1) + 1) $ T.lines inp
  let raw = case inplines of
                []   -> error "raw: inplines is null" -- shouldn't happen
                [l]  -> T.take (c2 - c1) l
                ls   -> T.unlines (init ls) <> T.take (c2 - 1) (last ls)
  return (result, raw)

pEscapedChar :: P Char
pEscapedChar = try $ char '\\' *> satisfy isEscapable

isEscapable :: Char -> Bool
isEscapable c = isSymbol c || isPunctuation c

-- parses a character satisfying the predicate, but understands escaped
-- symbols
pSatisfy :: (Char -> Bool) -> P Char
pSatisfy p =
  satisfy (\c -> c /= '\\' && p c)
   <|> try (char '\\' *> satisfy (\c -> isEscapable c && p c))

pAnyChar :: P Char
pAnyChar = pSatisfy (const True)

pNonspaceChar :: P Char
pNonspaceChar = pSatisfy (`notElem` " \t\r\n")

pBlankline :: P ()
pBlankline = try $ skipMany pSpaceChar <* newline

pBlockquoteStart :: P ()
pBlockquoteStart = try $ pNonindentSpaces *> char '>' *> optional (char ' ')

pIndentSpace :: P ()
pIndentSpace = try (() <$ count 4 (char ' '))
  <|> try (pNonindentSpaces >> char '\t' >> return ())

pNonindentSpaces :: P String
pNonindentSpaces = do
  option "" $ do
    char ' '
    option " " $ do
      char ' '
      option "  " $ do
        char ' '
        return "   "

pSpaceChar :: P Char
pSpaceChar = oneOf " \t"

pLine :: P Text
pLine = T.pack <$> (  manyTill (satisfy (/='\n')) newline
                  <|> many1 anyChar  -- for last line w/o newline
                   )

pCodeFenceLine :: P (String, CodeAttr)
pCodeFenceLine = try $ do
  c <- oneOf "`~"
  count 2 (char c)
  extra <- many (char '`')
  attr <- parseCodeAttributes <$> pLine
  return (c:c:c:extra, attr)

parseCodeAttributes :: Text -> CodeAttr
parseCodeAttributes t = CodeAttr { codeLang = lang }
  where lang = case T.words (T.strip t) of
                     []    -> Nothing
                     (l:_) -> Just l

-- | Parses one of a list of strings (tried in order).
oneOfStrings :: [String] -> P String
oneOfStrings []   = fail "no strings"
oneOfStrings strs = do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x == c]
  case strs' of
       []  -> fail "not found"
       z | "" `elem` z -> return [c]
         | otherwise   -> (c:) `fmap` oneOfStrings strs'

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: P (Text, Text)
uri = try $ do
  let protocols = [ "http:", "https:", "ftp:", "file:", "mailto:",
                    "news:", "telnet:" ]
  lookAhead $ oneOfStrings protocols
  -- Scan non-ascii characters and ascii characters allowed in a URI.
  -- We allow punctuation except when followed by a space, since
  -- we don't want the trailing '.' in 'http://google.com.'
  let innerPunct = try $ pSatisfy isPunctuation <*
                         notFollowedBy (newline <|> pSpaceChar)
  let uriChar = innerPunct <|>
                pSatisfy (\c -> not (isPunctuation c) &&
                            (not (isAscii c) || isAllowedInURI c))
  -- We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org)
  -- So we include balanced parens in the URL.
  let inParens = try $ do char '('
                          res <- many uriChar
                          char ')'
                          return $ '(' : res ++ ")"
  str <- concat <$> many1 (inParens <|> count 1 (innerPunct <|> uriChar))
  str' <- option str $ char '/' >> return (str ++ "/")
  -- now see if they amount to an absolute URI
  let escapeURI = escapeURIString (not . isSpace)
  case parseURI (escapeURI str') of
       Just uri' -> if uriScheme uri' `elem` protocols
                       then return (T.pack str', T.pack $ show uri')
                       else fail "not a URI"
       Nothing   -> fail "not a URI"

-- | Parses an email address; returns original and corresponding
-- escaped mailto: URI.
emailAddress :: P (Text, Text)
emailAddress = try $ do
  firstLetter <- alphaNum
  restAddr <- many emailChar
  let addr = firstLetter:restAddr
  char '@'
  dom <- domain
  let full = addr ++ '@':dom
  return (T.pack full, T.pack $ escapeURIString (not . isSpace)
                        $ "mailto:" ++ full)
 where emailChar = alphaNum <|> oneOf "-+_."
       domainChar = alphaNum <|> char '-'
       domain = intercalate "." <$> (many1 domainChar `sepBy1` (char '.'))

data HtmlTagType = Opening Text | Closing Text | SelfClosing Text

-- returns name of tag needed to close, and whole tag
pHtmlTag :: P (HtmlTagType, Text)
pHtmlTag = try $ do
  char '<'
  -- do not end the tag with a > character in a quoted attribute.
  closing <- (char '/' >> return True) <|> return False
  tagname <- T.toLower . T.pack <$> many1 alphaNum
  lookAhead $ oneOf " \t\r\n/>"
  chunks <- manyTill (pQuoted '\'' <|> pQuoted '"'
                       <|> many1 (noneOf "\"'<>"))  (char '>')
  let body = concat chunks
  let tagtype = if closing
                   then Closing tagname
                   else case reverse body of
                         ('/':_) -> SelfClosing tagname
                         _       -> Opening tagname
  return (tagtype, T.pack ('<' : ['/' | closing]) <> tagname <> T.pack body
                            <> ">")

pQuoted :: Char -> P String
pQuoted c = try $ do
  char c
  contents <- manyTill (satisfy (/= c)) (char c)
  return (c : contents ++ [c])

-- Block-level parsers.

pDoc :: P Blocks
pDoc = pBlocks (return ()) (return ()) <* skipMany pBlankline <* eof

pBlock :: P Blocks
pBlock = pBlockquote
     <|> pAtxHeader
     <|> pHrule
     <|> pList
     <|> pCodeFence
     <|> pCodeBlock
     <|> pReference
     <|> pHtmlBlock
     <|> pPara

pInBalancedTags :: P Text
pInBalancedTags = try $ do
  (tagtype, opener) <- pHtmlTag
  case tagtype of
       SelfClosing _ -> return opener
       Closing _     -> mzero
       Opening name  -> (opener <>) <$> getRest name
  where getRest name = try $ do
          nontag <- T.pack <$> many (satisfy (/='<'))
          (tagtype', x') <- lookAhead pHtmlTag
          case tagtype' of
               Closing n | n == name -> do
                 _ <- pHtmlTag
                 return $ nontag <> x'
               Opening n | n == name -> do
                 chunk <- pInBalancedTags
                 rest <- getRest name
                 return $ nontag <> chunk <> rest
               _  -> do
                 _ <- pHtmlTag
                 rest <- getRest name
                 return $ (nontag <> x') <> rest

pHtmlBlock :: P Blocks
pHtmlBlock = singleton . HtmlBlock <$>
  ((pInBalancedTags <|> pHtmlComment) <* skipMany pBlankline)

pHtmlComment :: P Text
pHtmlComment = try $ do
  string "<!--"
  rest <- manyTill anyChar (try $ string "-->")
  return $ "<!--" <> T.pack rest <> "-->"

pReference :: P Blocks
pReference = try $ do
  pNonindentSpaces
  lab <- pLinkLabel
  char ':'
  pSpnl
  url <- pLinkUrl
  tit <- option T.empty $ try $ pSpnl >> pLinkTitle
  pBlankline
  addLinkReference lab (url,tit)
  return empty

pLinkLabel :: P Text
pLinkLabel = try $ char '[' *>
  (T.concat <$> (manyTill (regChunk <|> bracketed <|> codeChunk) (char ']')))
  where regChunk = T.pack <$> many1 (pSatisfy (`notElem` "`[]"))
        codeChunk = snd <$> withRaw pCode
        bracketed = inBrackets <$> pLinkLabel
        inBrackets t = "[" <> t <> "]"

pLinkUrl :: P Text
pLinkUrl = try $ do
  inPointy <- (char '<' >> return True) <|> return False
  T.pack <$> if inPointy
                then manyTill (pSatisfy (`notElem` "\r\n>")) (char '>')
                else concat <$> many (regChunk <|> parenChunk)
               where regChunk = many1 (pSatisfy (`notElem` " \t\r\n()"))
                     parenChunk = inParens . concat <$>
                                  (char '(' *>
                                  manyTill (regChunk <|> parenChunk) (char ')'))
                     inParens x = '(' : x ++ ")"

pLinkTitle :: P Text
pLinkTitle = T.pack <$> (pLinkTitleDQ <|> pLinkTitleSQ <|> pLinkTitleP)
  where pLinkTitleDQ = try $ char '"' *> manyTill pAnyChar (char '"')
        pLinkTitleSQ = try $ char '\'' *> manyTill pAnyChar (char '\'')
        pLinkTitleP  = try $ char '(' *> manyTill pAnyChar (char ')')

pHruleLine :: P ()
pHruleLine = do
  pNonindentSpaces
  c <- oneOf "*-_"
  count 2 (try $ pSp >> char c)
  skipMany $ try $ pSp >> char c
  pSp
  lookAhead newline
  return ()

pHrule :: P Blocks
pHrule = singleton HRule <$ try pHruleLine

pSp :: P ()
pSp = skipMany pSpaceChar

pSpnl :: P ()
pSpnl = try $ pSp *> optional (newline *> pSp)

joinLines :: [Text] -> Text
joinLines = T.intercalate (T.pack "\n")

-- handles paragraphs and setext headers and hrules
pPara :: P Blocks
pPara = processLines . filter (not . T.null)
       <$> withBeginLineScanner paraLine (pLine `sepBy1` pBeginLine)
 where paraLine = notFollowedBy pBlankline
                >> notFollowedBy pIndentSpace
                >> notFollowedBy pBlockquoteStart
                >> notFollowedBy pAtxHeaderStart
                >> notFollowedBy (try $ pSp >> pListMarker)
                >> notFollowedBy pCodeFenceLine
       markdown = singleton . Markdown . T.strip
       processLines [] = empty
       processLines ws =
         case break isSpecialLine ws of
               (xs, [])           -> singleton $ Para $ markdown $ joinLines xs
               (xs,(y:ys))
                 | isSetextLine y ->
                     case reverse xs of
                           []     -> Header (setextLevel y) empty
                                     <| processLines ys
                           [z]    -> Header (setextLevel y) (markdown z)
                                     <| processLines ys
                           (z:zs) -> Para (markdown $ joinLines $ reverse zs)
                                  <| Header (setextLevel y) (markdown z)
                                  <| processLines ys
                 | isHruleLine y  ->
                     case xs of
                           []     -> HRule
                                     <| processLines ys
                           _      -> Para (markdown $ joinLines xs)
                                     <| HRule
                                     <| processLines ys
                 | otherwise      -> error "Should not happen"
       isSetext1Line x = not (T.null x) && T.all (=='=') x
       isSetext2Line x = not (T.null x) && T.all (=='-') x
       isSetextLine  x = isSetext1Line x || isSetext2Line x
       setextLevel   x = if isSetext1Line x then 1 else 2
       isHruleLine x = case runP pHruleLine startingState "" x of
                            Right _ -> True
                            Left _  -> False
       isSpecialLine x = isSetextLine x || isHruleLine x

pBlockquote :: P Blocks
pBlockquote = singleton . Blockquote <$>
  try (pBlockquoteStart >> pBlocks (optional pBlockquoteStart) pBlockquoteStart)

pCodeFence :: P Blocks
pCodeFence = try $ do
  (fence, attr) <- pCodeFenceLine
  lns <- manyTill pLine (try $ string fence >> pLine)
  return $ singleton $ CodeBlock attr $ T.unlines lns

pCodeBlock :: P Blocks
pCodeBlock = try $ do
  pIndentSpace
  singleton . CodeBlock CodeAttr{ codeLang = Nothing } . T.unlines
    <$> withBeginLineScanner pIndentSpace (pLine `sepBy1` pBeginLine)

pList :: P Blocks
pList = try $ do
  sps <- pNonindentSpaces
  col <- sourceColumn <$> getPosition
  listType <- pListMarker
  col' <- sourceColumn <$> getPosition
  let sublistIndent = () <$ count (col' - col - 1) (char ' ')
  let starter = try $ string sps *> pListStart listType
  let listItemBlocks =
        pBlocks (notFollowedBy
                  (try $ string sps >> sublistIndent >> pSp >> pListMarker))
                (try $ (string sps >> sublistIndent)
                   <|> lookAhead (try $ pBlankline *> notFollowedBy pBlankline))
  first <- listItemBlocks
  isTight <- (== 0) <$> (lookAhead pBlockSep <|> return 0)
  let listItem = try $ do
        num <- pBlockSep
        when (isTight && num > 0) $
           fail "Change in tightness ends list"
        starter
        blocks <- listItemBlocks
        return blocks
  rest <- many listItem
  let isTight' = if null rest then True else isTight
  return $ singleton $ List isTight' listType (first:rest)

pListMarker :: P ListType
pListMarker = pBullet <|> pListNumber

pBullet :: P ListType
pBullet = Bullet <$> oneOf "*+-" <* (pSpaceChar <|> lookAhead newline)

pListNumber :: P ListType
pListNumber =
  (pListNumberDig <|> pListNumberPar) <* (pSpaceChar <|> lookAhead newline)
  where pListNumberDig = try $ do
           num <- read <$> many1 digit
           wrap <-  PeriodFollowing <$ char '.'
                <|> ParenFollowing <$ char ')'
           return $ Numbered wrap num
        pListNumberPar = try $ do
           char '('
           num <- read <$> many1 digit
           char ')'
           return $ Numbered ParensAround num

pListStart :: ListType -> P ()
pListStart (Bullet   c) = () <$ notFollowedBy pHruleLine <* char c <* pSpaceChar
pListStart (Numbered w _) = try $ do
  marker <- pListNumber
  case marker of
        Numbered w' _ | w == w' -> return ()
        _                       -> fail "Change in list style"

pSetextHeaderLine :: P Int
pSetextHeaderLine = try $ do
  n <- (1 <$ skipMany1 (char '=')) <|> (2 <$ skipMany1 (char '-'))
  pBlankline
  return n

pAtxHeaderStart :: P Int
pAtxHeaderStart = length <$> try (many1 (char '#') <* pSpaceChar)

pAtxHeader :: P Blocks
pAtxHeader = do
  lev <- pAtxHeaderStart
  singleton . Header lev . singleton . Markdown . stripClosingHashes <$> pLine
   where stripClosingHashes = T.strip . T.dropAround (=='#') . T.strip

parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t =
  case runP (msum <$> many pInline <* eof) st "" t of
                           Left e  -> singleton $ Err t' (T.pack $ show e)
                           Right r -> r
 where st = startingState{ linkReferences = refmap }
       t' = T.strip t

pInline :: P Inlines
pInline =  pSpace
       <|> pUri
       <|> pStr
       <|> pStrong '*'
       <|> pStrong '_'
       <|> pEmph '*'
       <|> pEmph '_'
       <|> pLink
       <|> pImage
       <|> pCode
       <|> pEntity
       <|> pAutolink
       <|> pRawHtml
       <|> pSym

pSpace :: P Inlines
pSpace = singleton <$> (pSpaceSpace <|> pSpaceNewline)
  where pSpaceSpace = try $ pSpaceChar >>
            (pSpaceNewline <|> pSpaceLB <|> return Space)
        pSpaceLB = try $ pSpaceChar >> pSp >>
                      ((pSpaceNewline >> return LineBreak) <|> return Space)
        pSpaceNewline = newline >> pSp >> return SoftBreak

pStr :: P Inlines
pStr = do
  first <- alphaNum
  rest <- many $ alphaNum <|> (try $ char '_' <* lookAhead alphaNum)
  return $ singleton . Str . T.pack $ first:rest

pSym :: P Inlines
pSym = singleton . Str . T.singleton <$> pNonspaceChar

pUri :: P Inlines
pUri = do
  (orig,escaped) <- uri
  return $ singleton $ Link (singleton $ Str orig) escaped (T.empty)

pEmph :: Char -> P Inlines
pEmph c = try $ do
  char c
  notFollowedBy pSpaceChar
  contents <- msum <$>
     many1 ( (try $ notFollowedBy (char c) >> pInline) <|> pStrong c )
  (char c >> return (singleton (Emph contents)))
    <|> return (Str (T.pack "*") <| contents)

pStrong :: Char -> P Inlines
pStrong c = try $ do
  let marker = try $ char c >> char c
  marker
  notFollowedBy pSpaceChar
  contents <- msum <$> many1 (try $ notFollowedBy marker >> pInline)
  marker
  return (singleton (Strong contents))

pCode :: P Inlines
pCode = try $ do
  numticks <- length <$> many1 (char '`')
  pSp
  let end = try $ count numticks (char '`') *> notFollowedBy (char '`')
  let nonBacktickSpan = many1 (noneOf "`")
  let backtickSpan = many1 (char '`')
  singleton . Code . T.strip . T.pack . concat
   <$> manyTill (nonBacktickSpan <|> backtickSpan) end

pLink :: P Inlines
pLink = try $ do
  lab <- pLinkLabel
  refmap <- linkReferences <$> getState
  let lab' = parseInlines refmap lab
  pInlineLink lab' <|> pReferenceLink lab lab'
    <|> return (singleton (Str "[") <> lab' <> singleton (Str "]"))

pInlineLink :: Inlines -> P Inlines
pInlineLink lab = try $ do
  char '('
  pSp
  url <- pLinkUrl
  tit <- option "" $ try $ pSpnl *> pLinkTitle <* pSp
  char ')'
  return $ singleton $ Link lab url tit

pReferenceLink :: Text -> Inlines -> P Inlines
pReferenceLink rawlab lab = try $ do
  ref <- option rawlab $ try $ pSpnl >> pLinkLabel
  let ref' = if T.null ref then rawlab else ref
  lookupResult <- lookupLinkReference ref'
  case lookupResult of
       Just (url,tit)  -> return $ singleton $ Link lab url tit
       Nothing         -> fail "Reference not found"

pImage :: P Inlines
pImage = try $ do
  char '!'
  let linkToImage (Link lab url tit) = Image lab url tit
      linkToImage x                  = x
  fmap linkToImage <$> pLink

pRawHtml :: P Inlines
pRawHtml = singleton . RawHtml . snd <$> pHtmlTag

pEntity :: P Inlines
pEntity = try $ do
  char '&'
  res <- pCharEntity <|> pDecEntity <|> pHexEntity
  char ';'
  return $ singleton $ Entity $ T.pack $ '&':res ++ ";"

pCharEntity :: P [Char]
pCharEntity = many1 letter

pDecEntity :: P [Char]
pDecEntity = try $ do
  char '#'
  res <- many1 digit
  return $ '#':res

pHexEntity :: P [Char]
pHexEntity = try $ do
  char '#'
  x <- oneOf "Xx"
  res <- many1 hexDigit
  return $ '#':x:res

pAutolink :: P Inlines
pAutolink = try $ char '<' *> (pUri <|> pEmailAddress) <* char '>'

pEmailAddress :: P Inlines
pEmailAddress = do
  (orig,escaped) <- emailAddress
  return $ singleton $ Link (singleton $ Str orig) escaped (T.empty)

-- blocks

parseMarkdown :: Text -> Either ParseError Blocks
parseMarkdown t =
  case parseBlocks (t <> "\n") of
       Left err            -> Left err
       Right (bls, refmap) -> Right $ processBlocks refmap bls

processBlocks :: ReferenceMap -> Blocks -> Blocks
processBlocks refmap = fmap processBl
  where processInlines = foldMap processInline
        processInline (Markdown t) = parseInlines refmap t
        processInline x = singleton x
        processBl :: Block -> Block
        processBl bl =
          case bl of
            Para ils -> Para $ processInlines ils
            Header n ils -> Header n $ processInlines ils
            Blockquote bls -> Blockquote $ processBlocks refmap bls
            List tight listType items ->
               List tight listType $ map (processBlocks refmap) items
            x -> x

renderBlocks :: Blocks -> Html
renderBlocks = foldMap renderBlock
  where renderBlock :: Block -> Html
        renderBlock (Header n ils)
          | n >= 1 && n <= 5 = withNl $ ([H.h1,H.h2,H.h3,H.h4,H.h5] !! (n - 1))
                                      $ renderInlines ils
          | otherwise        = withNl $ H.p (renderInlines ils)
        renderBlock (Para ils) = withNl $ H.p (renderInlines ils)
        renderBlock (HRule) = withNl $ H.hr
        renderBlock (Blockquote bs) =
          withNl $ H.blockquote $ "\n" <> renderBlocks bs
        renderBlock (CodeBlock attr t) =
          withNl $ case codeLang attr of
                        Nothing   -> base
                        Just lang -> base ! A.class_ (toValue lang)
          where base = H.pre $ H.code $ toHtml t
        renderBlock (List tight (Bullet _) items) =
          withNl $ H.ul $ "\n" <> mapM_ (li tight) items
        renderBlock (List tight (Numbered _ n) items) =
          if n == 1 then base else base ! A.start (toValue n)
          where base = withNl $ H.ol $ "\n" <> mapM_ (li tight) items
        renderBlock (HtmlBlock raw) = withNl $ H.preEscapedToMarkup raw
        withNl x = x <> "\n"

li :: Bool -> Blocks -> Html
li tight bs =
  if tight
     then case toList bs of
                [Para zs]         -> H.li (renderInlines zs) <> "\n"
                [Para zs, List{}] -> H.li (renderInlines zs <> "\n" <>
                                       renderBlocks (Seq.drop 1 bs)) <> "\n"
                _                 -> toLi bs
     else toLi bs
 where toLi x = (H.li $ renderBlocks x) <> "\n"

renderInlines :: Inlines -> Html
renderInlines = foldMap renderInline
  where renderInline :: Inline -> Html
        renderInline (Str t) = toHtml t
        renderInline Space   = " "
        renderInline SoftBreak = "\n"
        renderInline LineBreak = H.br <> "\n"
        renderInline (Emph ils) = H.em $ renderInlines ils
        renderInline (Strong ils) = H.strong $ renderInlines ils
        renderInline (Code t) = H.code $ toHtml t
        renderInline (Link ils url tit) =
          if T.null tit then base else base ! A.title (toValue tit)
          where base = H.a ! A.href (toValue url) $ renderInlines ils
        renderInline (Image ils url tit) =
          if T.null tit then base else base ! A.title (toValue tit)
          where base = H.img ! A.src (toValue url)
                             ! A.alt (toValue $ BT.renderHtml -- TODO strip tags
                                              $ renderInlines ils)
        renderInline (Entity t) = H.preEscapedToMarkup t
        renderInline (RawHtml t) = H.preEscapedToMarkup t
        renderInline (Markdown t) = toHtml t -- shouldn't happen
        renderInline (Err t e) = H.span ! A.class_ "error"
                                        ! A.title (toValue e)
                                        $ toHtml t


