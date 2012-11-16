{-# LANGUAGE OverloadedStrings #-}

{-

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

module Markdown {-(parseMarkdown, renderBlocks)-} where
import qualified Data.Map as M
import Control.Monad.State
import Data.List (intercalate)
import Data.Char (isAscii, isSpace, isPunctuation, isSymbol, isDigit)
import Network.URI (parseURI, URI(..), isAllowedInURI, escapeURIString)
import Data.Monoid ((<>))
import Data.Foldable (foldMap, toList)
import Control.Monad
import Control.Applicative hiding (many,optional,empty)
import Text.Parsec hiding (sepBy1, State, (<|>))
import Text.Parsec.Text
import Data.Sequence (Seq, singleton, empty, (<|))
import qualified Data.Sequence as Seq

import qualified Data.Text as T
import Data.Text ( Text )

import qualified Data.Attoparsec.Text as A

-- for HTML rendering
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as BT
import Text.Blaze.Html hiding(contents)

-- for debugging
import Debug.Trace

-- Replacement for Parsec's 'sepBy1', which does not have the @try@
-- and so does not behave as I want it to.
sepBy1 :: Parsec s u a -> Parsec s u b -> Parsec s u [a]
sepBy1 p sep = do
  first <- p
  rest <- many $ try $ sep >> p
  return (first:rest)

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


-----
-- TODO eventually we won't need this:
type P = GenParser ParserState

type Scanner = A.Parser ()

-- Try to match the scanner, returning Just the remaining text if
-- it matches, Nothing otherwise.
applyScanners :: [Scanner] -> Text -> Maybe Text
applyScanners scanners t =
  case A.parseOnly (sequence_ scanners >> A.takeText) t of
       Right t'   -> Just t'
       Left e     -> Nothing

-- Scanners

scanBlockquoteStart :: Scanner
scanBlockquoteStart =
  scanNonindentSpaces >> scanChar '>' >> opt (scanChar ' ')

scanNonindentSpaces :: Scanner
scanNonindentSpaces =
  (scanChar ' ' >>
    (scanChar ' ' >>
      (scanChar ' ' <|> return ())
    ) <|> return ()
  ) <|> return ()

scanChar :: Char -> Scanner
scanChar c = A.char c >> return ()

scanBlankline :: Scanner
scanBlankline = A.skipWhile isSpaceOrTab *> A.endOfInput

scanSpace :: Scanner
scanSpace = () <$ A.satisfy isSpaceOrTab

-- 0 or more spaces
scanSpaces :: Scanner
scanSpaces = A.skipWhile isSpaceOrTab

isSpaceOrTab :: Char -> Bool
isSpaceOrTab ' '  = True
isSpaceOrTab '\t' = True
isSpaceOrTab _    = False

scanIndentSpaces :: Scanner
scanIndentSpaces =
  scanChar '\t'
  <|> (scanChar ' ' >>
       (scanChar '\t' <|>
        (scanChar ' ' >>
         (scanChar '\t' <|>
           (scanChar ' ' >> scanSpace)))))

-- optional
opt :: Scanner -> Scanner
opt s = A.option () (s >> return ())

-- not followed by
nfb :: A.Parser a -> Scanner
nfb s = do
  succeeded <- A.option False (True <$ s)
  if succeeded
     then mzero
     else return ()

parseAtxHeaderStart :: A.Parser Int
parseAtxHeaderStart = do
  hashes <- A.takeWhile1 (=='#')
  scanSpace
  return $ T.length hashes

scanAtxHeaderStart :: Scanner
scanAtxHeaderStart = () <$ parseAtxHeaderStart

scanHRuleLine :: Scanner
scanHRuleLine = A.try $ do
  scanNonindentSpaces
  c <- A.satisfy (\c -> c == '*' || c == '-' || c == '_')
  A.count 2 $ A.try $ scanSpaces >> A.char c
  A.skipWhile (\x -> isSpaceOrTab x || x == c)
  A.endOfInput
  return ()

isCodeFenceChar :: Char -> Bool
isCodeFenceChar '`' = True
isCodeFenceChar '~' = True
isCodeFenceChar _   = False

scanCodeFenceLine :: Scanner
scanCodeFenceLine = () <$ parseCodeFenceLine

parseCodeFenceLine :: A.Parser (Text, Text)
parseCodeFenceLine = do
  c <- A.satisfy isCodeFenceChar
  A.count 2 (A.char c)
  extra <- A.takeWhile (== c)
  scanSpaces
  rawattr <- A.takeText
  return (T.pack [c,c,c] <> extra, rawattr)

isBulletChar :: Char -> Bool
isBulletChar '-' = True
isBulletChar '+' = True
isBulletChar '*' = True
isBulletChar _   = False

scanListMarker :: Scanner
scanListMarker = () <$ parseListMarker

parseListMarker :: A.Parser ListType
parseListMarker = parseBullet <|> parseListNumber

parseBullet :: A.Parser ListType
parseBullet = do
  c <- A.satisfy isBulletChar
  scanSpace
  nfb $ A.count 2 $ A.try $ scanSpaces >> A.char c -- hrule
  return $ Bullet c

parseListNumber :: A.Parser ListType
parseListNumber =
  (parseListNumberDig <|> parseListNumberPar) <* scanSpace <* scanSpaces
  where parseListNumberDig = A.try $ do
           num <- A.decimal
           wrap <-  PeriodFollowing <$ A.char '.'
                <|> ParenFollowing <$ A.char ')'
           return $ Numbered wrap num
        parseListNumberPar = A.try $ do
           A.char '('
           num <- A.decimal
           A.char ')'
           return $ Numbered ParensAround num

---


data BlockParserState = BlockParserState{
          inputLines    :: [Text]
        , references    :: ReferenceMap
        , lineScanners  :: [Scanner]
        , blockScanners :: [Scanner]
        }

type BlockParser = State BlockParserState

-- Add a scanner to the line scanner stack and run a parser,
-- then pop the scanner.
withLineScanner :: Scanner -> BlockParser a -> BlockParser a
withLineScanner scanner parser = do
  scanners <- gets lineScanners
  modify $ \st -> st{ lineScanners = scanners ++ [scanner] }
  result <- parser
  modify $ \st -> st{ lineScanners = scanners }
  return result

-- Add a scanner to the block scanner stack and run a parser,
-- then pop the scanner.
withBlockScanner :: Scanner -> BlockParser a -> BlockParser a
withBlockScanner scanner parser = do
  scanners <- gets blockScanners
  modify $ \st -> st{ blockScanners = scanners ++ [scanner] }
  result <- parser
  modify $ \st -> st{ blockScanners = scanners }
  return result

data ScanType  = BlockScan | LineScan deriving Eq
data OnSuccess = Peek | Consume deriving Eq

-- Apply scanners to next line, and return result if they match.
-- Skip over empty lines if blockStart.
nextLine :: OnSuccess -> ScanType -> BlockParser (Maybe Text)
nextLine onSuccess scanType = do
  lns <- gets inputLines
  scanners <- gets $ case scanType of
                           BlockScan -> blockScanners
                           LineScan  -> lineScanners
  case lns of
       []     -> return Nothing
       (x:xs)
         | isEmptyLine x && scanType == BlockScan -> do
                  modify $ \st -> st{ inputLines = xs }
                  nextLine onSuccess scanType
         | otherwise     -> do
                  case applyScanners scanners x of
                       Just x' -> do
                          when (onSuccess == Consume) $
                            modify $ \st -> st{ inputLines = xs }
                          return $ Just x'
                       Nothing -> return Nothing

parseBlocks :: Text -> (Blocks, ReferenceMap)
parseBlocks t = (bs, references s)
  where (bs, s) = runState blocksParser
                    BlockParserState{ inputLines = T.lines t
                                    , references = M.empty
                                    , lineScanners = []
                                    , blockScanners = []
                                    }

isEmptyLine :: Text -> Bool
isEmptyLine = T.all isSpChar
  where isSpChar ' '  = True
        isSpChar '\t' = True
        isSpChar _    = False

blocksParser :: BlockParser Blocks
blocksParser = nextLine Peek BlockScan >>= maybe (return empty) doLine
 where doLine ln  = do
          next <- tryScanners
                    [ (scanBlockquoteStart, parseBlockquote)
                    , (scanCodeFenceLine, parseCodeFence)
                    ] ln
          rest <- blocksParser
          return (next <> rest)
       tryScanners [] _            = parseLines
       tryScanners ((s,p):rest) ln = case applyScanners [s] ln of
                                          Just _  -> p
                                          Nothing -> tryScanners rest ln

parseBlockquote :: BlockParser Blocks
parseBlockquote = singleton . Blockquote <$>
  (withLineScanner (opt scanBlockquoteStart)
    $ withBlockScanner scanBlockquoteStart
        $ blocksParser)

parseCodeFence :: BlockParser Blocks
parseCodeFence = do
  next <- maybe "" id <$> nextLine Consume BlockScan
  case A.parseOnly parseCodeFenceLine next of
       Left _  -> return $ singleton $ Para $ singleton $ Str next
       Right (fence, rawattr) ->
         singleton . CodeBlock (parseCodeAttributes rawattr)
          . joinLines . reverse <$> getLines fence
   where getLines fence = do
           mbln <- nextLine Consume BlockScan
           case mbln of
                Nothing -> return []
                Just ln
                  | fence `T.isPrefixOf` ln -> return []
                  | otherwise -> (ln:) <$> getLines fence

parseCodeAttributes :: Text -> CodeAttr
parseCodeAttributes t = CodeAttr { codeLang = lang }
  where lang = case T.words (T.strip t) of
                     []    -> Nothing
                     (l:_) -> Just l

parseLines :: BlockParser Blocks
parseLines = do
  next <- nextLine Consume BlockScan
  processLines <$> maybe (return []) (\x ->
                          (x:) <$> (withLineScanner paraLine getLines)) next
 where getLines = nextLine Consume LineScan >>=
                    maybe (return []) (\x -> (x:) <$> getLines)
       paraLine =   nfb scanBlankline
                 >> nfb scanIndentSpaces
                 >> nfb scanBlockquoteStart
                 >> nfb scanAtxHeaderStart
                 >> nfb scanCodeFenceLine
                 >> nfb (scanSpaces >> scanListMarker)
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
       isSetext1Line x = not (T.null x) && T.all (=='=') (T.stripEnd x)
       isSetext2Line x = not (T.null x) && T.all (=='-') (T.stripEnd x)
       isSetextLine  x = isSetext1Line x || isSetext2Line x
       setextLevel   x = if isSetext1Line x then 1 else 2
       isHruleLine = maybe False (const True) . applyScanners [scanHRuleLine]
       isSpecialLine x = isSetextLine x || isHruleLine x



-- Utility parsers.

joinLines :: [Text] -> Text
joinLines = T.intercalate (T.pack "\n")

{-


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

pSp :: P ()
pSp = skipMany pSpaceChar

pSpnl :: P ()
pSpnl = try $ pSp *> optional (newline *> pSp)

pCodeFenceLine :: P (String, CodeAttr)
pCodeFenceLine = try $ do
  c <- oneOf "`~"
  count 2 (char c)
  extra <- many (char '`')
  attr <- parseCodeAttributes <$> pLine
  return (c:c:c:extra, attr)

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


-}

{-
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

pAtxHeaderStart :: P Int
pAtxHeaderStart = length <$> try (many1 (char '#') <* pSpaceChar)

pAtxHeader :: P Blocks
pAtxHeader = do
  lev <- pAtxHeaderStart
  singleton . Header lev . singleton . Markdown . stripClosingHashes <$> pLine
   where stripClosingHashes = T.strip . T.dropAround (=='#') . T.strip


-}

parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t = singleton $ Str t  -- TODO

{-
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
pSym = singleton . Str . T.singleton <$> (pEscapedChar <|> pNonspaceChar)

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
-}

-- blocks

parseMarkdown :: Text -> Blocks
parseMarkdown t = processBlocks refmap bls
  where (bls, refmap) = parseBlocks (t <> "\n")

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


