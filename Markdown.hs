{-# LANGUAGE OverloadedStrings #-}

{-

TODO

* optimizations
* comment

QUESTIONS

* nested quotes in link title?  seems silly, but some impls do?
* limit html blocks to list of html block tags?
* how exactly do html blocks work?
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
   NO - but two blank lines separate blockquotes, just like lists
* store entities as chars or entities?
   CURRENTLY AS ENTITIES
* should we retain user line breaks?

-}

module Markdown {-(parseMarkdown, renderBlocks)-} where
import Prelude hiding (takeWhile)
import qualified Data.Map as M
import Control.Monad.State
import Data.Char (isAscii, isSpace, isPunctuation, isSymbol,
                    isDigit, isHexDigit, isAlphaNum, isLetter)
import Data.List (intersperse)
import Network.URI (parseURI, isAllowedInURI, escapeURIString)
import Data.Monoid ((<>), mconcat)
import Data.Foldable (foldMap, toList)
import Control.Applicative hiding (optional,empty)
import Data.Sequence (Seq, singleton, empty, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Data.Text as T
import Data.Text ( Text )
import Data.Attoparsec.Text

-- for HTML rendering
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as BT
import Text.Blaze.Html hiding(contents)

-- for debugging
-- import Debug.Trace
-- tr s = trace s (return ())


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

listMarkerWidth :: ListType -> Int
listMarkerWidth (Bullet _) = 1
listMarkerWidth (Numbered wrap n) =
  (if wrap == ParensAround then 2 else 1) +
  case n of
       _ | n < 10    -> 1
         | n < 100   -> 2
         | n < 1000  -> 3
         | n < 10000 -> 4
         | otherwise -> 5

-- Defining the parser.

type ReferenceMap = M.Map Text (Text, Text)

-- link references are case sensitive and ignore line breaks
normalizeReference :: Text -> Text
normalizeReference = T.toUpper . T.concat . T.split isWhitespace
  where isWhitespace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

addLinkReference :: Text -> (Text, Text) -> BlockParser ()
addLinkReference key (url,tit) = modify $ \st ->
  st{ references = M.insert (normalizeReference key) (url,tit) (references st) }

lookupLinkReference :: ReferenceMap -> Text -> Maybe (Text, Text)
lookupLinkReference refmap key = M.lookup (normalizeReference key) refmap

type Scanner = Parser ()

-- Try to match the scanner, returning Just the remaining text if
-- it matches, Nothing otherwise.
applyScanners :: [Scanner] -> Text -> Maybe Text
applyScanners scanners t =
  case parseOnly (sequence_ scanners >> takeText) t of
       Right t'   -> Just t'
       Left _err  -> Nothing

-- Scanners

scanBlockquoteStart :: Scanner
scanBlockquoteStart =
  scanNonindentSpaces >> scanChar '>' >> opt (scanChar ' ')

scanIndentSpace :: Scanner
scanIndentSpace = scanSpace >> scanSpace >> scanSpace >> scanSpace

scanNonindentSpaces :: Scanner
scanNonindentSpaces =
  (scanChar ' ' >>
    (scanChar ' ' >>
      (scanChar ' ' <|> return ())
    ) <|> return ()
  ) <|> return ()

scanChar :: Char -> Scanner
scanChar c = char c >> return ()

scanBlankline :: Scanner
scanBlankline = skipWhile (==' ') *> endOfInput

scanSpace :: Scanner
scanSpace = () <$ satisfy (==' ')

-- 0 or more spaces
scanSpaces :: Scanner
scanSpaces = skipWhile (==' ')

scanSpnl :: Scanner
scanSpnl = scanSpaces *> opt (endOfLine *> scanSpaces)

-- optional
opt :: Scanner -> Scanner
opt s = option () (s >> return ())

-- not followed by
nfb :: Parser a -> Scanner
nfb s = do
  succeeded <- option False (True <$ s)
  if succeeded
     then mzero
     else return ()

parseAtxHeaderStart :: Parser Int
parseAtxHeaderStart = do
  hashes <- takeWhile1 (=='#')
  scanSpace
  return $ T.length hashes

scanAtxHeaderStart :: Scanner
scanAtxHeaderStart = () <$ parseAtxHeaderStart

scanHRuleLine :: Scanner
scanHRuleLine = try $ do
  scanNonindentSpaces
  c <- satisfy (\c -> c == '*' || c == '-' || c == '_')
  count 2 $ try $ scanSpaces >> char c
  skipWhile (\x -> x == ' ' || x == c)
  endOfInput
  return ()

isCodeFenceChar :: Char -> Bool
isCodeFenceChar '`' = True
isCodeFenceChar '~' = True
isCodeFenceChar _   = False

scanCodeFenceLine :: Scanner
scanCodeFenceLine = () <$ codeFenceParserLine

codeFenceParserLine :: Parser (Text, Text)
codeFenceParserLine = try $ do
  c <- satisfy isCodeFenceChar
  count 2 (char c)
  extra <- takeWhile (== c)
  scanSpaces
  rawattr <- takeWhile (/='`')
  endOfInput
  return (T.pack [c,c,c] <> extra, rawattr)

scanHtmlBlockStart :: Scanner
scanHtmlBlockStart = ((pHtmlTag >>= guard . f . fst) <|> (() <$ string "<!--"))
  where f (Opening name) = name `Set.member` blockHtmlTags
        f (SelfClosing name) = name `Set.member` blockHtmlTags
        f _ = False

isBulletChar :: Char -> Bool
isBulletChar '-' = True
isBulletChar '+' = True
isBulletChar '*' = True
isBulletChar _   = False

scanListStart :: Maybe ListType -> Parser ()
scanListStart Nothing = () <$ parseListMarker
scanListStart (Just (Bullet   c)) = try $ do
  marker <- parseBullet
  case marker of
        Bullet c' | c == c' -> return ()
        _                   -> fail "Change in list style"
scanListStart (Just (Numbered w _)) = try $ do
  marker <- parseListNumber
  case marker of
        Numbered w' _ | w == w' -> return ()
        _                       -> fail "Change in list style"

parseListMarker :: Parser ListType
parseListMarker = parseBullet <|> parseListNumber

parseBullet :: Parser ListType
parseBullet = do
  c <- satisfy isBulletChar
  scanSpace <|> scanBlankline
  nfb $ count 2 $ try $ scanSpaces >> char c -- hrule
  return $ Bullet c

parseListNumber :: Parser ListType
parseListNumber =
  (parseListNumberDig <|> parseListNumberPar) <*
     ((scanSpace <* scanSpaces) <|> scanBlankline)
  where parseListNumberDig = try $ do
           num <- decimal
           wrap <-  PeriodFollowing <$ char '.'
                <|> ParenFollowing <$ char ')'
           return $ Numbered wrap num
        parseListNumberPar = try $ do
           char '('
           num <- decimal
           char ')'
           return $ Numbered ParensAround num

-- note: this requires reference labels to be on one line.
scanReference :: Scanner
scanReference = scanNonindentSpaces >> pLinkLabel >> scanChar ':' >>
  (scanSpace <|> endOfLine)

---


data BlockParserState = BlockParserState{
          inputLines    :: [Text]
        , lastLine      :: Maybe Text
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

data ScanType     = BlockScan | LineScan deriving Eq

-- Apply scanners to next line, and return result if they match.
-- Skip over empty lines if blockStart.
nextLine :: ScanType -> BlockParser (Maybe Text)
nextLine scanType = do
  lns <- gets inputLines
  scanners <- gets $ case scanType of
                           BlockScan -> blockScanners
                           LineScan  -> lineScanners
  case lns of
       []     -> return Nothing
       (x:xs) -> case applyScanners scanners x of
                      Just x' -> do
                         modify $ \st -> st{ inputLines = xs
                                           , lastLine = Just x }
                         return $ Just x'
                      Nothing -> return Nothing

tabFilter :: Int -> Text -> Text
tabFilter tabstop = T.concat . pad . T.split (== '\t')
  where pad []  = []
        pad [t] = [t]
        pad (t:ts) = let tl = T.length t
                         n  = tl + tabstop - (tl `mod` tabstop)
                         in  T.justifyLeft n ' ' t : pad ts

parseBlocks :: Text -> (Blocks, ReferenceMap)
parseBlocks t = (bs, references s)
  where (bs, s) = runState (blocksParser Nothing)
                    BlockParserState{ inputLines = map (tabFilter 4) $ T.lines t
                                    , lastLine = Nothing
                                    , references = M.empty
                                    , lineScanners = []
                                    , blockScanners = []
                                    }

isEmptyLine :: Text -> Bool
isEmptyLine = T.all isSpChar
  where isSpChar ' '  = True
        isSpChar _    = False

blocksParser :: Maybe Text -> BlockParser Blocks
blocksParser mbln =
  case mbln of
       Nothing -> nextLine BlockScan >>= maybe (return empty) doLine
       Just ln -> doLine ln
 where doLine ln
         | isEmptyLine ln = do
             xs <- gets inputLines
             case xs of
                  (x:_) | isEmptyLine x -> do
                      -- two blanklines ends block parsing in a container
                      bscs <- gets blockScanners
                      if null bscs
                         then blocksParser Nothing
                         else return empty
                  _ -> blocksParser Nothing
         | otherwise = do
          next <- tryScanners
                    [ (scanBlockquoteStart, blockquoteParser)
                    , (scanIndentSpace, indentedCodeBlockParser)
                    , (scanAtxHeaderStart, atxHeaderParser)
                    , (scanCodeFenceLine, codeFenceParser)
                    , (scanReference, referenceParser)
                    , (scanNonindentSpaces >> scanListStart Nothing, listParser)
                    , (scanHtmlBlockStart, htmlBlockParser)
                    , (return (), parseLines)
                    ] ln
          rest <- blocksParser Nothing
          return (next <> rest)
       tryScanners [] _            = error "Empty scanner list"
       tryScanners ((s,p):rest) ln = case applyScanners [s] ln of
                                          Just ln' -> p ln ln'
                                          Nothing  -> tryScanners rest ln

blockquoteParser :: Text -> Text -> BlockParser Blocks
blockquoteParser _ firstLine = singleton . Blockquote <$>
  (withLineScanner (opt scanBlockquoteStart)
    $ withBlockScanner scanBlockquoteStart
        $ blocksParser $ Just firstLine)

indentedCodeBlockParser :: Text -> Text -> BlockParser Blocks
indentedCodeBlockParser _ ln = do
  lns <- withLineScanner (scanIndentSpace <|> scanBlankline) $ getLines
  return $ singleton . CodeBlock CodeAttr{ codeLang = Nothing } .  T.unlines
     . reverse . dropWhile T.null . reverse $ (ln:lns)
 where getLines = nextLine LineScan >>=
                    maybe (return []) (\l -> (l:) <$> getLines)

atxHeaderParser :: Text -> Text -> BlockParser Blocks
atxHeaderParser ln _ = do
  let ln' = T.strip $ T.dropAround (=='#') ln
  let inside = if "\\" `T.isSuffixOf` ln' && "#" `T.isSuffixOf` ln
                       then ln' <> "#"  -- escaped final #
                       else ln'
  case parseOnly parseAtxHeaderStart ln of
        Left _  -> return $ singleton $ Para $ singleton $ Str ln
        Right lev -> return
                     $ singleton . Header lev . singleton . Markdown $ inside

codeFenceParser :: Text -> Text -> BlockParser Blocks
codeFenceParser ln _ = do
  case parseOnly codeFenceParserLine ln of
       Left _  -> return $ singleton $ Para $ singleton $ Str ln
       Right (fence, rawattr) ->
         singleton . CodeBlock (parseCodeAttributes rawattr)
          . T.unlines . reverse <$> getLines fence
   where getLines fence = do
           mbln <- nextLine LineScan
           case mbln of
                Nothing -> return []
                Just l
                  | fence `T.isPrefixOf` l -> return []
                  | otherwise -> (l:) <$> getLines fence

parseCodeAttributes :: Text -> CodeAttr
parseCodeAttributes t = CodeAttr { codeLang = lang }
  where lang = case T.words (T.strip t) of
                     []    -> Nothing
                     (l:_) -> Just l

referenceParser :: Text -> Text -> BlockParser Blocks
referenceParser first _ = do
  let getLines = do
             mbln <- nextLine LineScan
             case mbln of
                  Nothing  -> return []
                  Just ln  -> (ln:) <$> getLines
  rest <- withLineScanner (nfb scanBlankline >> nfb scanReference) getLines
  let raw = joinLines (first:rest)
  case parseOnly pReference raw of
       Left  _               -> return $ singleton $ Para
                                       $ singleton $ Markdown raw
       Right (lab, url, tit) -> empty <$ addLinkReference lab (url,tit)

pReference :: Parser (Text, Text, Text)
pReference = do
  scanNonindentSpaces
  lab <- pLinkLabel
  char ':'
  scanSpnl
  url <- pLinkUrl
  scanSpnl
  tit <- option T.empty $ try $ scanSpnl >> pLinkTitle
  scanSpaces
  endOfInput
  return (lab, url, tit)

listParser :: Text -> Text -> BlockParser Blocks
listParser first first' = do
  let listStart = do
        initialSpaces <- takeWhile (==' ')
        listType <- parseListMarker
        return (initialSpaces, listType)
  (initialSpaces, listType) <-
        case parseOnly listStart first of
             Left _   -> fail "Could not parse list marker"
             Right r  -> return r
  let scanContentsIndent = () <$ count
         (T.length initialSpaces + listMarkerWidth listType) (skip (==' '))
  let starter = try $ string initialSpaces *> scanListStart (Just listType)
  let blockScanner = scanContentsIndent <|> scanBlankline
  let lineScanner = nfb $
              scanContentsIndent >> scanSpaces >> scanListStart Nothing
  -- TODO this needs work for input like
  -- +   list
  --     item
  -- The problem is that the second line is interpreted as a block start.
  firstItem <- withBlockScanner blockScanner
               $ withLineScanner lineScanner
               $ blocksParser $ Just first'
  prev <- gets lastLine
  let isTight = case prev of
                     Just l | not (isEmptyLine l) -> True
                     _                            -> False
  restItems <- listItemsParser isTight starter blockScanner lineScanner
  let isTight' = isTight || null restItems
  return $ singleton $ List isTight' listType (firstItem:restItems)

listItemsParser :: Bool -> Scanner -> Scanner -> Scanner -> BlockParser [Blocks]
listItemsParser isTight starter blockScanner lineScanner = do
  mbfirst <- withBlockScanner starter $ nextLine BlockScan
  case mbfirst of
       Nothing    -> return []
       Just first -> do
         item <- withBlockScanner blockScanner
                 $ withLineScanner lineScanner
                 $ blocksParser $ Just first
         prev <- gets lastLine
         rest <- case prev of
                      Just l | isEmptyLine l && isTight -> return []
                      _                                 ->
                       listItemsParser isTight starter blockScanner lineScanner
         return (item:rest)

parseLines :: Text -> Text -> BlockParser Blocks
parseLines _ firstLine = do
  processLines <$> (firstLine:) <$> withLineScanner paraLine getLines
 where getLines = nextLine LineScan >>=
                    maybe (return []) (\x -> (x:) <$> getLines)
       paraLine =   nfb scanBlankline
                 >> nfb scanIndentSpace
                 >> nfb scanBlockquoteStart
                 >> nfb scanAtxHeaderStart
                 >> nfb scanCodeFenceLine
                 >> nfb (scanSpaces >> scanListStart Nothing)

processLines :: [Text] -> Blocks
processLines [] = empty
processLines ws =
  case break isSpecialLine ws of
        (xs, [])           -> singleton $ Para $ markdown $ joinLines xs
        ([],(y:ys))
          | isHruleLine y  -> HRule
                              <| processLines ys
          | otherwise      -> Para (markdown y)
                              <| processLines ys
        (xs,(y:ys))
          | isSetextLine y ->
              case reverse xs of
                    []     -> error "Should not happen"
                    [z]    -> Header (setextLevel y) (markdown z)
                              <| processLines ys
                    (z:zs) -> Para (markdown $ joinLines $ reverse zs)
                           <| Header (setextLevel y) (markdown z)
                           <| processLines ys
          | isHruleLine y  -> Para (markdown $ joinLines xs)
                              <| HRule
                              <| processLines ys
          | otherwise      -> error "Should not happen"
  where isSetext1Line x = not (T.null x) && T.all (=='=') (T.stripEnd x)
        isSetext2Line x = not (T.null x) && T.all (=='-') (T.stripEnd x)
        isSetextLine  x = isSetext1Line x || isSetext2Line x
        setextLevel   x = if isSetext1Line x then 1 else 2
        isHruleLine = maybe False (const True) . applyScanners [scanHRuleLine]
        isSpecialLine x = isSetextLine x || isHruleLine x
        markdown = singleton . Markdown . T.strip

-- Utility parsers.

joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

pEscapedChar :: Parser Char
pEscapedChar = try $ char '\\' *> satisfy isEscapable

isEscapable :: Char -> Bool
isEscapable c = isSymbol c || isPunctuation c

-- parses a character satisfying the predicate, but understands escaped
-- symbols
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p =
  satisfy (\c -> c /= '\\' && p c)
   <|> try (char '\\' *> satisfy (\c -> isEscapable c && p c))

pAnyChar :: Parser Char
pAnyChar = pSatisfy (const True)

pNonspaceChar :: Parser Char
pNonspaceChar = pSatisfy isNonspaceChar
  where isNonspaceChar ' '  = False
        isNonspaceChar '\n' = False
        isNonspaceChar '\r' = False
        isNonspaceChar _    = True

data HtmlTagType = Opening Text | Closing Text | SelfClosing Text deriving Show

-- returns name of tag needed to close, and whole tag
pHtmlTag :: Parser (HtmlTagType, Text)
pHtmlTag = try $ do
  char '<'
  -- do not end the tag with a > character in a quoted attribute.
  closing <- (char '/' >> return True) <|> return False
  tagname <- T.toLower <$>
                takeWhile1 (\c -> isAlphaNum c || c == '?' || c == '!')
  let attr = do ss <- takeWhile isSpace
                x <- letter
                xs <- takeWhile (\c -> isAlphaNum c || c == ':')
                skip (=='=')
                v <- pQuoted '"' <|> pQuoted '\'' <|> takeWhile1 isAlphaNum
                      <|> return ""
                return $ ss <> T.singleton x <> xs <> "=" <> v
  attrs <- T.concat <$> many (try attr)
  final <- takeWhile (\c -> isSpace c || c == '/')
  char '>'
  let tagtype = if closing
                   then Closing tagname
                   else case T.stripSuffix "/" final of
                         Just _  -> SelfClosing tagname
                         Nothing -> Opening tagname
  return (tagtype,
          T.pack ('<' : ['/' | closing]) <> tagname <> attrs <> final <> ">")

pHtmlComment :: Parser Text
pHtmlComment = try $ do
  string "<!--"
  rest <- manyTill anyChar (try $ string "-->")
  return $ "<!--" <> T.pack rest <> "-->"

pQuoted :: Char -> Parser Text
pQuoted c = try $ do
  char c
  contents <- takeTill (== c)
  char c
  return (T.singleton c <> contents <> T.singleton c)

pLinkLabel :: Parser Text
pLinkLabel = try $ char '[' *> (T.concat <$>
  (manyTill (regChunk <|> bracketed <|> codeChunk) (char ']')))
  where regChunk = T.pack <$> many1 (pSatisfy (notInClass "`[]"))
        codeChunk = snd <$> pCode'
        bracketed = inBrackets <$> pLinkLabel
        inBrackets t = "[" <> t <> "]"

pLinkUrl :: Parser Text
pLinkUrl = try $ do
  inPointy <- (char '<' >> return True) <|> return False
  if inPointy
     then takeWhile (notInClass "\r\n>") <* char '>'
     else T.concat <$> many (regChunk <|> parenChunk)
    where regChunk = takeWhile1 (notInClass " \r\n()")
          parenChunk = inParens . T.concat <$> (char '(' *>
                         manyTill (regChunk <|> parenChunk) (char ')'))
          inParens x = "(" <> x <> ")"

pLinkTitle :: Parser Text
pLinkTitle = T.pack <$> (pLinkTitleDQ <|> pLinkTitleSQ <|> pLinkTitleP)
  where pLinkTitleDQ = try $ char '"' *> manyTill pAnyChar (char '"')
        pLinkTitleSQ = try $ char '\'' *> manyTill pAnyChar (char '\'')
        pLinkTitleP  = try $ char '(' *> manyTill pAnyChar (char ')')

blockHtmlTags :: Set.Set Text
blockHtmlTags = Set.fromList
 [ "article", "header", "aside", "hgroup", "blockquote", "hr",
   "body", "li", "br", "map", "button", "object", "canvas", "ol",
   "caption", "output", "col", "p", "colgroup", "pre", "dd",
   "progress", "div", "section", "dl", "table", "dt", "tbody",
   "embed", "textarea", "fieldset", "tfoot", "figcaption", "th",
   "figure", "thead", "footer", "footer", "tr", "form", "ul",
   "h1", "h2", "h3", "h4", "h5", "h6", "video"]

htmlBlockParser :: Text -> Text -> BlockParser Blocks
htmlBlockParser ln _ = go (parse pHtmlBlock ln)
  where go x = case x of
                    Fail unparsed _ _ -> return $ processLines
                                                $ T.lines unparsed
                    Partial f         -> nextLine BlockScan >>=
                                             go . f .  maybe "" ("\n" <>)
                    Done unparsed r   -> return $ r <>
                                         if T.null unparsed
                                            then empty
                                            else processLines (T.lines unparsed)

pInBalancedTags :: Maybe (HtmlTagType, Text) -> Parser Text
pInBalancedTags mbtag = try $ do
  (tagtype, opener) <- maybe pHtmlTag return mbtag
  case tagtype of
       SelfClosing _ -> return opener
       Closing _     -> mzero
       Opening name  -> (opener <>) <$> getRest name
  where getRest name = try $ do
          nontag <- T.pack <$> many (notChar '<')
          (tagtype', x') <- pHtmlTag
          case tagtype' of
               Closing n | n == name -> do
                 return $ nontag <> x'
               Opening n | n == name -> do
                 chunk <- pInBalancedTags (Just (tagtype',x'))
                 rest <- getRest name
                 return $ nontag <> chunk <> rest
               _  -> ((nontag <> x') <>) <$> getRest name

pHtmlBlock :: Parser Blocks
pHtmlBlock = singleton . HtmlBlock <$>
  ((pInBalancedTags Nothing <|> pHtmlComment))

parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t =
  case parseOnly (msum <$> many (pInline refmap) <* endOfInput) t of
       Left e   -> singleton $ Err (T.strip t) (T.pack $ show e)
       Right r  -> r

pInline :: ReferenceMap -> Parser Inlines
pInline refmap =
           pSpace
       <|> pStr
       <|> pEnclosure '*' refmap
       <|> pEnclosure '_' refmap
       <|> pLink refmap
       <|> pImage refmap
       <|> pCode
       <|> pEntity
       <|> pRawHtml
       <|> pInPointyBrackets
       <|> pSym

pSpace :: Parser Inlines
pSpace = singleton <$> (pSpaceSpace <|> pSpaceNewline)
  where pSpaceSpace = scanSpace >>
            (pSpaceNewline <|> pSpaceLB <|> return Space)
        pSpaceLB = scanSpace >> scanSpaces >>
                      ((pSpaceNewline >> return LineBreak) <|> return Space)
        pSpaceNewline = endOfLine >> scanSpaces >> return SoftBreak

pStr :: Parser Inlines
pStr = do
  let strChunk = takeWhile1 isAlphaNum
  let underscore = string "_"
  s <- T.intercalate "_" <$> strChunk `sepBy1` underscore
  if s `elem` uriProtocols
     then try (pUri s) <|> return (singleton $ Str s)
     else return (singleton $ Str s)

pSym :: Parser Inlines
pSym = singleton . Str . T.singleton <$> (pEscapedChar <|> pNonspaceChar)

uriProtocols :: [Text]
uriProtocols =
  [ "http", "https", "ftp", "file", "mailto", "news", "telnet" ]

pUri :: Text -> Parser Inlines
pUri protocol = do
  char ':'
  -- Scan non-ascii characters and ascii characters allowed in a URI.
  -- We allow punctuation except when followed by a space, since
  -- we don't want the trailing '.' in 'http://google.com.'
  let isUriChar c = not (isPunctuation c) &&
                       (not (isAscii c) || isAllowedInURI c)
  -- We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org)
  -- So we include balanced parens in the URL.
  let inParens = try $ do char '('
                          res <- takeWhile isUriChar
                          char ')'
                          return $ "(" <> res <> ")"
  let innerPunct = T.singleton <$> try (char '/'
        <|> (pSatisfy isPunctuation <* nfb space <* nfb endOfInput))
  let uriChunk = takeWhile1 isUriChar <|> inParens <|> innerPunct
  rest <- T.concat <$> many1 uriChunk
  -- now see if they amount to an absolute URI
  let rawuri = protocol <> ":" <> rest
  case parseURI (T.unpack $ escapeUri rawuri) of
       Just uri' -> return $ singleton $ Link (singleton $ Str rawuri)
                                  (T.pack $ show uri') (T.empty)
       Nothing   -> fail "not a URI"

escapeUri :: Text -> Text
escapeUri = T.pack . escapeURIString (not . isSpace) . T.unpack

isEnclosureChar :: Char -> Bool
isEnclosureChar '*' = True
isEnclosureChar '_' = True
isEnclosureChar _   = False

pEnclosure :: Char -> ReferenceMap -> Parser Inlines
pEnclosure c refmap = do
  cs <- takeWhile1 (== c)
  (Str cs <|) <$> pSpace
   <|> case T.length cs of
            3  -> pThree c refmap
            2  -> pTwo c refmap empty
            1  -> pOne c refmap empty
            _  -> return (singleton $ Str cs)

-- singleton sequence or empty if contents are empty
single :: (Inlines -> Inline) -> Inlines -> Inlines
single constructor ils = if Seq.null ils
                            then empty
                            else singleton (constructor ils)

-- parse inlines til you hit a c, and emit Emph.
-- if you never hit a c, emit '*' + inlines parsed.
pOne :: Char -> ReferenceMap -> Inlines -> Parser Inlines
pOne c refmap prefix = do
  contents <- msum <$> many ( (nfb (char c) >> pInline refmap)
                             <|> (try $ string (T.pack [c,c]) >>
                                  nfb (char c) >> pTwo c refmap prefix) )
  (char c >> return (single Emph $ prefix <> contents))
    <|> return (singleton (Str (T.singleton c)) <> (prefix <> contents))

-- parse inlines til you hit two c's, and emit Strong.
-- if you never do hit two c's, emit '**' plus + inlines parsed.
pTwo :: Char -> ReferenceMap -> Inlines -> Parser Inlines
pTwo c refmap prefix = do
  let ender = string $ T.pack [c,c]
  contents <- msum <$> many (nfb ender >> pInline refmap)
  (ender >> return (single Strong $ prefix <> contents))
    <|> return (singleton (Str $ T.pack [c,c]) <> (prefix <> contents))

-- parse inlines til you hit one c or a sequence of two c's.
-- If one c, emit Emph and then parse pTwo.
-- if two c's, emit Strong and then parse pOne.
pThree :: Char -> ReferenceMap -> Parser Inlines
pThree c refmap = do
  contents <- msum <$> (many (nfb (char c) >> pInline refmap))
  (string (T.pack [c,c]) >> (pOne c refmap (single Strong contents)))
   <|> (char c >> (pTwo c refmap (single Emph contents)))
   <|> return (singleton (Str $ T.pack [c,c,c]) <> contents)

pCode :: Parser Inlines
pCode = fst <$> pCode'

pCode' :: Parser (Inlines, Text)
pCode' = try $ do
  ticks <- takeWhile1 (== '`')
  let end = try $ string ticks >> nfb (char '`')
  let nonBacktickSpan = takeWhile1 (/= '`')
  let backtickSpan = takeWhile1 (== '`')
  contents <- T.concat <$> manyTill (nonBacktickSpan <|> backtickSpan) end
  return (singleton . Code . T.strip $ contents, ticks <> contents <> ticks)

pLink :: ReferenceMap -> Parser Inlines
pLink refmap = do
  lab <- pLinkLabel
  let lab' = parseInlines refmap lab
  pInlineLink lab' <|> pReferenceLink refmap lab lab'
    <|> return (singleton (Str "[") <> lab' <> singleton (Str "]"))

pInlineLink :: Inlines -> Parser Inlines
pInlineLink lab = try $ do
  char '('
  scanSpaces
  url <- pLinkUrl
  tit <- option "" $ try $ scanSpnl *> pLinkTitle <* scanSpaces
  char ')'
  return $ singleton $ Link lab url tit

pReferenceLink :: ReferenceMap -> Text -> Inlines -> Parser Inlines
pReferenceLink refmap rawlab lab = try $ do
  ref <- option rawlab $ try $ scanSpaces >> pLinkLabel
  let ref' = if T.null ref then rawlab else ref
  case lookupLinkReference refmap ref' of
       Just (url,tit)  -> return $ singleton $ Link lab url tit
       Nothing         -> fail "Reference not found"

pImage :: ReferenceMap -> Parser Inlines
pImage refmap = try $ do
  char '!'
  let linkToImage (Link lab url tit) = Image lab url tit
      linkToImage x                  = x
  fmap linkToImage <$> pLink refmap

pEntity :: Parser Inlines
pEntity = try $ do
  char '&'
  res <- pCharEntity <|> pDecEntity <|> pHexEntity
  char ';'
  return $ singleton $ Entity $ "&" <> res <> ";"

pCharEntity :: Parser Text
pCharEntity = takeWhile1 (\c -> isAscii c && isLetter c)

pDecEntity :: Parser Text
pDecEntity = try $ do
  char '#'
  res <- takeWhile1 isDigit
  return $ "#" <> res

pHexEntity :: Parser Text
pHexEntity = try $ do
  char '#'
  x <- char 'X' <|> char 'x'
  res <- takeWhile1 isHexDigit
  return $ "#" <> T.singleton x <> res

pRawHtml :: Parser Inlines
pRawHtml = singleton . RawHtml <$> (snd <$> pHtmlTag <|> pHtmlComment)

pInPointyBrackets :: Parser Inlines
pInPointyBrackets = try $ do
  char '<'
  t <- takeWhile1 (/='>')
  char '>'
  case t of
       _ | startsWithProtocol t -> return $ autoLink t
         | T.any (=='@') t && T.all (/=' ') t -> return $ emailLink t
         | otherwise   -> fail "Unknown contents of <>"

scanMatches :: Scanner -> Text -> Bool
scanMatches scanner t =
  case parseOnly scanner t of
       Right ()   -> True
       _          -> False

startsWithProtocol :: Text -> Bool
startsWithProtocol =
  scanMatches $ choice (map string uriProtocols) >> skip (== ':')

autoLink :: Text -> Inlines
autoLink t = singleton $ Link (singleton $ Str t) (escapeUri t) (T.empty)

emailLink :: Text -> Inlines
emailLink t = singleton $ Link (singleton $ Str t)
                               (escapeUri $ "mailto:" <> t) (T.empty)

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
renderBlocks = mconcat . intersperse blocksep . map renderBlock . toList
  where renderBlock :: Block -> Html
        renderBlock (Header n ils)
          | n >= 1 && n <= 5 = ([H.h1,H.h2,H.h3,H.h4,H.h5] !! (n - 1))
                                  $ renderInlines ils
          | otherwise        = H.p (renderInlines ils)
        renderBlock (Para ils) = H.p (renderInlines ils)
        renderBlock (HRule) = H.hr
        renderBlock (Blockquote bs) = H.blockquote $ nl <> renderBlocks bs <> nl
        renderBlock (CodeBlock attr t) =
          case codeLang attr of
                Nothing   -> base
                Just lang -> base ! A.class_ (toValue lang)
          where base = H.pre $ H.code $ toHtml t
        renderBlock (List tight (Bullet _) items) =
          H.ul $ nl <> mapM_ (li tight) items
        renderBlock (List tight (Numbered _ n) items) =
          if n == 1 then base else base ! A.start (toValue n)
          where base = H.ol $ nl <> mapM_ (li tight) items
        renderBlock (HtmlBlock raw) = H.preEscapedToMarkup raw
        li :: Bool -> Blocks -> Html
        li True bs = case toList bs of
                          [Para zs]         -> H.li (renderInlines zs) <> nl
                          [Para zs, List{}] -> H.li (renderInlines zs <>
                             nl <> renderBlocks (Seq.drop 1 bs)) <> nl
                          _                 -> toLi bs
        li False bs = toLi bs
        toLi x = (H.li $ renderBlocks x) <> nl
        nl = "\n"
        blocksep = "\n"

renderInlines :: Inlines -> Html
renderInlines = foldMap renderInline
  where renderInline :: Inline -> Html
        renderInline (Str t) = toHtml t
        renderInline Space   = " "
        renderInline SoftBreak = "\n" -- or space optionally
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


