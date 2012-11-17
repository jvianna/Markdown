{-# LANGUAGE OverloadedStrings #-}

{-

TODO

* use attoparsec's 'scanChar' to make an efficient escape-aware takeWhile1.

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
import Data.Char (isAscii, isSpace, isPunctuation, isSymbol,
                    isDigit, isHexDigit, isAlphaNum, isLetter)
import Network.URI (parseURI, isAllowedInURI, escapeURIString)
import Data.Monoid ((<>))
import Data.Foldable (foldMap, toList)
import Control.Applicative hiding (optional,empty)
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

type Scanner = A.Parser ()

-- Try to match the scanner, returning Just the remaining text if
-- it matches, Nothing otherwise.
applyScanners :: [Scanner] -> Text -> Maybe Text
applyScanners scanners t =
  case A.parseOnly (sequence_ scanners >> A.takeText) t of
       Right t'   -> Just t'
       Left _err  -> Nothing

-- Scanners

scanBlockquoteStart :: Scanner
scanBlockquoteStart =
  scanNonindentSpaces >> scanChar '>' >> opt (scanChar ' ')

scanIndentSpace :: Scanner
scanIndentSpace =
  scanChar '\t' <|>
    A.try (scanChar ' ' >>
      (scanChar '\t' <|>
        A.try (scanChar ' ' >>
          (scanChar '\t' <|>
            A.try (scanChar ' ' >>
              (scanChar '\t' <|>
                scanChar ' '))))))

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
scanBlankline = A.skipWhile A.isHorizontalSpace  *> A.endOfInput

scanSpace :: Scanner
scanSpace = () <$ A.satisfy A.isHorizontalSpace

-- 0 or more spaces
scanSpaces :: Scanner
scanSpaces = A.skipWhile A.isHorizontalSpace

scanSpnl :: Scanner
scanSpnl = scanSpaces *> opt (A.endOfLine *> scanSpaces)

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
  A.skipWhile (\x -> A.isHorizontalSpace x || x == c)
  A.endOfInput
  return ()

isCodeFenceChar :: Char -> Bool
isCodeFenceChar '`' = True
isCodeFenceChar '~' = True
isCodeFenceChar _   = False

scanCodeFenceLine :: Scanner
scanCodeFenceLine = () <$ parseCodeFenceLine

parseCodeFenceLine :: A.Parser (Text, Text)
parseCodeFenceLine = A.try $ do
  c <- A.satisfy isCodeFenceChar
  A.count 2 (A.char c)
  extra <- A.takeWhile (== c)
  scanSpaces
  rawattr <- A.takeWhile (/='`')
  A.endOfInput
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

scanReference :: Scanner
scanReference = mzero -- TODO

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
                    , ((scanIndentSpace >> nfb scanBlankline),
                        parseIndentedCodeBlock)
                    , (scanAtxHeaderStart, parseAtxHeader)
                    , (scanCodeFenceLine, parseCodeFence)
                    , (scanReference, parseReference)
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

parseIndentedCodeBlock :: BlockParser Blocks
parseIndentedCodeBlock =
  withLineScanner (scanIndentSpace <|> scanBlankline) $
  singleton . CodeBlock CodeAttr{ codeLang = Nothing } .  T.unlines
     . reverse . dropWhile T.null . reverse <$> getLines
 where getLines = nextLine Consume LineScan >>=
                    maybe (return []) (\ln -> (ln:) <$> getLines)

parseAtxHeader :: BlockParser Blocks
parseAtxHeader = do
  next <- maybe "" id <$> nextLine Consume BlockScan
  case A.parseOnly parseAtxHeaderStart next of
        Left _  -> return $ singleton $ Para $ singleton $ Str next
        Right lev -> return
                     $ singleton . Header lev . singleton . Markdown
                     $ stripClosingHashes next
   where stripClosingHashes = T.reverse . stripLeadingHashes . T.reverse
         stripLeadingHashes ln = case T.uncons ln of
                                      Just ('#',rest)
                                        | "\\" `T.isPrefixOf` rest -> ln
                                           -- escaped \#
                                        | otherwise -> stripLeadingHashes rest
                                      _ -> ln

parseCodeFence :: BlockParser Blocks
parseCodeFence = do
  next <- maybe "" id <$> nextLine Consume BlockScan
  case A.parseOnly parseCodeFenceLine next of
       Left _  -> return $ singleton $ Para $ singleton $ Str next
       Right (fence, rawattr) ->
         singleton . CodeBlock (parseCodeAttributes rawattr)
          . T.unlines . reverse <$> getLines fence
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

parseReference :: BlockParser Blocks
parseReference = do
  return empty -- TODO
{-
  pNonindentSpaces
  lab <- pLinkLabel
  char ':'
  pSpnl
  url <- pLinkUrl
  tit <- option T.empty $ try $ pSpnl >> pLinkTitle
  pBlankline
  addLinkReference lab (url,tit)
  return empty
-}


parseLines :: BlockParser Blocks
parseLines = do
  next <- nextLine Consume BlockScan
  processLines <$> maybe (return []) (\x ->
                          (x:) <$> (withLineScanner paraLine getLines)) next
 where getLines = nextLine Consume LineScan >>=
                    maybe (return []) (\x -> (x:) <$> getLines)
       paraLine =   nfb scanBlankline
                 >> nfb scanIndentSpace
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
joinLines = T.intercalate "\n"

pEscapedChar :: A.Parser Char
pEscapedChar = A.try $ A.char '\\' *> A.satisfy isEscapable

isEscapable :: Char -> Bool
isEscapable c = isSymbol c || isPunctuation c

-- parses a character satisfying the predicate, but understands escaped
-- symbols
pSatisfy :: (Char -> Bool) -> A.Parser Char
pSatisfy p =
  A.satisfy (\c -> c /= '\\' && p c)
   <|> A.try (A.char '\\' *> A.satisfy (\c -> isEscapable c && p c))

pAnyChar :: A.Parser Char
pAnyChar = pSatisfy (const True)

pNonspaceChar :: A.Parser Char
pNonspaceChar = pSatisfy isNonspaceChar
  where isNonspaceChar ' '  = False
        isNonspaceChar '\n' = False
        isNonspaceChar '\t' = False
        isNonspaceChar '\r' = False
        isNonspaceChar _    = True

data HtmlTagType = Opening Text | Closing Text | SelfClosing Text deriving Show

-- returns name of tag needed to close, and whole tag
pHtmlTag :: A.Parser (HtmlTagType, Text)
pHtmlTag = A.try $ do
  A.char '<'
  -- do not end the tag with a > character in a quoted attribute.
  closing <- (A.char '/' >> return True) <|> return False
  tagname <- T.toLower <$>
                A.takeWhile1 (\c -> isAlphaNum c || c == '?' || c == '!')
  let attr = do ss <- A.takeWhile isSpace
                x <- A.letter
                xs <- A.takeWhile (\c -> isAlphaNum c || c == ':')
                A.skip (=='=')
                v <- pQuoted '"' <|> pQuoted '\'' <|> A.takeWhile1 isAlphaNum
                      <|> return ""
                return $ ss <> T.singleton x <> xs <> "=" <> v
  attrs <- T.concat <$> many (A.try attr)
  final <- A.takeWhile (\c -> isSpace c || c == '/')
  A.char '>'
  let tagtype = if closing
                   then Closing tagname
                   else case T.stripSuffix "/" final of
                         Just _  -> SelfClosing tagname
                         Nothing -> Opening tagname
  return (tagtype,
          T.pack ('<' : ['/' | closing]) <> tagname <> attrs <> final <> ">")

pQuoted :: Char -> A.Parser Text
pQuoted c = A.try $ do
  A.char c
  contents <- A.takeTill (== c)
  A.char c
  return (T.singleton c <> contents <> T.singleton c)

pLinkLabel :: A.Parser Text
pLinkLabel = A.try $ A.char '[' *> (T.concat <$>
  (A.manyTill (regChunk <|> bracketed <|> codeChunk) (A.char ']')))
  where regChunk = T.pack <$> A.many1 (pSatisfy (A.notInClass "`[]"))
        codeChunk = snd <$> pCode'
        bracketed = inBrackets <$> pLinkLabel
        inBrackets t = "[" <> t <> "]"

pLinkUrl :: A.Parser Text
pLinkUrl = A.try $ do
  inPointy <- (A.char '<' >> return True) <|> return False
  if inPointy
     then A.takeWhile (A.notInClass "\r\n>") <* A.char '>'
     else T.concat <$> many (regChunk <|> parenChunk)
    where regChunk = A.takeWhile1 (A.notInClass " \t\r\n()")
          parenChunk = inParens . T.concat <$> (A.char '(' *>
                         A.manyTill (regChunk <|> parenChunk) (A.char ')'))
          inParens x = "(" <> x <> ")"

pLinkTitle :: A.Parser Text
pLinkTitle = T.pack <$> (pLinkTitleDQ <|> pLinkTitleSQ <|> pLinkTitleP)
  where pLinkTitleDQ = A.try $ A.char '"' *> A.manyTill pAnyChar (A.char '"')
        pLinkTitleSQ = A.try $ A.char '\'' *> A.manyTill pAnyChar (A.char '\'')
        pLinkTitleP  = A.try $ A.char '(' *> A.manyTill pAnyChar (A.char ')')


{-
-- Block-level parsers.

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

-}

parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t =
  case A.parseOnly (msum <$> many (pInline refmap) <* A.endOfInput) t of
       Left e   -> singleton $ Err (T.strip t) (T.pack $ show e)
       Right r  -> r

pInline :: ReferenceMap -> A.Parser Inlines
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

pSpace :: A.Parser Inlines
pSpace = singleton <$> (pSpaceSpace <|> pSpaceNewline)
  where pSpaceSpace = scanSpace >>
            (pSpaceNewline <|> pSpaceLB <|> return Space)
        pSpaceLB = scanSpace >> scanSpaces >>
                      ((pSpaceNewline >> return LineBreak) <|> return Space)
        pSpaceNewline = A.endOfLine >> scanSpaces >> return SoftBreak

pStr :: A.Parser Inlines
pStr = do
  let strChunk = A.takeWhile1 isAlphaNum
  let underscore = A.string "_"
  s <- T.intercalate "_" <$> strChunk `A.sepBy1` underscore
  if s `elem` uriProtocols
     then A.try (pUri s) <|> return (singleton $ Str s)
     else return (singleton $ Str s)

pSym :: A.Parser Inlines
pSym = singleton . Str . T.singleton <$> (pEscapedChar <|> pNonspaceChar)

uriProtocols :: [Text]
uriProtocols =
  [ "http", "https", "ftp", "file", "mailto", "news", "telnet" ]

pUri :: Text -> A.Parser Inlines
pUri protocol = do
  A.char ':'
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
  let inParens = A.try $ do A.char '('
                            res <- A.takeWhile isUriChar
                            A.char ')'
                            return $ "(" <> res <> ")"
  let innerPunct = T.singleton <$> A.try (A.char '/'
        <|> (pSatisfy isPunctuation <* nfb A.space <* nfb A.endOfInput))
  let uriChunk = A.takeWhile1 isUriChar <|> inParens <|> innerPunct
  rest <- T.concat <$> A.many1 uriChunk
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

pEnclosure :: Char -> ReferenceMap -> A.Parser Inlines
pEnclosure c refmap = do
  cs <- A.takeWhile1 (== c)
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
pOne :: Char -> ReferenceMap -> Inlines -> A.Parser Inlines
pOne c refmap prefix = do
  contents <- msum <$> many ( (nfb (A.char c) >> pInline refmap)
                             <|> (A.try $ A.string (T.pack [c,c]) >>
                                  nfb (A.char c) >> pTwo c refmap prefix) )
  (A.char c >> return (single Emph $ prefix <> contents))
    <|> return (singleton (Str (T.singleton c)) <> (prefix <> contents))

-- parse inlines til you hit two c's, and emit Strong.
-- if you never do hit two c's, emit '**' plus + inlines parsed.
pTwo :: Char -> ReferenceMap -> Inlines -> A.Parser Inlines
pTwo c refmap prefix = do
  let ender = A.string $ T.pack [c,c]
  contents <- msum <$> many (nfb ender >> pInline refmap)
  (ender >> return (single Strong $ prefix <> contents))
    <|> return (singleton (Str $ T.pack [c,c]) <> (prefix <> contents))

-- parse inlines til you hit one c or a sequence of two c's.
-- If one c, emit Emph and then parse pTwo.
-- if two c's, emit Strong and then parse pOne.
pThree :: Char -> ReferenceMap -> A.Parser Inlines
pThree c refmap = do
  contents <- msum <$> (many (nfb (A.char c) >> pInline refmap))
  (A.string (T.pack [c,c]) >> (pOne c refmap (single Strong contents)))
   <|> (A.char c >> (pTwo c refmap (single Emph contents)))
   <|> return (singleton (Str $ T.pack [c,c,c]) <> contents)

pCode :: A.Parser Inlines
pCode = fst <$> pCode'

pCode' :: A.Parser (Inlines, Text)
pCode' = A.try $ do
  ticks <- A.takeWhile1 (== '`')
  let end = A.try $ A.string ticks >> nfb (A.char '`')
  let nonBacktickSpan = A.takeWhile1 (/= '`')
  let backtickSpan = A.takeWhile1 (== '`')
  contents <- T.concat <$> A.manyTill (nonBacktickSpan <|> backtickSpan) end
  return (singleton . Code . T.strip $ contents, ticks <> contents <> ticks)

pLink :: ReferenceMap -> A.Parser Inlines
pLink refmap = do
  lab <- pLinkLabel
  let lab' = parseInlines refmap lab
  pInlineLink lab' <|> pReferenceLink refmap lab lab'
    <|> return (singleton (Str "[") <> lab' <> singleton (Str "]"))

pInlineLink :: Inlines -> A.Parser Inlines
pInlineLink lab = A.try $ do
  A.char '('
  scanSpaces
  url <- pLinkUrl
  tit <- A.option "" $ A.try $ scanSpnl *> pLinkTitle <* scanSpaces
  A.char ')'
  return $ singleton $ Link lab url tit

pReferenceLink :: ReferenceMap -> Text -> Inlines -> A.Parser Inlines
pReferenceLink refmap rawlab lab = mzero -- TODO
{-

pReferenceLink :: Text -> Inlines -> P Inlines
pReferenceLink rawlab lab = try $ do

  ref <- option rawlab $ try $ pSpnl >> pLinkLabel
  let ref' = if T.null ref then rawlab else ref
  lookupResult <- lookupLinkReference ref'
  case lookupResult of
       Just (url,tit)  -> return $ singleton $ Link lab url tit
       Nothing         -> fail "Reference not found"

-}

pImage :: ReferenceMap -> A.Parser Inlines
pImage refmap = A.try $ do
  A.char '!'
  let linkToImage (Link lab url tit) = Image lab url tit
      linkToImage x                  = x
  fmap linkToImage <$> pLink refmap

pEntity :: A.Parser Inlines
pEntity = A.try $ do
  A.char '&'
  res <- pCharEntity <|> pDecEntity <|> pHexEntity
  A.char ';'
  return $ singleton $ Entity $ "&" <> res <> ";"

pCharEntity :: A.Parser Text
pCharEntity = A.takeWhile1 (\c -> isAscii c && isLetter c)

pDecEntity :: A.Parser Text
pDecEntity = A.try $ do
  A.char '#'
  res <- A.takeWhile1 isDigit
  return $ "#" <> res

pHexEntity :: A.Parser Text
pHexEntity = A.try $ do
  A.char '#'
  x <- A.char 'X' <|> A.char 'x'
  res <- A.takeWhile1 isHexDigit
  return $ "#" <> T.singleton x <> res

pRawHtml :: A.Parser Inlines
pRawHtml = singleton . RawHtml . snd <$> pHtmlTag

pInPointyBrackets :: A.Parser Inlines
pInPointyBrackets = A.try $ do
  A.char '<'
  t <- A.takeWhile1 (/='>')
  A.char '>'
  case t of
       _ | startsWithProtocol t -> return $ autoLink t
         | T.any (=='@') t && T.all (/=' ') t -> return $ emailLink t
         | otherwise   -> fail "Unknown contents of <>"

scanMatches :: Scanner -> Text -> Bool
scanMatches scanner t =
  case A.parseOnly scanner t of
       Right ()   -> True
       _          -> False

startsWithProtocol :: Text -> Bool
startsWithProtocol =
  scanMatches $ A.choice (map A.string uriProtocols) >> A.skip (== ':')

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


