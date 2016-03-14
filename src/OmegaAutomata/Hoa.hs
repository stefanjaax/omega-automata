{-# LANGUAGE OverloadedStrings #-}

-- | Definition of parser for the Hanoi omega automata format
module OmegaAutomata.Hoa where
--import OmegaAutomata.Automata
import Prelude hiding (takeWhile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile1, inClass)
import Data.Attoparsec.Expr
import Control.Applicative
import Control.Monad (guard)
import Control.Monad.State as SM

type AliasName = ByteString


class MBoolExpr a where
  _true :: a
  _false :: a
  _and :: a -> a -> a
  _or :: a -> a -> a


class (MBoolExpr a) => BoolExpr a where
  _not :: a -> a


data LabelExpr = LBoolExpr Bool
               | RefAP Int
               | RefAlias AliasName
               | LNot LabelExpr
               | LAnd LabelExpr LabelExpr
               | LOr LabelExpr LabelExpr
               deriving Show

data HoaAccCond = FinCond Int
                | InfCond Int
                | CompFinCond Int
                | CompInfCond Int
                | AccAnd HoaAccCond HoaAccCond
                | AccOr HoaAccCond HoaAccCond
                | AccBoolExpr Bool
                deriving Show

data MinMax = Min | Max deriving Show

data EvenOdd = Even | Odd deriving Show

data AccName = Buchi
             | GBuchi Int
             | CoBuchi
             | GCoBuchi Int
             | Streett Int
             | Rabin Int
             | GRabin Int [Int]
             | Parity MinMax EvenOdd Int
             | All
             | None
             deriving Show

data HeaderItem = NumStates Int
                | AP [ByteString]
                | Alias (AliasName, LabelExpr)
                | Acceptance (Int, HoaAccCond)
                | Start [Int]
                | Tool [ByteString]
                | Name ByteString
                | Properties [ByteString]
                | AcceptanceName AccName
                deriving Show

data EdgeItem = EdgeItem
                { edgeLabel :: Maybe LabelExpr
                , stateConj :: [Int]
                , accSig :: Maybe [Int]
                }
                deriving Show

data BodyItem = BodyItem
                { stateLabel :: Maybe LabelExpr
                , num :: Int
                , descr :: Maybe ByteString
                , stateAccSig :: Maybe [Int]
                , edges :: [EdgeItem]
                }
                deriving Show


instance MBoolExpr LabelExpr where
   _true = LBoolExpr True
   _false = LBoolExpr False
   _and = LAnd
   _or = LOr

instance BoolExpr LabelExpr where
  _not = LNot

instance MBoolExpr HoaAccCond where
  _and = AccAnd
  _or = AccOr
  _true = AccBoolExpr True
  _false = AccBoolExpr False


parseHoa :: Parser ([HeaderItem], [BodyItem])
parseHoa= do
  parseAttribute "HOA"
  string "v1"
  (hs, (i,_)) <- runStateT (many parseHeaderItem) (0, [])
  skipNonToken $ string "--BODY--"
  bs <- many $ parseBodyItem i
  skipNonToken $ string "--END--"
  return (hs, bs)


parseBodyItem :: Int -> Parser BodyItem
parseBodyItem i = do
  skipNonToken $ parseAttribute "State"
  l <- skipNonToken $ option Nothing $ Just <$> brackets (parseLabelExpr i [])
  n <- skipNonToken $ decimal
  d <- skipNonToken $ option Nothing $ Just <$> parseDoubleQuotedString
  a <- skipNonToken $ option Nothing $ Just <$> parseAccSig
  es <- many $ parseEdgeItem i
  return BodyItem
         { stateLabel = l
         , num = n
         , descr = d
         , stateAccSig = a
         , edges = es
         }


parseEdgeItem :: Int -> Parser EdgeItem
parseEdgeItem i = do
  l <- skipNonToken $ option Nothing $ Just <$> brackets (parseLabelExpr i [])
  s <- skipNonToken $ parseSpaceSeparated decimal
  a <- skipNonToken $ option Nothing $ Just <$> parseAccSig
  return EdgeItem
         { edgeLabel = l
         , stateConj = s
         , accSig = a
         }


parseAccSig :: Parser [Int]
parseAccSig = curls $ parseSpaceSeparated decimal


parseAttribute :: ByteString -> Parser ()
parseAttribute a = do
  skipNonToken $ string a
  skipNonToken $ string ":"
  skipNonToken $ return ()


parseProperties :: Parser HeaderItem
parseProperties = do
  parseAttribute "properties"
  skipNonToken $ return ()
  Properties <$> (parseSpaceSeparated parseIdentifier)


parseHeaderItem :: SM.StateT (Int, [AliasName]) Parser HeaderItem
parseHeaderItem = do
  (i, as) <- get
  r <- choice $ lift <$> [parseAccName,
                          parseAP,
                          parseStart,
                          parseAccName,
                          parseNumStates,
                          parseHoaAccCond,
                          parseProperties,
                          parseTool,
                          parseName,
                          parseAlias i as]
  case r of
    (Alias (s, _)) -> put (i, s:as) >> return r
    (AP ap) -> put (length ap, as) >> return r
    _ -> return r


parseNumStates :: Parser HeaderItem
parseNumStates = do
  parseAttribute "States"
  num_states <- decimal
  return (NumStates num_states)


parseName :: Parser HeaderItem
parseName = do
  parseAttribute "name"
  Name <$> parseDoubleQuotedString


parseStart :: Parser HeaderItem
parseStart = do
  parseAttribute "Start"
  Start <$> parseSpaceSeparated decimal


parseTool :: Parser HeaderItem
parseTool = do
  parseAttribute "tool"
  Tool <$> (parseSpaceSeparated parseDoubleQuotedString)


parseAliasName :: Parser AliasName
parseAliasName = do
  char '@'
  parseIdentifier


parseAlias :: Int -> [AliasName] -> Parser HeaderItem
parseAlias i as = do
  parseAttribute "Alias"
  a <- parseAliasName
  guard (not (a `elem` as)) <?> "Duplicate definition of aliases."
  skipSpace
  expr <- parseLabelExpr i as
  return $ Alias (a, expr)


parseRefAlias :: [AliasName] -> Parser AliasName
parseRefAlias as = do
  a <- parseAliasName
  guard (a `elem` as) <?> "Reference to undefined alias name."
  return a


parseAccName :: Parser HeaderItem
parseAccName = do
  parseAttribute "acc-name"
  (AcceptanceName <$>
   ((string "Buchi" >> return Buchi) <|>
   (string "co-Buchi" >> return CoBuchi) <|>
   (string "all" >> return All) <|>
   (string "none" >> return None) <|>
   (GBuchi <$> (string "generalized-Buchi" >> skipNonToken decimal)) <|>
   (GCoBuchi <$> (string "generalized-co-Buchi" >> skipNonToken decimal)) <|>
   (Streett <$> (string "Streett" >> skipNonToken decimal)) <|>
   (Rabin <$> (string "Rabin" >> skipNonToken decimal)) <|>
   parseParityName <|>
   parseGRabinName))


parseParityName :: Parser AccName
parseParityName = do
  string "parity"
  skipSpace
  min_max <- (string "min" >> return Min) <|>
             (string "max" >> return Max)
  skipSpace
  even_odd <- (string "even" >> return Even) <|>
              (string "odd" >> return Odd)
  skipSpace
  num <- decimal
  return (Parity min_max even_odd num)


parseGRabinName :: Parser AccName
parseGRabinName = do
  string "generalized-Rabin"
  skipSpace
  num <- decimal
  nums <- count num decimal
  return $ GRabin num nums


parseLabelExpr :: Int -> [AliasName] -> Parser LabelExpr
parseLabelExpr i as = parseMBoolExpr p boolOps where
  p = RefAP <$> parseIntInRange i <|>
      RefAlias <$> parseRefAlias as


parseHoaAccCond :: Parser HeaderItem
parseHoaAccCond = do
  parseAttribute "Acceptance"
  i <- skipNonToken decimal
  acc <- parseAccCond i
  return (Acceptance (i, acc)) where
    parseAccCond i = parseMBoolExpr (p i) monotonicBoolOps
    p i = parseFin i <|>
          parseInf i <|>
          parseCompFin i<|>
          parseCompInf i
    parseAcc str p = skipNonToken (string str) >> parens p
    parseFin i = parseAcc "Fin" (FinCond <$> parseIntInRange i)
    parseInf i = parseAcc "Inf" (InfCond <$> parseIntInRange i)
    parseCompFin i = parseAcc "Fin" (string "!" >> (FinCond <$> parseIntInRange i))
    parseCompInf i = parseAcc "Inf" (string "!" >> (InfCond <$> parseIntInRange i))


parseAP :: Parser HeaderItem
parseAP = do
  parseAttribute "AP"
  num_aps <- decimal
  aps <- count num_aps (skipSpace >> parseDoubleQuotedString)
  return (AP aps)


parseIdentifier :: Parser ByteString
parseIdentifier = takeWhile1 (inClass "0-9a-zA-Z_-")


parseSpaceSeparated :: Parser a -> Parser [a]
parseSpaceSeparated p = p `sepBy1` many space


monotonicBoolOps :: MBoolExpr a => [[Operator ByteString a]]
monotonicBoolOps = [[Infix (skipNonToken (string "|") >> return _or) AssocLeft]
                   ,[Infix (skipNonToken (string "&") >> return _and) AssocLeft]
                   ]


boolOps :: BoolExpr a => [[Operator ByteString a]]
boolOps = [[Prefix (skipNonToken (string "!") >> return _not)]] ++ monotonicBoolOps


parseMBoolExpr :: (MBoolExpr a) => Parser a -> [[Operator ByteString a]] -> Parser a
parseMBoolExpr p ops = buildExpressionParser ops term where
  term = (skipNonToken (string "t") >> return _true) <|>
         (skipNonToken (string "f") >> return _false) <|>
         parens (parseMBoolExpr p ops) <|>
         p


embracedBy :: Parser a -> ByteString -> ByteString -> Parser a
embracedBy p s1 s2 = do
  skipNonToken $ string s1
  r <- p
  skipNonToken $ string s2
  return r


parens :: Parser a -> Parser a
parens p = embracedBy p "(" ")"


brackets :: Parser a -> Parser a
brackets p = embracedBy p "[" "]"


curls :: Parser a -> Parser a
curls p = embracedBy p "{" "}"


skipNonToken :: Parser a -> Parser a
skipNonToken p =  do
  skipSpace
  many $ do
    string "/*" *> manyTill anyChar (string "*/")
    skipSpace
  p


parseDoubleQuotedString :: Parser ByteString
parseDoubleQuotedString = do
  char '"'
  x <- many (notChar '\"' <|> (char '\\' >> char '\"'))
  char '"'
  return $ pack x


parseIntInRange :: Int -> Parser Int
parseIntInRange i = do
  x <- decimal
  guard (x >= 0 && x < i) <?> "Reference out of range."
  return x
