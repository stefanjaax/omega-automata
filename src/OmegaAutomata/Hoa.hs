{-# LANGUAGE OverloadedStrings #-}

-- | Definition of parser for the Hanoi omega automata format
module Hoa where
import Automata
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


data LabelExpr = LBoolExpr Bool | RefAP Int | RefAlias AliasName | LNot LabelExpr |
                 LAnd LabelExpr LabelExpr | LOr LabelExpr LabelExpr
                 deriving Show

data HoaAccCond = FinCond Int | InfCond Int | CompFinCond Int | CompInfCond Int |
                  AccAnd HoaAccCond HoaAccCond | AccOr HoaAccCond HoaAccCond |
                  AccBoolExpr Bool
                  deriving Show

data MinMax = Min | Max deriving Show

data EvenOdd = Even | Odd deriving Show

data AccName = Buchi | GBuchi Int | CoBuchi | GCoBuchi Int | Streett Int |
               Rabin Int | GRabin Int [Int] | Parity MinMax EvenOdd Int | All | None
               deriving Show

data HeaderItem = NumStates Int | AP [ByteString] | Alias (AliasName, LabelExpr) |
                  Acceptance (Int, HoaAccCond) | Tool [ByteString] | Name ByteString |
                  Properties [ByteString] | AcceptanceName AccName
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


parseOmegaAutomaton :: Parser (OmegaAutomaton String ())
parseOmegaAutomaton = undefined


parseHoaHeader :: Parser [HeaderItem]
parseHoaHeader = do
  parseAttribute "HOA"
  string "v1"
  fst <$> runStateT (many parseHeaderItem) (0, [])


parseAttribute :: ByteString -> Parser ()
parseAttribute a = do
  skipSpace
  string a
  skipSpace
  string ":"
  skipSpace


parseProperties :: Parser HeaderItem
parseProperties = do
  parseAttribute "properties"
  skipSpace
  Properties <$> (parseSpaceSeparated parseIdentifier)


parseHeaderItem :: SM.StateT (Int, [AliasName]) Parser HeaderItem
parseHeaderItem = do
  (i, as) <- get
  r <- choice $ lift <$> [parseAccName,
                          parseAP,
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
   (GBuchi <$> (string "generalized-Buchi" >> decimal)) <|>
   (GCoBuchi <$> (string "generalized-co-Buchi" >> decimal)) <|>
   (Streett <$> (string "Streett" >> decimal)) <|>
   (Rabin <$> (string "Rabin" >> decimal)) <|>
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
  i <- decimal
  acc <- parseAccCond i
  return (Acceptance (i, acc)) where
    parseAccCond i = parseMBoolExpr (p i) monotonicBoolOps
    p i = parseFin i <|>
          parseInf i <|>
          parseCompFin i<|>
          parseCompInf i
    parseAcc str p = do
      string str
      skipSpace
      string "("
      skipSpace
      result <- p
      skipSpace
      string ")"
      return result
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


parseSpaceSeparated :: Parser ByteString -> Parser [ByteString]
parseSpaceSeparated p = p `sepBy1` many space


monotonicBoolOps :: MBoolExpr a => [[Operator ByteString a]]
monotonicBoolOps = [[Infix (withoutSpace (string "|") >> return _or) AssocLeft]
                   ,[Infix (withoutSpace (string "&") >> return _and) AssocLeft]
                   ]


boolOps :: BoolExpr a => [[Operator ByteString a]]
boolOps = [[Prefix (withoutSpace (string "!") >> return _not)]] ++ monotonicBoolOps


parseMBoolExpr :: (MBoolExpr a) => Parser a -> [[Operator ByteString a]] -> Parser a
parseMBoolExpr p ops = buildExpressionParser ops term where
  term = (withoutSpace (string "t") >> return _true) <|>
         (withoutSpace (string "f") >> return _false) <|>
         parens (parseMBoolExpr p ops) <|>
         p


parens :: Parser a -> Parser a
parens p = do
  skipSpace
  string "("
  a <- p
  skipSpace
  string ")"
  return a


withoutSpace :: Parser a -> Parser a
withoutSpace p = skipSpace >> p


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
