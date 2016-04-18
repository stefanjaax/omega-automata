{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the Hanoi omega automata format (https://github.com/adl/hoaf)
module OmegaAutomata.Hoa ( AliasName
                         , LabelExpr(..)
                         , HoaAccCond(..)
                         , MinMax(..)
                         , EvenOdd(..)
                         , AccName(..)
                         , HeaderItem(..)
                         , EdgeItem(..)
                         , BodyItem(..)
                         , parseHoa
                         , toHoa
                         , nbaToHoa
                         , hoaToNBA) where
import OmegaAutomata.Automata as A
import Prelude hiding (takeWhile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Attoparsec.ByteString hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile1, inClass)
import Data.Attoparsec.Expr
import Control.Applicative
import Control.Monad.State as SM
import Data.List (intersperse)
import qualified Data.Set as S

type AliasName = ByteString

data LabelExpr = LBoolExpr Bool
               | RefAP Int
               | RefAlias AliasName
               | LNot LabelExpr
               | LAnd LabelExpr LabelExpr
               | LOr LabelExpr LabelExpr
               deriving (Show, Eq, Ord)

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

class MBoolExpr a where
  _true :: a
  _false :: a
  _and :: a -> a -> a
  _or :: a -> a -> a

class (MBoolExpr a) => BoolExpr a where
  _not :: a -> a

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

-- | The actual parser
--   Returns a tuple of headeritems and bodyitems
parseHoa :: Parser ([HeaderItem], [BodyItem])
parseHoa = do
  parseAttribute "HOA"
  string "v1"
  (hs, (i, as)) <- runStateT (many parseHeaderItem) (0, [])
  skipNonToken $ string "--BODY--"
  bs <- many $ parseBodyItem i as
  skipNonToken $ string "--END--"
  return (hs, bs)


hoaToEdges :: [BodyItem] -> [(A.State, Maybe LabelExpr, A.State)]
hoaToEdges bs = [(q1, l, q2) | b <- bs
                             , e <- edges b
                             , let q1 = num b
                             , q2 <- stateConj e
                             , let l = edgeLabel e]


hoaToStates :: [BodyItem] -> [(A.State, Maybe LabelExpr)]
hoaToStates bs = [(q,l) | BodyItem l q _ _ _ <- bs]


hoaToStartStates :: [HeaderItem] -> [A.State]
hoaToStartStates hs = concat [qs | Start qs <- hs]


-- | Convert parsed Hoa to NBA.
hoaToNBA :: ([HeaderItem], [BodyItem])
         -> A.NBA A.State (Maybe LabelExpr) (Maybe LabelExpr)
hoaToNBA (hs, bs) = let qs = hoaToStates bs
                        ts = hoaToEdges bs
                        ss = hoaToStartStates hs
                        as = [q | BodyItem _ q _ (Just _) _ <- bs] in
                          makeNBA qs ts ss as


parseBodyItem :: Int -> [AliasName] -> Parser BodyItem
parseBodyItem i as = do
  skipNonToken $ parseAttribute "State"
  l <- skipNonToken $ option Nothing $ Just <$> brackets (parseLabelExpr i as)
  n <- skipNonToken $ decimal
  d <- skipNonToken $ option Nothing $ Just <$> parseDoubleQuotedString
  a <- skipNonToken $ option Nothing $ Just <$> parseAccSig
  es <- many $ parseEdgeItem i as
  return BodyItem
         { stateLabel = l
         , num = n
         , descr = d
         , stateAccSig = a
         , edges = es
         }


parseEdgeItem :: Int -> [AliasName] -> Parser EdgeItem
parseEdgeItem i as = do
  l <- skipNonToken $ option Nothing $ Just <$> brackets (parseLabelExpr i as)
  s <- skipNonToken $ parseConjunction decimal
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
    (AP p) -> put (length p, as) >> return r
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
  Start <$> decimal `sepBy1` skipNonToken "&"


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
  n <- decimal
  return (Parity min_max even_odd n)


parseGRabinName :: Parser AccName
parseGRabinName = do
  string "generalized-Rabin"
  skipSpace
  n <- decimal
  nums <- count n decimal
  return $ GRabin n nums


parseLabelExpr :: Int -> [AliasName] -> Parser LabelExpr
parseLabelExpr i as = parseMBoolExpr p boolOps where
  p = RefAP <$> skipNonToken (parseIntInRange i) <|>
      RefAlias <$> skipNonToken (parseRefAlias as)


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
    parseAcc str p' = skipNonToken (string str) >> parens p'
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
parseSpaceSeparated p = p `sepBy1` many (string " ")


parseConjunction :: Parser a -> Parser [a]
parseConjunction p = p `sepBy1` skipNonToken (string "&")


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

-- | Convert NBA into HOA format
nbaToHoa :: (Show q, Show l, Ord q) => NBA q (Maybe LabelExpr) l
                                    -> ([HeaderItem], [BodyItem])
nbaToHoa a = let
  hs = [ NumStates $ S.size (states a)
       , Acceptance (1, InfCond 0)
       , Start $ [(toNode a q) - 1 | q <- S.toList (start a)]
       , Tool ["ldba-tool"]
       , AcceptanceName Buchi
       ]
  bs = [BodyItem{ stateLabel = Nothing
                , num = (toNode a q) - 1
                , descr = Nothing
                , stateAccSig = (if isAcc then Just [0] else Nothing)
                , edges = [EdgeItem{ edgeLabel = l
                                   , stateConj = [(toNode a q') - 1]
                                   , accSig = Nothing
                                   } | (q', l) <- succs a q]
                }
         | q <- S.toList (states a), let isAcc = S.member q (accept a)]
  in (hs, bs)


-- | Pretty-print Hoa Format
toHoa :: ([HeaderItem], [BodyItem]) -> String
toHoa (hs, bs) = unlines $ ["HOA: v1"] ++
                            (headerItemToHoa <$> hs) ++
                            ["--BODY--"] ++
                            [concat (bodyItemToHoa <$> bs) ++ "--END--"]


headerItemToHoa :: HeaderItem -> String
headerItemToHoa (NumStates i) = "States: " ++ show i
headerItemToHoa (AP as) = "AP: " ++ show (length as) ++
                          " " ++ unwords ((inDoubleQuotes . unpack) <$> as)
headerItemToHoa (Alias (n,l)) = "Alias: @" ++ unpack n ++ " " ++ labelExprToHoa l
headerItemToHoa (Acceptance (i, a)) = "Acceptance: " ++ show i ++ " " ++ accCondToHoa a
headerItemToHoa (Start ss) = "Start: " ++ concat  (intersperse "&" (show <$> ss))
headerItemToHoa (Tool ts) = "tool: " ++ unwords (map inDoubleQuotes (unpack <$> ts))
headerItemToHoa (Name s) = "name: " ++ inDoubleQuotes (unpack s)
headerItemToHoa (Properties ps) = "properties: " ++ unwords (unpack <$> ps)
headerItemToHoa (AcceptanceName n) = "acc-name: " ++ accNameToHoa n


accNameToHoa :: AccName -> String
accNameToHoa a = case a of
  Buchi -> "Buchi"
  CoBuchi -> "coBuchi"
  All -> "all"
  None -> "none"
  (GBuchi i) -> "generalized-Buchi " ++ show i
  (GCoBuchi i) -> "generalized-co-Buchi " ++ show i
  (Streett i) -> "Streett " ++ show i
  (Rabin i) -> "Rabin " ++ show i
  (Parity as b i) -> "Parity " ++ minMaxToHoa as ++ evenOddToHoa b ++ " " ++ show i where
    minMaxToHoa Min = "min"
    minMaxToHoa Max = "max"
    evenOddToHoa Even = "even"
    evenOddToHoa Odd = "odd"
  (GRabin i is) -> "generalized-Rabin " ++ show i ++ " " ++ unwords (show <$> is)


accCondToHoa :: HoaAccCond -> String
accCondToHoa (FinCond i) = "Fin" ++ inParens (show i)
accCondToHoa (InfCond i) = "Inf" ++ inParens (show i)
accCondToHoa (CompFinCond i) = "Fin" ++ inParens ("!" ++ show i)
accCondToHoa (CompInfCond i) = "Inf" ++ inParens ("!" ++ show i)
accCondToHoa (AccAnd e1 e2) = inParens (accCondToHoa e1 ++ " & " ++ accCondToHoa e2)
accCondToHoa (AccOr e1 e2) = inParens (accCondToHoa e1 ++ " | " ++ accCondToHoa e2)
accCondToHoa (AccBoolExpr b) = if b then "t" else "f"


labelExprToHoa :: LabelExpr -> String
labelExprToHoa (LBoolExpr b) = if b then "t" else "f"
labelExprToHoa (RefAP i) = show i
labelExprToHoa (RefAlias s) = "@" ++ unpack s
labelExprToHoa (LNot e) = "!" ++ labelExprToHoa e
labelExprToHoa (LAnd e1 e2) = inParens (labelExprToHoa e1 ++ " & " ++ labelExprToHoa e2)
labelExprToHoa (LOr e1 e2) = inParens (labelExprToHoa e1 ++ " | " ++ labelExprToHoa e2)


bodyItemToHoa :: BodyItem -> String
bodyItemToHoa b = ("State: " ++
                   maybeBlank labelExprToHoa (stateLabel b) ++
                   show (num b) ++ " " ++
                   maybeBlank (inDoubleQuotes . unpack) (descr b) ++
                   maybeBlank (inCurls . unwords . map show) (stateAccSig b) ++ "\n" ++
                   unlines [maybeBlank (inBrackets . labelExprToHoa) (edgeLabel e) ++
                            " " ++
                            concat (intersperse "&" (show <$> stateConj e)) ++
                            " " ++
                            maybeBlank (inCurls . unwords . map show) (accSig e)
                            | e <- edges b])


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


maybeBlank :: (a -> String) -> Maybe a -> String
maybeBlank _ Nothing = " "
maybeBlank f (Just a) = f a


inDoubleQuotes :: String -> String
inDoubleQuotes s = "\"" ++ s ++ "\""


inCurls :: String -> String
inCurls s = "{" ++ s ++ "}"


inParens :: String -> String
inParens s = "(" ++ s ++ ")"


inBrackets :: String -> String
inBrackets s = "[" ++ s ++ "]"
