{-# LANGUAGE OverloadedStrings #-}
module Main where
import OmegaAutomata.Hoa
import OmegaAutomata.LDBA
import OmegaAutomata.Automata
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified  Data.Text.IO as IOT
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= actionsForArgs


printHoa :: (Ord q, Show q, Show l) => ([HeaderItem], [BodyItem])
                    -> NBA q (Maybe LabelExpr) l
                    -> IO ()
printHoa (hs, _) automaton = let
  props = [AP b | (AP b) <- hs]
  (hs', bs) = nbaToHoa automaton in
    putStr $ toHoa (props ++ hs', bs)


parseHoaOrExit :: T.Text -> IO ([HeaderItem], [BodyItem])
parseHoaOrExit s = do
  let p = parseOnly parseHoa s
  case p of
    (Right (hs, bs)) -> return (hs, bs)
    _ -> putStrLn "An error occured during parsing of HOA input." >> die'


action2LDBA :: T.Text -> IO ()
action2LDBA s = do
  hoa <- parseHoaOrExit s
  let nba = hoaToNBA hoa
  printHoa hoa (toLDBA nba)


action2Complement :: T.Text -> IO ()
action2Complement s = do
  hoa <- parseHoaOrExit s
  let nba = hoaToNBA hoa
  printHoa hoa (buchiComplement nba)


actionIntersection :: T.Text -> T.Text -> IO ()
actionIntersection s1 s2 = do
  hoa1 <- parseHoaOrExit s1
  hoa2 <- parseHoaOrExit s2
  let (nba1, nba2) = (hoaToNBA hoa1, hoaToNBA hoa2)
  printHoa hoa1 (buchiIntersection nba1 nba2)


actionUnion :: T.Text -> T.Text -> IO ()
actionUnion s1 s2 = do
  hoa1 <- parseHoaOrExit s1
  hoa2 <- parseHoaOrExit s2
  let (nba1, nba2) = (hoaToNBA hoa1, hoaToNBA hoa2)
  printHoa hoa1 (buchiUnion nba1 nba2)


actionIsLDBA :: T.Text -> IO ()
actionIsLDBA s  = do
  hoa <- parseHoaOrExit s
  let isLDBA = isLimitDeterministic $ hoaToNBA hoa
  putStrLn (if isLDBA then "true" else "false")


actionsForArgs :: [String] -> IO ()
actionsForArgs ("-isldba":xs) = pipeOrFile xs actionIsLDBA
actionsForArgs ("-2ldba":xs) = pipeOrFile xs action2LDBA
actionsForArgs ("-complement":xs) = pipeOrFile xs action2Complement
actionsForArgs ("-intersection":xs) = pipeOrFiles xs actionIntersection
actionsForArgs ("-union":xs) = pipeOrFiles xs actionUnion
actionsForArgs ["-h"] = usage >> exit
actionsForArgs ["--help"] = usage >> exit
actionsForArgs _ = usage >> die'

pipeOrFile :: [String] -> (T.Text -> IO ()) -> IO ()
pipeOrFile xs action = case xs of
  [] -> IOT.getContents >>= action
  [fn] -> IOT.readFile fn >>= action
  _ -> usage >> die'


pipeOrFiles :: [String] -> (T.Text -> T.Text -> IO ()) -> IO ()
pipeOrFiles xs action = case xs of
  [fn] -> do
    s1 <- IOT.getContents
    s2 <- IOT.readFile fn
    action s1 s2
  [fn1, fn2] -> do
    s1 <- IOT.readFile fn1
    s2 <- IOT.readFile fn2
    action s1 s2
  _ -> usage >> die'


usage :: IO ()
usage = do
  putStrLn "ldba-tool - A tool for limit-deterministic Buchi automata. \n"
  putStrLn "Usage:  ldba-tool [args]\n"
  putStrLn "args:"
  putStrLn " -isldba [fname]                  Tell if NBA is limit-deterministic"
  putStrLn " -2ldba [fname]                   Convert NBA to LDBA"
  putStrLn " -complement [fname]              Complement NBA to LDBA"
  putStrLn " -intersection fname [fname]      Intersection-LDBA of two LDBAs"
  putStrLn " -union fname [fname]             Union-LDBA of two LDBAs"
  putStrLn "\n All automata must be specified in Hanoi-Omega-Automata format."


exit :: IO a
exit = exitWith ExitSuccess

die' :: IO a
die' = exitWith (ExitFailure 1)
