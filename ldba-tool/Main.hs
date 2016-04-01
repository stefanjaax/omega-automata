{-# LANGUAGE OverloadedStrings #-}
module Main where
import OmegaAutomata.Hoa
import Data.Attoparsec.ByteString
import Data.ByteString as BS
import System.IO as I

main :: IO ()
main = do
  s <- BS.getContents
  let p = parseOnly parseHoa s
  case p of
    (Right (hs, bs)) -> I.putStr (toHoa (hs,bs))
    _ -> print "An error occured."
