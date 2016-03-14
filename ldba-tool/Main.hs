{-# LANGUAGE OverloadedStrings #-}
module Main where
import OmegaAutomata.Hoa
import Data.Attoparsec.ByteString
import Data.ByteString as BS

main :: IO ()
main = do
  s <- BS.getContents
  let p = parseOnly parseHoa s
  print $ show p
  return ()
