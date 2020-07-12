module Main where

import Text.Parsec.String (parseFromFile)
import TeX (parseTeX, process)
import System.Environment (getArgs)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseFromFile parseTeX str >>= either print process
    _ -> error "need the .bbl file path to parse"
