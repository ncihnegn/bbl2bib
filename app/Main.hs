module Main where

import Text.Parsec.String (parseFromFile)
import TeX (parseTeX, process)
import System.Environment (getArgs)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [input, output] -> parseFromFile parseTeX input >>= either print (process output)
    _ -> error "need the input .bbl and output .bib paths"
