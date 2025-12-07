module Main where

import qualified Day1

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1"] -> Day1.run
    _     -> putStrLn "Usage: aoc2025 <day-number>"
