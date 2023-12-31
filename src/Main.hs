module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1" : _ -> day01
    "2" : _ -> day02
    "3" : _ -> day03
    "4" : _ -> day04
    "5" : _ -> day05
    _ -> error "None or invalid day number provided."
