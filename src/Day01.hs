module Day01 where

import Data.Char (isDigit)
import Paths_aoc2023 (getDataFileName)

solution1 :: String -> Int
solution1 = sum . fmap calibrationValue . lines
  where
    calibrationValue = read . firstAndLast . filter isDigit
    firstAndLast s = head s : [last s]

day01 :: IO ()
day01 = do
  input <- getDataFileName "day01-input.txt" >>= readFile
  let answer1 = solution1 input
  putStrLn $ "Puzzle 1: " ++ show answer1
