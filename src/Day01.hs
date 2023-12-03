module Day01 where

import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.List (find, inits, tails)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..))
import Paths_aoc2023 (getDataFileName)

type CalibrationDocument = String

type CalibrationLine = String

type Subsequencer = CalibrationLine -> [String]

type Digit = String

type Solution = CalibrationLine -> Int

digitWords :: [String]
digitWords = ["one","two","three","four","five","six","seven","eight","nine"]

wordToNum :: String -> Digit
wordToNum word = fromMaybe word $ lookup word wordMap
  where wordMap = zip digitWords $ show <$> ([1..] :: [Int])

isDigitChar :: String -> Bool
isDigitChar s
  | length s == 1 = isDigit $ head s
  | otherwise = False

isDigitWord :: String -> Bool
isDigitWord = getAny . foldMap isPrefixOfAny digitWords
  where isPrefixOfAny word = Any . (== word)

isDigitWordOrChar :: String -> Bool
isDigitWordOrChar s = isDigitWord s || isDigitChar s

subStrs :: Subsequencer
subStrs = tails >=> inits

reverseSubStrs :: Subsequencer
reverseSubStrs = fmap reverse . subStrs . reverse

searchString :: Subsequencer -> CalibrationLine -> Maybe Digit
searchString getSubStrs = fmap wordToNum . find isDigitWordOrChar . getSubStrs

solution1 :: Solution
solution1 = read . firstAndLast . filter isDigit
  where
    firstAndLast s
      | null s = "0"
      | otherwise = head s : [last s]

solution2 :: Solution
solution2 str = read $ fromMaybe "0" $ (++) <$> firstDigit <*> lastDigit
  where
    firstDigit = searchString subStrs str
    lastDigit = searchString reverseSubStrs str

showSolution :: String -> Solution -> CalibrationDocument -> IO ()
showSolution label solution document = putStrLn $ label ++ show answer
  where answer = sum $ solution <$> lines document

day01 :: IO ()
day01 = do
  calibrationDocument <- getDataFileName "day01-input.txt" >>= readFile
  showSolution "Puzzle 1: " solution1 calibrationDocument
  showSolution "Puzzle 2: " solution2 calibrationDocument
