module Day04 where

import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (nub)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP
  (char, eof, manyTill, optional, ReadP, sepBy, skipSpaces, string)
import Utils.Parsing (parseReadable, runParser)

type Card = ([Int], [Int])
type Pile = [Card]

parseCard' :: ReadP Card
parseCard' = do
  let parseInt = parseReadable :: ReadP Int
  winningNums <- sepBy parseInt skipSpaces
  void $ string " | "
  nums <- sepBy parseInt skipSpaces
  return (winningNums, nums)

parseCard :: ReadP Card
parseCard = (,) <$> parseWinningNums <*> parseNums
  where
    parseWinningNums = parseCardNum *> parseIntList
    parseNums = string " | " *> parseIntList
    parseCardNum = string "Card " *> parseInt <* string ": "
    parseIntList = sepBy parseInt skipSpaces
    parseInt = parseReadable :: ReadP Int

parsePile :: ReadP Pile
parsePile = manyTill parseCardLine eof
  where
    parseCardLine = parseCard <* optional parseNewLine
    parseNewLine = char '\n'

cardWins :: Card -> Int
cardWins (winningNums, nums) = length $ filter id equals
  where equals = (==) <$> nub winningNums <*> nub nums

cardPoints :: Card -> Int
cardPoints = roundDown . (2 **) . fromIntegral . flip (-) 1 . cardWins
  where roundDown = floor :: Double -> Int

totalPoints :: Pile -> Int
totalPoints = sum . fmap cardPoints

runSolutions :: Pile -> IO ()
runSolutions pile = putStrLn . ("Puzzle 1: "++) . show $ totalPoints pile

day04 :: IO ()
day04 = getDataFileName "day04-input.txt" >>= readFile >>= run . parse
  where
    parse = runParser parsePile
    run = traverse_ runSolutions
