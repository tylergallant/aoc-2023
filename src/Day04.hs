module Day04 where

import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.List (intersect)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP
  (char, eof, manyTill, munch1, optional, ReadP, sepBy, skipSpaces, string)
import Utils.Parsing (runParser)

type Card = ([String], [String])
type Pile = [Card]

parseCard :: ReadP Card
parseCard = (,) <$> parseWinningNums <*> parseNums
  where
    parseWinningNums = parseCardNum *> parseIntList
    parseNums = skipSpaces *> char '|' *> skipSpaces *> parseIntList
    parseCardNum = string "Card" *> skipSpaces *> parseInt <* char ':' <* skipSpaces
    parseIntList = sepBy parseInt skipSpaces
    parseInt = munch1 isDigit

parsePile :: ReadP Pile
parsePile = manyTill parseCardLine eof
  where
    parseCardLine = parseCard <* optional parseNewLine
    parseNewLine = char '\n'

cardWins :: Card -> Int
cardWins = length . uncurry intersect

cardPoints :: Card -> Int
cardPoints = roundDown . (2 **) . fromIntegral . flip (-) 1 . cardWins
  where roundDown = floor :: Double -> Int -- Need to annotate the type here

totalPoints :: Pile -> Int
totalPoints = sum . fmap cardPoints

totalCards :: Pile -> Int
totalCards = sum . foldr foldPile []
  where
    -- Hint: Solving the problem backwards leads to a right-associative fold
    foldPile card cards =
      let wins = cardWins card
          copies = take wins cards
       in 1 + sum copies : cards

runSolutions :: Pile -> IO ()
runSolutions pile = do
  putStrLn . ("Puzzle 1: "++) . show $ totalPoints pile
  putStrLn . ("Puzzle 2: "++) . show $ totalCards pile

day04 :: IO ()
day04 = getDataFileName "day04-input.txt" >>= readFile >>= run . parse
  where
    parse = runParser parsePile
    run = traverse_ runSolutions
