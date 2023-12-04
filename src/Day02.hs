module Day02 where

import Data.Foldable (traverse_)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP
  (char, choice, eof, ReadP, sepBy1, string)
import Utils.Parsing (parseReadable, runParser)

type GameId = Int

type Color = String

data Cubes = Cubes
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Show, Eq)

data Game = Game GameId [Cubes]

instance Ord Cubes where
  this <= that = all colorLT [red, green, blue]
    where colorLT color = color this <= color that

sortCubes :: [(Int, Color)] -> Cubes
sortCubes cubes = Cubes
  { red = getCubesByColor "red" cubes
  , green = getCubesByColor "green" cubes
  , blue = getCubesByColor "blue" cubes
  }
    where
      getCubesByColor color = sum . fmap fst . filterByColor color
      filterByColor color = filter $ (==color) . snd

parseCubes :: ReadP Cubes
parseCubes = fmap sortCubes $ sepBy1 parseColorCount $ string ", "
  where
    parseColorCount = (,) <$> parseCount <*> parseColor
    parseCount = parseReadable <* char ' '
    parseColor = choice $ string <$> ["red", "green", "blue"]

parseGame :: ReadP Game
parseGame = Game <$> parseGameId <*> sepBy1 parseCubes cubeSep
  where
    parseGameId = string "Game " *> parseReadable <* string ": "
    cubeSep = string "; "

parseGames :: ReadP [Game]
parseGames = sepBy1 parseGame sep <* end
  where
    sep = char '\n'
    end = char '\n' *> eof

targetCubes :: Cubes
targetCubes = Cubes { red = 12 , green = 13 , blue = 14 }

gameIsPossible :: Game -> Bool
gameIsPossible (Game _ cubeSets) = all (<= targetCubes) cubeSets

parsePossibleGames :: ReadP [Game]
parsePossibleGames = filter gameIsPossible <$> parseGames

parseSumOfPossibleGameIds :: ReadP Int
parseSumOfPossibleGameIds = sum . fmap getId <$> parsePossibleGames
  where getId (Game gameId _) = gameId

day02 :: IO ()
day02 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  traverse_ print $ runParser parseSumOfPossibleGameIds input
