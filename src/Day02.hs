module Day02 where

import Data.Foldable (traverse_)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP
  (char, choice, eof, ReadP, sepBy1, skipSpaces, string)
import Utils.Parsing (parseReadable, runParser)

type GameId = Int

type Color = String

data Cubes = Cubes { red :: Int , green :: Int , blue :: Int }

data Game = Game GameId [Cubes]

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
parseGames = sepBy1 parseGame sep <* skipSpaces <* eof
  where sep = char '\n'

targetCubes :: Cubes
targetCubes = Cubes { red = 12 , green = 13 , blue = 14 }

possibleWith :: Cubes -> Cubes -> Bool
possibleWith minCubes maxCubes = all colorLT [red, green, blue]
  where colorLT color = color minCubes <= color maxCubes

gameIsPossible :: Game -> Bool
gameIsPossible (Game _ cubeSets) = all (`possibleWith` targetCubes) cubeSets

solution :: [Game] -> Int
solution = sum . fmap getId . filter gameIsPossible
  where getId (Game gameId _) = gameId

day02 :: IO ()
day02 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  printMaybe $ solution <$> runParser parseGames input
    where printMaybe = traverse_ print
