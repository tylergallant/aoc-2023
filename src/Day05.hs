module Day05 where

import Data.Foldable (traverse_)
import Data.List (find)
import Data.Monoid (Dual(..), Endo(..))
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP (char, get, many, optional, ReadP, sepBy, skipSpaces, string)
import Utils.Parsing (parseReadable, runParser)

type Seed = Int
type Dest = Int
type Source = Int
type Range = Int
type CategoryMapping = (Dest, Source, Range)
type CategoryMap = [CategoryMapping]
type Almanac = ([Seed], [CategoryMap])

parseCategoryMapping :: ReadP CategoryMapping
parseCategoryMapping = (,,) <$> pDest <*> pSource <*> pRange
  where
    pDest = parseReadable
    pSource = skipSpaces *> parseReadable
    pRange = skipSpaces *> parseReadable

parseCategoryMap :: ReadP CategoryMap
parseCategoryMap = parseTitle *> many parseMapping
  where
    pNewLine = char '\n'
    parseTitle = many get <* char ':' <* pNewLine
    parseMapping = parseCategoryMapping <* optional pNewLine

parseSeeds :: ReadP [Seed]
parseSeeds = string "seeds: " *> sepBy parseReadable skipSpaces

parseAlmanac :: ReadP Almanac
parseAlmanac = (,) <$> parseSeeds <*> parseMaps
  where
    parseMaps = parseSep *> sepBy parseCategoryMap parseSep
    parseSep = string "\n\n"

applyMap :: CategoryMap -> Dual (Endo Int)
applyMap catMap = Dual $ Endo f
  where
    f srcId = maybe srcId (apply srcId) $ find (mapsSrcId srcId) catMap
    mapsSrcId srcId (_, src, range) = srcId >= src && srcId < src + range
    apply srcId (dest, src, _) = (+) dest . abs $ srcId - src

nearestSeedLocation :: Almanac -> Int
nearestSeedLocation (seeds, maps) = minimum $ applyMaps <$> seeds
  where applyMaps = appEndo . getDual $ foldMap applyMap maps

runSolutions :: Almanac -> IO ()
runSolutions almanac = do
  putStrLn . ("Puzzle 1: " ++) . show $ nearestSeedLocation almanac

day05 :: IO ()
day05 = getDataFileName "day05-input.txt" >>= readFile >>= run . parse
  where
    parse = runParser parseAlmanac
    run = traverse_ runSolutions
