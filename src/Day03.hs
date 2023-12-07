module Day03 where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
  (filterWithKey, fromList, lookup, restrictKeys, toList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Paths_aoc2023 (getDataFileName)

newtype Idx = Idx (Int, Int) deriving Eq

type EngineSchematic = Map Idx Char

instance Ord Idx where
  Idx (x1, y1) <= Idx (x2, y2)
    | y1 == y2 = x1 <= x2
    | otherwise = y1 < y2

parseEngineSchematic :: String -> EngineSchematic
parseEngineSchematic input = Map.fromList indexed
  where
    indexed = do
      (y, line) <- zip [0..] $ lines input
      let makeIndex x c = (Idx (x, y), c)
      zipWith makeIndex [0..] line

neighborIdxs :: Idx -> Set Idx
neighborIdxs (Idx (x, y)) = Set.fromList $ Idx . addRelative <$> relativePos
  where
    addRelative (dx, dy) = (x + dx, y + dy)
    relativePos = filter notIdentical $ (,) <$> coords <*> coords
    coords = [-1, 0, 1]
    notIdentical (0, 0) = False
    notIdentical _ = True

neighbors :: Idx -> EngineSchematic -> EngineSchematic
neighbors idx schematic = Map.restrictKeys schematic $ neighborIdxs idx

isSymbol :: Char -> Bool
isSymbol c
  | c == '.' || isDigit c = False
  | otherwise = True

isPartNumberDigit :: EngineSchematic -> Idx -> Bool
isPartNumberDigit schematic idx = idxIsDigit && hasSymbolNeighbors
  where
    idxIsDigit = maybe False isDigit $ Map.lookup idx schematic
    hasSymbolNeighbors = any isSymbol $ neighbors idx schematic

getContiguousDigits :: Idx -> EngineSchematic -> [(Idx, Char)]
getContiguousDigits (Idx (x, y)) =
  let subsequents = Map.filterWithKey subsequent
   in getDigits . Map.toList . subsequents
  where
    subsequent (Idx (x', y')) = const $ y' == y && x' >= x
    getDigits = takeWhile (isDigit . snd)

maybePartNumber :: Idx -> EngineSchematic -> Maybe (Idx, Int)
maybePartNumber idx schematic
  | isPartNumber = Just . (,) end . read $ snd <$> digits
  | otherwise = Nothing
  where
    digits = getContiguousDigits idx schematic
    end = fst $ last digits
    isPartNumber =
      let isPartDigit = isPartNumberDigit schematic . fst
       in any isPartDigit digits

partNumbers :: EngineSchematic -> [Int]
partNumbers schematic = scan $ Idx (0, 0)
  where
    scan idx = maybeAddAndContinue idx $ maybePartNumber idx schematic
    maybeAddAndContinue idx = maybe (continueOrEnd idx) addNumAndContinue
    continueOrEnd idx = maybe [] scan $ nextIdx idx
    addNumAndContinue (Idx (x, y), n) = n : scan (Idx (x + 1, y))
    nextIdx (Idx (x, y)) =
      let rightIdx = Idx (x + 1, y)
          downIdx = Idx (0, y + 1)
          right = rightIdx <$ Map.lookup rightIdx schematic
          down = downIdx <$ Map.lookup downIdx schematic
       in right <|> down

solution1 :: EngineSchematic -> Int
solution1 = sum . partNumbers

day03 :: IO ()
day03 = do
  input <- getDataFileName "day03-input.txt" >>= readFile
  print . solution1 $ parseEngineSchematic input
