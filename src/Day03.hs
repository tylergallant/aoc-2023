module Day03 where

import Control.Applicative (asum, optional)
import Control.Monad.State.Lazy (evalStateT, get, lift, put, StateT(..))
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (nubBy)
import Data.Tuple (swap)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP (char, eof, look, munch1, ReadP, satisfy)
import Utils.Parsing (runParser)

data Element = Blank | Symbol Char | Number Int deriving (Show, Eq)

type Position = (Int, Int)

type IndexedElement = (Element, [Position])

type Schematic = [IndexedElement]

type Parser = StateT Position ReadP

parseBlank :: Parser IndexedElement
parseBlank = do
  el <- lift $ Blank <$ char '.'
  (x, y) <- get
  put (x + 1, y)
  return (el, [(x, y)])

parseSymbol :: Parser IndexedElement
parseSymbol = do
  symbol <- lift . satisfy $ \c -> not $ isDigit c || c == '.' || c == '\n'
  (x, y) <- get
  put (x + 1, y)
  return (Symbol symbol, [(x, y)])

parseNumber :: Parser IndexedElement
parseNumber = do
  digits <- lift $ munch1 isDigit
  (x, y) <- get
  let x' = x + length digits
  put (x', y)
  return (Number $ read digits, [(x'', y) | x'' <- [x..x' - 1]])

parseNewLine :: Parser ()
parseNewLine = do
  lift . void $ char '\n'
  (_, y) <- get
  put (0, y + 1)

parseManyTill :: Parser a -> Parser end -> Parser [a]
parseManyTill element end = scan
  where
    reachedEnd (_, pos) = put pos >> return []
    parseElement = (:) <$> element <*> scan
    scan = do
      input <- lift look
      pos <- get
      let parseEnd = runStateT end pos
      maybe parseElement reachedEnd $ runParser parseEnd input

parseSchematic :: Parser Schematic
parseSchematic = parseManyTill parseElement $ lift eof
  where
    parseElement = asum parsers <* optional parseNewLine
    parsers = [parseBlank, parseSymbol, parseNumber]

symbols :: Schematic -> Schematic
symbols = filter f
  where
    f (Symbol _, _) = True
    f _ = False

numbers :: Schematic -> Schematic
numbers = filter f
  where
    f (Number _, _) = True
    f _ = False

asterisks :: Schematic -> Schematic
asterisks = filter f
  where
    f (Symbol '*', _) = True
    f _ = False

distance :: Position -> Position -> Int
distance (x, y) (a, b) = floor hyp
  where
    hyp = sqrt $ adj ** 2 + opp ** 2 :: Double
    adj = len x a
    opp = len y b
    len n m = abs $ fromIntegral n - fromIntegral m

elementDistance :: IndexedElement -> IndexedElement -> Int
elementDistance (_, is) (_, js) = minimum $ distance <$> is <*> js

adjacent :: IndexedElement -> IndexedElement -> Bool
adjacent a b = elementDistance a b == 1

partNumbers :: Schematic -> Schematic
partNumbers schematic = fst <$> filter adjs candidates
  where
    adjs = uncurry adjacent
    candidates = (,) <$> numbers schematic <*> symbols schematic

numberPairs :: Schematic -> [(IndexedElement, IndexedElement)]
numberPairs schematic = nubBy mirrored $ filter nearby pairs
  where
    nums = numbers schematic
    pairs = (,) <$> nums <*> nums
    mirrored pair = (== swap pair)
    nearby (a, b) = (a /= b &&) . (<= 2) $ elementDistance a b

gears :: Schematic -> [(IndexedElement, (IndexedElement, IndexedElement))]
gears schematic = filter isGear gearCandidates
  where
    isGear (g, (n, m)) = adjacent g n && adjacent g m
    gearCandidates = (,) <$> asterisks schematic <*> numberPairs schematic

gearRatios :: Schematic -> [Int]
gearRatios schematic = uncurry (*) . gearParts <$> gears schematic
  where
    gearParts (_, ((Number n, _), (Number m, _))) = (n, m)
    gearParts _ = (0, 0)

sumPartNumbers :: Schematic -> Int
sumPartNumbers = sum . fmap getNumber . partNumbers
  where
    getNumber = matchNumber . fst
    matchNumber (Number n) = n
    matchNumber _ = 0

totalGearRatios :: Schematic -> Int
totalGearRatios = sum . gearRatios

day03 :: IO ()
day03 = readSchematic >>= traverse_ runSolutions
    where
      file = "day03-input.txt"
      readSchematic = fmap parseInput $ getDataFileName file >>= readFile
      parseInput = runParser $ evalStateT parseSchematic (0, 0)
      runSolutions schematic = do
        putStrLn . ("Puzzle 1: "++) . show $ sumPartNumbers schematic
        putStrLn . ("Puzzle 2: "++) . show $ totalGearRatios schematic
