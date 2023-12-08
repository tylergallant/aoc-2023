module Day03 where

import Control.Applicative (asum, optional)
import Control.Monad.State.Lazy (evalStateT, get, lift, put, StateT(..))
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP (char, eof, look, munch1, ReadP, satisfy)
import Utils.Parsing (runParser)

data Element = Blank | Symbol Char | Number Int deriving Show

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
parseSchematic = parseElements
  where
    parseElements = parseManyTill parseElement $ lift eof
    parseElement = asum elementParsers <* optional parseNewLine
    elementParsers = [parseBlank, parseSymbol, parseNumber]

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

adjacent :: Position -> Position -> Bool
adjacent (x, y) (x', y')
  | (x, y) /= (x', y') && abs (x - x') <= 1 && abs (y - y') <= 1 = True
  | otherwise = False

elementsAdjacent :: IndexedElement -> IndexedElement -> Bool
elementsAdjacent (_, as) (_, bs) = or $ adjacent <$> as <*> bs

partNumbers :: Schematic -> Schematic
partNumbers schematic =
  fmap fst . filter adjs $ (,) <$> numbers schematic <*> symbols schematic
    where adjs = uncurry elementsAdjacent

sumPartNumbers :: Schematic -> Int
sumPartNumbers = sum . fmap getNumber . partNumbers
  where
    getNumber = matchNumber . fst
    matchNumber (Number n) = n
    matchNumber _ = 0

day03 :: IO ()
day03 = getInput >>= printSchematic . parseInput
    where
      getInput = getDataFileName "day03-input.txt" >>= readFile
      parseInput = runParser $ evalStateT parseSchematic (0, 0)
      printSchematic = traverse_ $ print . sumPartNumbers
