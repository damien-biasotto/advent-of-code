{-# language OverloadedStrings #-}
module Day06 where

import Prelude

import Control.Monad (void)
import Data.Char
import Data.Either(partitionEithers)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char


import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Direction = North | South | West | East  deriving (Show, Eq, Generic)
data Guard = Guard { coords :: Coord
                   , direction :: Direction
                   } deriving (Show, Eq, Generic)

newtype XCoord = XCoord Int deriving (Show, Eq, Generic)
newtype YCoord = YCoord Int deriving (Show, Eq, Generic)
newtype Coord = Coord (XCoord, YCoord) deriving (Show, Eq, Generic)

newtype Obstacle = Obstacle Coord deriving (Show, Eq, Generic)

data Input = Input { guard ::  Guard
                   , obstacles :: [Obstacle]
                   } deriving (Show)

type Parser = Parsec Void Text

sample :: Text
sample = Text.unlines
         [ "....#....."
         , ".........#"
         , ".........."
         , "..#......."
         , ".......#.."
         , ".........."
         , ".#..^....."
         , "........#."
         , "#........."
         , "......#..."
         ]

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme  spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space filler empty empty

isSep :: Char -> Bool
isSep x = isSpace x || isPunctuation x

filler :: Parser ()
filler = void $ some $ satisfy isSep

symbol :: Text -> Parser Text
symbol = Lexer.symbol hspace

parseObstacle :: Parser Obstacle
parseObstacle = do
  loc <- getSourcePos
  void $ char '#'
  return $ Obstacle $ Coord (XCoord $ unPos $ sourceColumn loc, YCoord $ unPos $ sourceLine loc)

parseGuard :: Parser Guard
parseGuard = do
  loc <- getSourcePos
  dir <- parseDirection
  let coords =  Coord (XCoord $ unPos $ sourceColumn loc, YCoord $ unPos $ sourceLine loc)
  return $ Guard coords dir

parseDirection :: Parser Direction
parseDirection = choice
  [ North <$ char '^'
  , South <$ char 'v'
  , West <$ char '<'
  , East <$ char '>'
  ]

parseCell :: Parser (Maybe (Either Guard Obstacle))
parseCell = choice
  [ Just . Left <$> parseGuard
  , Just . Right <$> parseObstacle
  , Nothing <$ char '.'
  ]

parseInput :: Parser Input
parseInput = do
  rows <- some (some parseCell <* optional newline)
  let cells = concat rows
  let (guards, obstacles) = partitionEithers . catMaybes $ cells
  return $ Input (head guards) obstacles

runParserInput :: Text -> Either (ParseErrorBundle Text Void) Input
runParserInput = parse parseInput ""

move :: Guard -> [Obstacle] -> (Int, Int)  -> [Coord] -> [Coord]
move (Guard c@(Coord c'@(XCoord x, YCoord y)) dir) obstacles limits@(y',x') acc =
  case dir of
    North    | y == 1 -> acc
    East | x == x' -> acc
    South  | y == y' -> acc
    West  | x == 1 -> acc
    _ -> case nextPos of
           pos | (Obstacle pos) `elem` obstacles -> move (Guard c nextDir) obstacles limits (c : acc)
               | otherwise -> move (Guard pos dir) obstacles limits (pos : acc)
  where
    nextPos = case dir of
      North   -> Coord (XCoord x, YCoord $ y-1)
      East -> Coord (XCoord $ x+1, YCoord y)
      South  -> Coord (XCoord x, YCoord $ y+1)
      West  -> Coord (XCoord $ x-1, YCoord y)

    nextDir = case dir of
      North    -> East
      East -> South
      South  -> West
      West  -> North

part1' :: Input -> (Int, Int) -> [Coord]
part1' inp lim =
  let
    guard' :: Guard = guard inp
  in
  move (guard inp) (obstacles inp) lim [coords guard']

part1 :: Input -> (Int, Int) -> Int
part1 inp lim = length $ nub $ part1' inp lim

-- | Predicate for paths that loop instead of running off the edge of the map.
-- <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare>
isLoop :: Eq a => [a] -> Bool
isLoop [] = False
isLoop xs = go xs xs
  where
    go (y:ys) (_:z:zs) = y == z || go ys zs
    go _ _ = False

part2 :: Input -> (Int, Int) -> IO ()
part2 inp lim =
  let
      guard'@(Guard c _) :: Guard = guard inp
      visited :: [Coord] = part1' inp lim
      obstacles' :: [Obstacle] = obstacles inp
      positions = filter (c /=) visited
      attempt p = move guard' ((Obstacle p) : obstacles') lim [c]
      check = isLoop . attempt
  in
    do
      print $ length positions
      print $ check $ reverse positions !! 1
--      print $ take 1 $ filter check $ reverse positions


solve :: IO ()
solve = do
--  rawInput <- Utils.getPuzzleInput 2024 6
  let rawInput = sample
  let rawInput' = Text.lines rawInput
  let result = runParserInput rawInput
  case result of
    Left err -> print err
    Right input -> do
      print $ guard input
      let solution1 = part1 input (length $ rawInput', Text.length $ rawInput' !! 0)
      print solution1
      part2 input (length $ rawInput', Text.length $ rawInput' !! 0)
