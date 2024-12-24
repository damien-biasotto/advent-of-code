{-# Language OverloadedStrings #-}
module Day06 where

import Prelude

import Control.Monad (void)
import Data.Char 
import Data.Either(partitionEithers)
import Data.Functor (($>))
import Data.Hashable
import Data.HashSet (HashSet)
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Pos (SourcePos)


import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Utils

data Direction = UP | DOWN | LEFT | RIGHT  deriving (Show, Eq)
instance Hashable Direction

data Guard = Guard { coords :: (Int,Int)
                   , direction :: Direction
                   } deriving (Show, Eq, Generic)
instance Hashable Guard 

newtype Obstacle = Obstacle (Int, Int) deriving (Show, Eq, Generic)
instance Hashable Obstacle

data Input = Input { guard ::  Guard
                   , obstacles :: HashSet Obstacle
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
  return $ Obstacle (unPos $ sourceLine loc, unPos $ sourceColumn loc)

parseGuard :: Parser Guard
parseGuard = do
  loc <- getSourcePos
  dir <- parseDirection
  let coords =  (unPos $ sourceLine loc, unPos $ sourceColumn loc)
  return $ Guard coords dir

parseDirection :: Parser Direction
parseDirection = choice
  [ UP <$ char '^'
  , DOWN <$ char 'v'
  , LEFT <$ char '<'
  , RIGHT <$ char '>'
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
  return $ Input (head guards) $ HashSet.fromList obstacles

runParserInput :: Text -> Either (ParseErrorBundle Text Void) Input
runParserInput = parse parseInput ""

move :: Guard -> HashSet Obstacle -> (Int, Int) -> HashSet Guard -> HashSet Guard
move (Guard (y,x) dir) obstacles limits@(y',x') acc =
  case dir of
    UP    | y == 1 -> acc
    RIGHT | x == x' -> acc
    DOWN  | y == y' -> acc
    LEFT  | x == 1 -> acc
    _ -> case nextPos of
           pos | (Obstacle pos) `elem` obstacles -> move (Guard (y,x) nextDir) obstacles limits ((Guard (y,x) nextDir): acc)
               | otherwise -> move (Guard pos dir) obstacles limits ((Guard pos dir): acc)
  where
    nextPos = case dir of
      UP    -> (y-1, x)
      RIGHT -> (y, x+1)
      DOWN  -> (y+1, x)
      LEFT  -> (y, x-1)
    
    nextDir = case dir of
      UP    -> RIGHT
      RIGHT -> DOWN
      DOWN  -> LEFT
      LEFT  -> UP

removeDuplicatePosition :: [Guard] -> Map (Int,Int) [Guard]
removeDuplicatePosition xs = foldl' (\acc g@(Guard coords _) -> case Map.lookup coords acc of
                                        Nothing -> Map.insert coords [g] acc
                                        Just _ -> Map.adjust (g :) coords acc
                                    ) Map.empty xs
part1' :: Input -> (Int, Int) -> [Guard]
part1' inp lim = reverse $ move (guard inp) (obstacles inp) lim [guard inp]

part1 :: Input -> (Int, Int) -> Int
part1 inp lim = length $ Map.keys $ removeDuplicatePosition $ part1' inp lim

-- | Predicate for paths that loop instead of running off the edge of the map.
-- <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare>
isLoop :: Eq a => [a] -> Bool
isLoop a = go a a
  where
   go (x:xs) (_:y:ys) = x == y || go xs ys
   go _      _        = False

part2 :: Input -> (Int, Int) -> IO ()
part2 inp lim =
  let
      guard'@(Guard c@(y,x) _) :: Guard = guard inp
      visited :: [(Int,Int)] = Map.keys $ removeDuplicatePosition $ part1' inp lim
      obstacles' :: [Obstacle] = obstacles inp
      positions = [ pos | pos <- visited, pos /= c]
  in
    do
      print $ take 1 $ filter (\p -> isLoop (move guard' ((Obstacle p) : obstacles') lim [])) positions
                
  
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
