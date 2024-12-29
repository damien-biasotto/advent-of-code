{-# language OverloadedStrings #-}
module Day06 where

import Prelude

import Control.Concurrent.Async 
import Data.Array (Array, (//), (!))
import Data.Bifunctor (bimap)
import Data.Either(partitionEithers)
import Data.Set (Set)
import Data.Text (Text)

import GHC.Generics

import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Utils 

type Coord = (Int, Int)
type Grid = Array Coord Cell
data Direction = North | East | South | West  deriving (Show, Eq, Generic, Enum, Ord)
data Cell = Empty | Obstacle deriving (Show, Eq)
data Guard = Guard Coord Direction deriving (Show, Eq, Ord)
type Route = Set Guard 

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

turn :: Guard -> Guard
turn (Guard coordinates West) = Guard coordinates North
turn (Guard coordinates direction) = Guard coordinates $ succ direction

parseDirection :: Char -> Direction
parseDirection '^' = North
parseDirection '>' = East
parseDirection 'v' = South
parseDirection '<' = West
parseDirection _ = error "Unknown direction"

parseCell :: Char -> Either Cell Direction
parseCell '#' = Left Obstacle
parseCell '.' = Left Empty
parseCell c
  | c `elem` (Text.unpack "^v<>") = Right $ parseDirection c
  | otherwise = Left Empty

parseInput :: Text -> (Grid, Guard)
parseInput input
  | null guards = error "No guard detected"
  | length guards > 1 = error "Multiple guards detected"
  | otherwise = (grid, guard) where
      rows = lines $ Text.unpack input
      bounds = ((0,0), (length rows - 1, length (head rows) - 1))
      (cells, guards) = partitionEithers [ bimap ((i,j),) ((i,j),) $ parseCell x | (xs,j) <- zip rows [0..], (x, i) <- zip xs [0..] ]
      guard@(Guard coordinates _) = uncurry Guard $ head guards
      grid = Array.array bounds ((coordinates, Empty) : cells)

move :: Guard -> Guard
move (Guard (x, y) dir) = case dir of
  North -> Guard (x, y -1) dir
  East -> Guard (x + 1, y) dir
  South -> Guard (x, y + 1) dir
  West -> Guard (x - 1, y) dir

isInBounds :: Coord -> Grid -> Bool
isInBounds (x, y) grid = x >= 0 && y >= 0 && x <= xMax && y <= yMax
  where (xMax, yMax) = snd $ Array.bounds grid

route :: Grid -> Guard -> (Route, Bool)
route = route' Set.empty False

route' :: Route -> Bool -> Grid -> Guard -> (Route, Bool)
route' r isCycle grid guard@(Guard _coordinates _direction)
  | isCycle = (r, isCycle)
  | not $ isInBounds coordinates' grid = (r, isCycle)
  | Empty <- grid ! coordinates' = route' (Set.insert guard' r) (Set.member guard' r) grid guard'
  | otherwise = route' (Set.insert guard'' r) (Set.member guard'' r) grid guard''
  where
    guard'@(Guard coordinates' _) = move guard
    guard'' = turn guard


part1 :: Text -> Int
part1 input =
  length $ Set.insert coord $ Set.map (\(Guard coordinates _) -> coordinates) r
  where
    o@(_, Guard coord _ ) = parseInput input
    r = fst $ uncurry route o

part2 :: Text -> IO Int
part2 input = do
  let (grid, guard@(Guard guardCoord _)) = parseInput  input
      coords = Set.filter (/= guardCoord) $ Set.map (\(Guard coord _) -> coord) $ fst $ route grid guard
      doesCreateCycle coord = pure $ snd $ route (grid // [(coord, Obstacle)]) guard
  xs <- mapConcurrently doesCreateCycle $ Set.elems coords
  return $ length $ filter id  xs

solve :: IO ()
solve = do
  rawInput <- Utils.getPuzzleInput 2024 6
  print $ part1 rawInput
  solution <- part2 rawInput
  print $ solution
