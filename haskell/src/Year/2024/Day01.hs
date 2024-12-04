module Day01 where

import Prelude hiding (lines, words)

import Data.List (sort, findIndices)
import Data.Text (Text, unpack, lines, words)
import Utils (getPuzzleInput)

toInt :: String -> Int
toInt i = read i :: Int

toTuple :: [a] -> (a, a)
toTuple (x:y:[]) = (x, y)
toTuple _ = error "Invalid input"

mapTuple :: forall a b . (a -> b) -> (a,a) -> (b,b)
mapTuple f (a,b) = (f a, f b)

sortTuple :: ([Int], [Int]) -> ([Int], [Int])
sortTuple = mapTuple sort

getDistance :: forall a . Num a => (a, a) -> a
getDistance = abs . uncurry (-) 

getSimilarityScore :: [Int] -> Int -> Int
getSimilarityScore xs x = x * (length $ findIndices (== x) xs)

prepareInput :: Text -> [(Int,Int)]
prepareInput text =
    map toTuple $ map (map (toInt . unpack)) $ map words $ lines text

part1 :: Text -> Int
part1 text =  let
  sortedTuple :: [(Int, Int)] = uncurry zip $ sortTuple (unzip $ prepareInput text)
  in
  sum $ map getDistance sortedTuple

part2 :: Text -> Int
part2 text = let
    tuples :: ([Int],[Int]) = unzip $ prepareInput text
  in
  sum $ map (getSimilarityScore $ snd tuples) (fst tuples)  
  
solve :: IO ()
solve =  do
  text <- getPuzzleInput 2024 01
  print $ part1 text
  print $ part2 text
