module Day01 where

import Data.Text (Text)
import Utils (getPuzzleInput)

import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Text as Text

toInt :: String -> Int
toInt i = read i :: Int

toTuple :: [a] -> (a, a)
toTuple (x:y:[]) = (x, y)
toTuple _ = error "Invalid input"

mapTuple :: forall a b . (a -> b) -> (a,a) -> (b,b)
mapTuple f (a,b) = (f a, f b)

sortTuple :: ([Int], [Int]) -> ([Int], [Int])
sortTuple = mapTuple List.sort

getDistance :: forall a . Num a => (a, a) -> a
getDistance = abs . Tuple.uncurry (-) 

getSimilarityScore :: [Int] -> Int -> Int
getSimilarityScore xs x = x * (List.length $ List.findIndices (== x) xs)

prepareInput :: Text -> [(Int,Int)]
prepareInput text =
  let
    lines :: [Text] = Text.lines text
    words :: [[Text]] = List.map Text.words lines
    ints :: [[Int]] = List.map (List.map (toInt . Text.unpack)) words
  in
    List.map toTuple ints

part1 :: Text -> Int
part1 text =  let
  sortedTuple :: [(Int, Int)] = Tuple.uncurry List.zip $ sortTuple (List.unzip $ prepareInput text)
  in
  List.sum $ List.map getDistance sortedTuple

part2 :: Text -> Int
part2 text = let
    tuples :: ([Int],[Int]) = List.unzip $ prepareInput text
  in
  List.sum $ List.map (getSimilarityScore $ Tuple.snd tuples) (Tuple.fst tuples)  
  
solve :: IO ()
solve =  do
  text <- getPuzzleInput 2024 01
  print $ part1 text
  print $ part2 text
