module Day02 where

import Prelude hiding (lines, words)
import Data.List (nub)
import Data.Text (Text, lines, words, strip)

import Utils (toInt, deleteElemAt,  getPuzzleInput, toPairs)  

parseInput :: forall a. (Num a, Read a) => Text -> [[a]]
parseInput = map (map toInt) . map words . lines . strip

isUniformelySorted :: forall a. (Ord a) =>[a] -> Bool
isUniformelySorted xs = (== 1) . length . nub . map (uncurry compare) $ toPairs xs

isUniformelyWithinRange :: forall a. (Num a, Ord a) => [a] -> Bool
isUniformelyWithinRange xs = (all withinRange $ map (abs . uncurry (-)) $ toPairs xs)

isSafe :: forall a. (Num a, Ord a) => [a] -> Bool
isSafe xs = (isUniformelySorted xs) && (isUniformelyWithinRange xs)

withinRange :: forall a. (Num a, Ord a) => a -> Bool
withinRange n = (0 < n) && (n <= 3)

isSafeWithProblemDampener :: forall a. (Num a,Ord a) => [a] -> Bool
isSafeWithProblemDampener xs = isSafe xs || (not . null $ (filter isSafe $ generateDampenReports xs))

generateDampenReports :: [a] -> [[a]]
generateDampenReports xs = go (length xs) xs []
  where
    go 0 _ acc = acc
    go n ys acc = deleteElemAt (n - 1) ys : go (n - 1) ys acc
  
solve :: IO ()
solve =  do
  text <- getPuzzleInput 2024 2
  print $ part1 $ parseInput text
  print $ part2 $ parseInput text


part1 :: forall a . (Num a, Read a, Ord a) => [[a]] -> Int
part1 = length . filter isSafe

part2 :: forall a . (Num a, Read a, Ord a) => [[a]] -> Int
part2 =  length . filter isSafeWithProblemDampener
