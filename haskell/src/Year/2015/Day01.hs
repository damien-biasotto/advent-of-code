module Day01 where

import Data.Text (Text)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Utils

run :: IO ()
run = do
  puzzleInput <- Utils.getPuzzleInput 2015 1
  print "Day 01:"
  print $ solve puzzleInput

solve :: Text -> (Int, Int)
solve input = (part1 input, part2 input)

parseChar :: Char -> Int
parseChar '(' = 1
parseChar ')' = (-1)
parseChar _ = 0

part1 :: Text -> Int
part1 = List.sum . List.map parseChar . Text.unpack

part2 :: Text -> Int
part2 input = List.head $ List.findIndices (== (-1)) $ List.scanl (+) 0 $ List.map parseChar . Text.unpack $ input
