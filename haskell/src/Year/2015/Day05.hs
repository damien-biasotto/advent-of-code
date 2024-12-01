module Day05 where

import Data.Text (Text)
import Utils (getPuzzleInput)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text


run :: IO ()
run = do
  puzzleInput <- getPuzzleInput 2015 5
  print "Day 05:"
  print $ part1 $ Text.lines puzzleInput


-- solve :: Text -> (Int, Int)
-- solve input = (part1 $ Text.lines input, part2 $ Text.lines input)

hasAtLeastThreeVowels :: Text -> Bool
hasAtLeastThreeVowels = (>= 3) . Text.length . Text.filter (\c -> c `elem` "aeiou")

hasAtLeastOneLetterAppearingTwiceInARow :: Text -> Bool
hasAtLeastOneLetterAppearingTwiceInARow text = (>= 1) . Text.length $ Text.filter (\c -> Text.isInfixOf (Text.replicate 2 $ Text.pack [c]) text) text

hasNoForbiddenSubString :: Text -> Bool
hasNoForbiddenSubString text = (== 0) . List.length . List.filter (\s -> Text.isInfixOf s text) $ List.map Text.pack ["ab", "cd", "pq", "xy"]

isNice :: Text -> Bool
isNice text = hasAtLeastThreeVowels text && hasAtLeastOneLetterAppearingTwiceInARow text && hasNoForbiddenSubString text

isNice2 :: Text -> Bool
isNice2 text = nonOverlappingPairAppearingTwice text && oneLetterWhichRepeatsWithExactlyOneLetterBetween text

nonOverlappingPairAppearingTwice :: Text -> Bool
nonOverlappingPairAppearingTwice text = False
oneLetterWhichRepeatsWithExactlyOneLetterBetween :: Text -> Bool
oneLetterWhichRepeatsWithExactlyOneLetterBetween text = False

part1 :: [Text] -> Int
part1 = List.length . List.filter isNice
  --part1 input = List.zip4 (List.map hasAtLeastThreeVowels input)  (List.map hasAtLeastOneLetterAppearingTwiceInARow input)  (List.map hasNoForbiddenSubString input) input

part2 :: [Text] -> Int
part2 = List.length . List.filter isNice2
