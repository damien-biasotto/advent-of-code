module Day03 where

import Data.Text (Text)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Utils

run :: IO ()
run = do
  puzzleInput <- Utils.getPuzzleInput 2015 3
  print "Day 03:"
  print $ part1 puzzleInput
  print $ part2 puzzleInput
  

type Coords = (Int, Int)

defaultPosition :: (Int, Int)
defaultPosition = (0,0)

generateGrid :: [[Int]]
generateGrid = List.repeat $ List.repeat 0

move :: Coords -> Char -> Coords
move coords direction = case direction of
  '^' -> (fst coords + 1, snd coords)
  'v' -> (fst coords - 1, snd coords)
  '<' -> (fst coords, snd coords -1)
  '>' -> (fst coords, snd coords +1)
  _ -> coords


processDirection :: [Char] -> [Coords]
processDirection = List.scanl move (0,0)


solve :: Text -> (Int, Int)
solve input = (part1 input, part2 input)

part1 :: Text -> Int 
part1 = List.length . List.nub . processDirection . Text.unpack

roboSantaTrack :: Text -> [Char] 
roboSantaTrack = Text.unpack . Text.concat . List.map (Text.drop 1) . Text.chunksOf 2

humanSantaTrack :: Text -> [Char] 
humanSantaTrack = Text.unpack . Text.concat . List.map (Text.take 1) . Text.chunksOf 2

part2 :: Text -> Int
part2 text = List.length . List.nub $ (processDirection $ humanSantaTrack text) ++ (processDirection $ roboSantaTrack text)
