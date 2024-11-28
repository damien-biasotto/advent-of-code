module Day02 where

import Data.Text (Text)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Utils

run :: IO ()
run = do
  puzzleInput <- Utils.getPuzzleInput 2015 2
  print "Day 02:"
  print $ solve puzzleInput


solve :: Text -> (Int, Int)
solve input = (part1 input, part2 input)

newtype Length = Length Int deriving (Show, Eq)
newtype Width = Width Int deriving (Show, Eq)
newtype Height = Height Int deriving (Show, Eq)
newtype Dimension = Dimension (Length, Width, Height) deriving (Show, Eq)
  
findSmallestSideArea :: Dimension -> Int
findSmallestSideArea = (`div` 2) . List.minimum . surfaceAreasOfTheBox  

surfaceAreasOfTheBox :: Dimension -> [Int]
surfaceAreasOfTheBox (Dimension (Length length, Width width, Height height)) = 2 * length * width : 2 * width * height : 2 * height * length : []

wrappingPaperNeededPerBox :: Dimension -> Int
wrappingPaperNeededPerBox dimension =  List.sum $ findSmallestSideArea dimension : surfaceAreasOfTheBox dimension

ribbonNeededPerBox :: Dimension -> Int
ribbonNeededPerBox d  = cubicVolume d + smallestPerimeter d

smallestPerimeter :: Dimension -> Int 
smallestPerimeter d = List.sum $ List.map (*2) $ List.init $ List.sort $ toArray d

cubicVolume :: Dimension -> Int
cubicVolume = List.product . toArray

toArray :: Dimension -> [Int]
toArray (Dimension (Length l, Width w, Height h)) = [l , w, h]

parseInput :: Text -> [Dimension]
parseInput = List.map toDimension . Text.lines 

toInt :: String -> Int
toInt = read 

toDimension :: Text -> Dimension
toDimension = toDimension' . List.map (toInt . Text.unpack) . Text.splitOn (Text.pack "x")

toDimension' :: [Int] -> Dimension
toDimension' dims = Dimension (Length $ dims !! 0, Width $ dims !! 1, Height $ dims !! 2) 

part1 :: Text -> Int
part1 = List.sum . List.map wrappingPaperNeededPerBox . parseInput

part2 :: Text -> Int
part2 = List.sum . List.map ribbonNeededPerBox . parseInput
