{-#Language OverloadedStrings #-}
module Day04 where

import Prelude hiding (words, elem)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import qualified Utils as Utils

type Input = Map (Int, Int) Char

parseInput :: Text -> Input
parseInput txt = do
  let
    ls :: [[Char]]
    ls = lines $ unpack txt
  Map.fromList [ ((i,j), c) | (i,l) <- zip [0 ..] ls, (j,c) <- zip [0 ..] l]

candidates :: Input ->  [[(Int,Int)]]
candidates input = concatMap candidatesFrom (Map.keys input)

candidatesFrom  :: (Int, Int) -> [[(Int,Int)]]
candidatesFrom p = map (goFrom 4 p) directions

part1 :: Input -> Int
part1 input =
  length $ filter (checkCandidate input) (candidates input)

checkCandidate :: Input -> [(Int, Int)] -> Bool
checkCandidate input path = lookupCandidate input path == Just "XMAS" 

lookupCandidate :: Input -> [(Int, Int)] -> Maybe String
lookupCandidate input = traverse (\p -> Map.lookup p input)

directions :: [(Int, Int)]
directions = [(i,j) | i <- [-1 .. 1], j <- [-1 .. 1], not (i == 0 && j == 0)]
  
goFrom :: Int -> (Int, Int) -> (Int, Int) -> [(Int,Int)]
goFrom n (i1,j1) (i2,j2) = take n (iterate (.+. (i2, j2))  (i1, j1))

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i1,j1) .+. (i2, j2) = (i1+i2, j1+j2)

(.-.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i1,j1) .-. (i2, j2) = (i1-i2, j1-j2)

tripleAround :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
tripleAround p d = [p .-. d, p,  p .+. d]

directionPairs :: [((Int, Int), (Int, Int))]
directionPairs = [(d1, d2) | d1 <- [(-1, -1), (1, 1)], d2 <- [(1, -1), (-1, 1)]]

xsFrom :: (Int, Int) -> [[(Int, Int)]]
xsFrom p = map (\(d1, d2) -> (tripleAround p d1 ++ tripleAround p d2)) directionPairs

part2 :: Input -> Int
part2 input =
  length $ filter (checkX input) (xs input)

checkX :: Input -> [(Int, Int)] -> Bool
checkX input path = lookupCandidate input path == Just "MASMAS" 

xs :: Input ->  [[(Int,Int)]]
xs input = concatMap xsFrom (Map.keys input)

sample :: Text
sample = pack "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"


solve :: IO ()
solve =  do
  txt <- Utils.getPuzzleInput 2024 4
  let parsedText = parseInput txt
  print $ part1 parsedText
  print $ part2 parsedText
