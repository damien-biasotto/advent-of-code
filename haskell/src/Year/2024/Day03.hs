module Day03 where

import Prelude hiding (lines, words)
import Data.List (nub)
import Data.Text (Text, lines, words, strip, pack)

import Utils (toInt, deleteElemAt,  getPuzzleInput)  

  
solve :: IO ()
solve =  do
  text <- getPuzzleInput 2024 3
  print $ text
