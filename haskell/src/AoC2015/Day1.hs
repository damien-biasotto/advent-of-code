{-# LANGUAGE OverloadedStrings #-}
module AoC2015.Day1 where

import Prelude hiding (foldl)

import Data.Either
import Data.Text (foldl, Text)
import AoC.Utils

solve :: IO ()
solve = do
  puzzle <- getPuzzleInput (Year "2015") (Day "One")
  case puzzle of
    Left err ->  print err
    Right input -> print $ partOne input 
  
partOne :: Text  -> Integer
partOne = foldl (\acc c -> case c of
                                  '(' -> acc + 1
                                  ')' -> acc - 1
                                  _ -> acc) 0

partTwo :: Text -> Integer
partTwo = undefined 
