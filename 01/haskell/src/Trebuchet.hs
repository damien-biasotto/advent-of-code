module Main where

import Prelude
import Data.Char

trebuchet :: String -> IO Int
trebuchet filepath = do
  fileContent <- readFile filepath
  foldl (\(carry, item) -> carry + to_number item) 0 (filter (not . isAlpha) fileContent)

to_number :: [Char] -> Int
to_number (x:[])  = (x ++ x) :: Int
to_number (x:y:[]) = (x ++ y) :: Int
to_number (head: tail) = (head ++ last tail) :: Int
to_number _ = fail "Non reachable case"

