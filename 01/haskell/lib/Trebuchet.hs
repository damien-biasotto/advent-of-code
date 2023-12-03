module Trebuchet where

import Prelude
import Data.Char

trebuchet :: String -> Int
trebuchet fileContent = (foldl to_number 0 (map (filter (not . isAlpha)) (words fileContent))) :: Int

to_number :: Int -> [Char] -> Int
to_number acc (x:[])  = acc + (read ([x] ++ [x]) :: Int)
to_number acc (x:y:[]) = acc + (read ([x] ++ [y]) :: Int)
to_number acc (head: tail) = acc + (read ([head] ++ [last tail]) :: Int)
to_number acc _ = acc + 0
