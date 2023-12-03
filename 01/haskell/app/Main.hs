module Main  where

import Prelude
import Trebuchet 
import System.Environment

main :: IO ()
main = do
  filePath <- getEnv "INPUT_FILE"
  fileContent <- readFile filePath
  let sum  = trebuchet fileContent
  putStrLn $ show sum
