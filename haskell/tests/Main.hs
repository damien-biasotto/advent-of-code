module Main where

import qualified AoC2015.Day1 as Day1 (partOne)

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Text
import Test.Sandwich

simple :: TopSpec
simple = describe "AoC Test suite" $ do
  describe "2015 - Day One" $ do
    it "part1" $ do
      Day1.partOne (pack "(())") `shouldBe` 0
      Day1.partOne (pack "()()") `shouldBe` 0
      Day1.partOne (pack "(((") `shouldBe` 3
      Day1.partOne (pack "))(((((") `shouldBe` 3
      Day1.partOne (pack "())") `shouldBe` (-1)
      Day1.partOne (pack "))(") `shouldBe` (-1)
      Day1.partOne (pack ")))") `shouldBe` (-3)
      Day1.partOne (pack ")())())") `shouldBe` (-3)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions simple 
  
