module Main where

import Test.HUnit
import Trebuchet
import qualified System.Exit as Exit

testTrebuchetWithFixtureDataSet :: String -> Test
testTrebuchetWithFixtureDataSet content =
  TestCase $ assertEqual "It should return 142" 142 (trebuchet content)

testTrebuchetWithInputDataSet :: String -> Test
testTrebuchetWithInputDataSet content =
  TestCase $ assertEqual "It should return 55538" 5538 (trebuchet content)

main :: IO Counts
main = do
  fixture <- readFile "/Users/damienbiasotto/Code/Perso/AoC/2023/01/fixture.txt"
  realData <- readFile "/Users/damienbiasotto/Code/Perso/AoC/2023/01/input.txt"
  result <- runTestTT (TestList $ [testTrebuchetWithFixtureDataSet fixture, testTrebuchetWithInputDataSet realData])

