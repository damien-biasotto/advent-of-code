module Main where

import Test.HUnit
import Trebuchet

testTrebuchetWithFixtureDataSet :: Test
testTrebuchetWithFixtureDataSet =
  TestCase $ assertEqual "It should return 142" 142 (trebuchet "/Users/damienbiasotto/Code/Perso/AoC/2023/01/fixture.txt")

testTrebuchetWithInputDataSet :: Test
testTrebuchetWithInputDataSet =
  TestCase $ assertEqual "It should return 55538" 142 (trebuchet "/Users/damienbiasotto/Code/Perso/AoC/2023/01/input.txt")

main :: IO Counts
main = runTestTT $ TestList [testTrebuchetWithFixtureDataSet, testTrebuchetWithInputDataSet]
