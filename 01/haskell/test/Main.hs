module TrebuchetTest where

import Test.Hunit
import Trebuchet

testTrebuchetWithFixtureDataSet :: Test
testTrebuchetWithFixtureDataSet =
  TestCase $ assertEqual "It should return 142" 142 (trebuchet "../../fixture.txt")

testTrebuchetWithInputDataSet :: Test
testTrebuchetWithInputDataSet =
  TestCase $ assertEqual "It should return 55538" 142 (trebuchet "../../input.txt")


main :: IO Counts
main = runTestTT $ TestList [testTrebuchetWithFixtureDataSet, testTrebuchetWithInputDataSet]
