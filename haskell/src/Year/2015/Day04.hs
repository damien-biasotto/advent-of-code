module Day04 where

import Data.Text (Text)

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Utils

run :: IO ()
run = do
  puzzleInput <- Utils.getPuzzleInput 2015 4
  print "Day 04:"
  print $ solve $ Text.init puzzleInput

hasWantedPrefix :: String -> String -> Bool
hasWantedPrefix prefix = (== prefix) . List.take (length prefix)

nextCharAfterPrefixIsANumber :: String -> String -> Bool
nextCharAfterPrefixIsANumber prefix = Char.isNumber . List.head . List.drop (List.length prefix)


isWantedHash :: String -> String -> Bool
isWantedHash prefix xs = hasWantedPrefix prefix xs && nextCharAfterPrefixIsANumber prefix xs


toHex :: ByteString.ByteString -> String
toHex = ByteStringChar8.unpack . ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString . ByteStringBuilder.byteStringHex

hashStringToHex' :: String -> String -> String 
hashStringToHex' secret input = toHex $ hashString' secret input

hashString' :: String -> String -> ByteString.ByteString
hashString' secretKey input = MD5.hash $ ByteStringChar8.pack secretKey <> ByteStringChar8.pack input

-- Hash string and convert to hex
hashString :: String -> String -> ByteString.ByteString
hashString secretKey = (MD5.hmac $ ByteStringChar8.pack secretKey)  . ByteStringChar8.pack

solve :: Text -> (Int, Int)
solve input = (part1 $ Text.unpack input, part2 $ Text.unpack input)

part1 :: String -> Int
part1 secretKey = fst . List.head $ List.filter (isWantedHash "00000" . snd ) $ List.zip [0..] $ List.map (hashStringToHex' secretKey) (List.map show [0..])


part2 :: String -> Int
part2 secretKey = fst . List.head $ List.filter (hasWantedPrefix "000000" . snd ) $ List.zip [0..] $ List.map (hashStringToHex' secretKey) (List.map show [0..])
