{-# Language OverloadedStrings  #-}
module Utils (getPuzzleInput, toInt, deleteElemAt, toPairs) where

import Data.Text (Text)

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import qualified Data.List as List
import qualified Data.List.Split as List

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpsClient
import qualified Network.HTTP.Types.Header as HttpHeader
import qualified Network.HTTP.Types.Status as HttpStatus

import qualified System.Directory as FileSystem
import qualified System.Environment as Environment

newtype Year = Year Int deriving Show
newtype Day = Day Int deriving Show

type CookiePair = (String, String)


mkPuzzleFolderPath :: Year -> IO String
mkPuzzleFolderPath (Year year) = do
  puzzleFolder <- Environment.getEnv "AOC_PUZZLE_PATH" 
  return $ puzzleFolder <> "/" <> show year

mkPuzzleFilePath :: Year -> Day -> IO String
mkPuzzleFilePath year (Day day) =  do
    puzzleFolder <- mkPuzzleFolderPath year
    return $ puzzleFolder <> "/" <> show day <> ".txt" 

getPuzzleInput :: Int -> Int -> IO Text
getPuzzleInput year day = do
  getPuzzleInput' (Year year) (Day day)

getPuzzleInput' :: Year -> Day -> IO Text
getPuzzleInput' year day = do
  puzzleFolderPath <- mkPuzzleFolderPath year
  puzzleFilePath <- mkPuzzleFilePath year day
  fileExists <- FileSystem.doesFileExist puzzleFilePath
  if fileExists
    then readPuzzleInput puzzleFilePath
    else saveAndReadPuzzleInput year day 

saveAndReadPuzzleInput :: Year -> Day -> IO Text
saveAndReadPuzzleInput year day = do
  content <- fetchPuzzleInput year day
  filePath <- savePuzzleInput year day content
  readPuzzleInput filePath

    
readPuzzleInput :: String -> IO Text
readPuzzleInput filepath = do
  content <- Text.readFile  filepath
  return content

mkGetInputRequest :: Year -> Day -> IO HttpClient.Request
mkGetInputRequest (Year year) (Day day) = do
  request <- HttpClient.parseRequest $ "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
  authToken <- Environment.getEnv "AOC_AUTH_TOKEN"
  return $ injectAuthTokenCookie authToken request

fetchPuzzleInput :: Year -> Day -> IO Text
fetchPuzzleInput year day = do
  manager <- HttpClient.newManager HttpsClient.tlsManagerSettings
  request <- mkGetInputRequest year day 
  response <- HttpClient.httpLbs request manager
  case HttpStatus.statusCode $ HttpClient.responseStatus response of
    200 -> return $ Text.decodeUtf8 . LazyByteString.toStrict $ HttpClient.responseBody response
    _ -> return Text.empty

savePuzzleInput :: Year -> Day -> Text -> IO String
savePuzzleInput year day puzzleInput = do
  folderPath <- mkPuzzleFolderPath year
  _ <- FileSystem.createDirectoryIfMissing True folderPath
  inputPath <- mkPuzzleFilePath year day
  Text.writeFile inputPath puzzleInput
  return inputPath


injectAuthTokenCookie :: String -> HttpClient.Request -> HttpClient.Request
injectAuthTokenCookie authToken request = do
  let hasSessionId = checkSessionIdCookie (HttpClient.requestHeaders request)
  let modifiedRequest = if not hasSessionId
        then request { HttpClient.requestHeaders = ("Cookie", formatCookies [("session", authToken)]) : HttpClient.requestHeaders request }
        else request 
  modifiedRequest
  

-- Parse cookie string and check for sessionId
checkSessionIdCookie :: HttpHeader.RequestHeaders -> Bool
checkSessionIdCookie headers = 
    case lookup "Cookie" headers of
        Nothing -> False
        Just cookieStr -> hasSessionId $ parseCookies cookieStr

-- Parse cookie string into key-value pairs
parseCookies :: ByteString.ByteString -> [(ByteString.ByteString, ByteString.ByteString)]
parseCookies cookieStr = 
    [ let (k,v) = break (=='=') cookie in
      (ByteString.strip $ ByteString.pack k, ByteString.strip . ByteString.pack . drop 1 $ v)
    | cookie <- List.splitOn ";" (ByteString.unpack cookieStr)
    ]

-- Check if sessionId exists in parsed cookies
hasSessionId :: [(ByteString.ByteString, ByteString.ByteString)] -> Bool
hasSessionId cookies = List.any (\(k,_) -> k == "session") cookies

formatCookies :: [CookiePair] -> ByteString.ByteString
formatCookies cookies = ByteString.pack $ List.intercalate "; " 
    [key ++ "=" ++ value | (key, value) <- cookies]

toInt :: forall a . (Num a, Read a) => Text -> a
toInt = fromInteger . read . Text.unpack 

deleteElemAt :: forall a . Int -> [a] -> [a]
deleteElemAt _ [] = []
deleteElemAt 0 (_x : xs) = xs
deleteElemAt n xs = take n xs ++ drop (n + 1) xs

toPairs :: forall a. [a] -> [(a, a)]
toPairs xs = zip xs $ tail xs
