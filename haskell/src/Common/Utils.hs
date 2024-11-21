{-# LANGUAGE OverloadedStrings #-}
module Common.Utils where

import Data.Text (Text)

import qualified Data.Text as Text
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as TLS
import qualified System.Directory as FileSystem
import qualified System.Environment as Environment
import qualified Web.Cookie as Cookie

newtype Year = Year Integer deriving Show
newtype Day =  Day Integer deriving Show

getPuzzleInput :: Year -> Day -> IO (Maybe Text)
getPuzzleInput (Year year) (Day day) = do
  puzzleFolder <- Environment.getEnv "AOC_PUZZLE_PATH"
  let filePath = puzzleFolder <> "/" <> show year <> "/" <> show day <> "/input.txt"
  if FileSystem.doesFileExist filePath
    then Just $ Text.readFile filePath
    else Just $ Text.readFile $ savePuzzleInput filePath $ fetchPuzzleInput year day

fetchPuzzleInput :: Year -> Day -> IO (Maybe Text)
fetchPuzzleInput (Year year) (Day day) = do
  manager <- HttpClient.newManager TlS.tlsManagerSettings
  aocAuthToken <- Environment.getEnv "AOC_AUTH_TOKEN"
  initialRequest <- HttpClient.parseRequest "https://adventofcode.com/" <> show year <> "/day/" <> show day
  let request = initialRequest { requestHeaders = ("Cookie", Cookie.renderSetCookieBS . Cookie.defaultSetCookie {setCookieName = "session", setCookieValue = aocAuthToken }) : requestHeaders initialRequest }
  response <- HttpClient.httpLbs request manager
  case HttpClient.response

savePuzzleInput :: FilePath -> Text -> FilePath
savePuzzleInput file content = do
  Text.writeFile file content
  return file
