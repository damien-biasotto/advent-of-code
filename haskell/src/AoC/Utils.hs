{-# LANGUAGE OverloadedStrings #-}
module AoC.Utils (getPuzzleInput, Year(..), Day(..)) where

import Prelude hiding (writeFile, readFile)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (Either(..))
import Data.Text (Text(..), pack, unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO (writeFile, readFile)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, requestHeaders, Request, Response,  responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue, renderSetCookieBS)
import qualified Data.Text as Text

import GHC.Generics

newtype Year = Year Text deriving (Show)
newtype Day = Day Text deriving (Show)
newtype Puzzle = Puzzle Text deriving (Show)
newtype Error = Error Text deriving (Show)

checkDirectory :: Either Error FilePath -> IO Bool
checkDirectory = either (const $ return False) doesDirectoryExist  

getPuzzleInput :: Year -> Day -> IO (Either Error Text)
getPuzzleInput year day = do
  puzzleFolder' <-  lookupEnv "PUZZLE_FOLDER"
  let puzzleFolder = maybeToEither (Error "Environment variable PUZZLE_FOLDER not found") puzzleFolder'
  puzzleFolderExists <- checkDirectory puzzleFolder
  case not puzzleFolderExists of
    True -> pure $ Left (Error "Unable to find the folder specified in PUZZLE_FOLDER environment variable")
    False -> case puzzleFolder of
               Left err -> return . Left $ err
               Right fp -> getPuzzleInput' fp year day

getPuzzleInput' :: FilePath -> Year -> Day -> IO (Either Error Text)
getPuzzleInput' puzzleFolder (Year year) (Day day) = do
  let basePath = puzzleFolder <> "/" <> unpack year <> "/" <> unpack day
  createDirectoryIfMissing True basePath
  let puzzleFilePath = basePath <> "/input.txt"
  readPuzzleInput puzzleFilePath (Year year) (Day day)

readPuzzleInput :: FilePath -> Year -> Day -> IO (Either Error Text)
readPuzzleInput fp year day = do
  shouldFetchFirst <- not <$> doesFileExist fp
  if shouldFetchFirst
    then do
      puzzle <- fetchPuzzleInput year day
      _ <- writePuzzle puzzle fp
      readPuzzleInput' fp
    else
      readPuzzleInput' fp

readPuzzleInput' :: FilePath -> IO (Either Error Text)
readPuzzleInput' fp = do
  text <- readFile fp
  return $ Right text

fetchPuzzleInput :: Year -> Day -> IO (Either Error Text)
fetchPuzzleInput (Year year) (Day day) = do
  authCookie <- generateAuthCookie
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ "https://adventofcode.com/" <> unpack year <> "/day/" <> unpack day <> "/input"
  case authCookie of
    Left err -> return $ Left err
    Right token -> do
      let request =  initialRequest { requestHeaders = ("Cookie", renderSetCookieBS token) : requestHeaders initialRequest }
      response <- httpLbs request manager
      return $ Right $ pack . BL8.unpack $ responseBody response      

writePuzzle :: Either Error Text -> FilePath -> IO (Either Error FilePath)
writePuzzle (Left err) _ = do
  return $ Left err
writePuzzle (Right text) fp = do
  writeFile fp text
  return $ Right fp

maybeToEither :: Error -> Maybe a -> Either Error a
maybeToEither _ (Just z) = Right z
maybeToEither error _ = Left error

generateAuthCookie :: IO (Either Error SetCookie)
generateAuthCookie = do
  authToken <- lookupEnv "AOC_AUTH_TOKEN"
  case authToken of
    Nothing -> return . Left $ Error "Missing env variable AOC_AUTH_TOKEN"
    Just token -> return . Right $ defaultSetCookie { setCookieName = "session"
                                                    , setCookieValue = B8.pack token
                                                    }

