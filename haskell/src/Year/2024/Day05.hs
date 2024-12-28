{-# Language OverloadedStrings #-}
module Day05 where

import Prelude hiding (words, elem)
import Control.Monad (void)
import Data.Function (on)
import Data.List (nub, elemIndex, sortBy, elem, foldl')
import Data.Map.Strict (Map, (!))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Utils

data Input = Input { pageOrderings :: [PageOrdering]
                   , updates :: [Update]
                   } deriving (Show, Eq)

type Parser = Parsec Void Text

newtype Page = Page Int deriving (Show, Eq, Num, Ord)
newtype PageOrdering = PageOrdering (Page, Page) deriving (Show, Eq)
newtype Update = Update [Page] deriving (Show, Eq, Ord)

newtype Rules = Rules (Map Page [Page]) deriving (Show, Ord, Eq)

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

page :: Parser Page
page = do
  p <- lexeme Lexer.decimal
  return $ Page p

pageOrderingParser :: Parser PageOrdering
pageOrderingParser = do
  first <- page
  void $ char '|'
  second <- page
  return $ PageOrdering (first, second)

pageOrderingsParser :: Parser [PageOrdering]
pageOrderingsParser = pageOrderingParser `sepEndBy` eol

generateRules :: [PageOrdering] -> Rules
generateRules pos = Rules $ foldl' (\acc (PageOrdering (p1,p2)) ->
                                  case Map.lookup p1 acc of
                                    Just _ -> Map.adjust ([p2] ++) p1 acc
                                    Nothing -> Map.insert p1 [p2] acc
                               ) Map.empty pos

updateParser :: Parser Update
updateParser = do
  updates <- page `sepBy` char ','
  return $ Update updates

updatesParser :: Parser [Update]
updatesParser = updateParser `sepEndBy` eol

inputParser :: Parser Input
inputParser = do
  pos <- pageOrderingsParser
  void $ some eol
  ups <- updatesParser
  eof
  return $ Input pos ups

parseInput :: Text -> Either (ParseErrorBundle Text Void) Input
parseInput = parse inputParser ""

isValidUpdate :: Update -> [PageOrdering] -> Bool
isValidUpdate (Update ups) pos = (== 1) $ length $ nub $ catMaybes $ map (checkPageOrdering ups) pos

getMiddlePage :: Update -> Maybe Int
getMiddlePage (Update []) = Nothing
getMiddlePage (Update xs) = let
  (Page middle) = xs !! (length xs `div` 2)
  in
  Just middle

checkPageOrdering :: [Page] -> PageOrdering -> Maybe Bool
checkPageOrdering p (PageOrdering (f, s)) =
  (<) <$> (elemIndex f p) <*> (elemIndex s p)

inTuple :: Page -> PageOrdering -> Bool
inTuple x (PageOrdering (y,z)) = x == y || z == x

part1 :: Input -> Int
part1 (Input pageOrderings updates) = sum $ catMaybes $ map getMiddlePage $ filter (flip isValidUpdate $ pageOrderings) updates

part2 :: Input -> Int
part2 (Input pageOrderings updates) = sum $ catMaybes $ map getMiddlePage $ map (reorderInvalidUpdate (generateRules pageOrderings)) $ filter (flip isInvalidUpdate $ pageOrderings) updates

isInvalidUpdate :: Update -> [PageOrdering] -> Bool 
isInvalidUpdate u p = not $ isValidUpdate u p

selectPageOrderingsForUpdate :: [PageOrdering] -> Update -> [PageOrdering]
selectPageOrderingsForUpdate pos (Update up) = filter (\(PageOrdering (p,n)) -> p `elem` up || n `elem` up) pos

selectPageOrderingsForPage :: [PageOrdering] -> Page -> [PageOrdering]
selectPageOrderingsForPage pos pa = filter (\(PageOrdering (p,n)) -> pa == p || pa == n) pos    

reorderInvalidUpdate :: Rules -> Update -> Update
reorderInvalidUpdate r (Update ups) = Update $ sortBy (reorderInvalidUpdate' r) ups

reorderInvalidUpdate' :: Rules -> Page -> Page -> Ordering
reorderInvalidUpdate' (Rules rules) p1 p2
  | Map.member p1 rules && not (Map.member p2 rules) = LT
  | Map.member p2 rules && not (Map.member p1 rules) = GT
  | p2 `elem` (rules ! p1) = LT
  | p1 `elem` (rules ! p2) = GT
  | otherwise = EQ

  
solve :: IO ()
solve =  do
  rawInput <- Utils.getPuzzleInput 2024 5
  let input = parseInput (Text.strip rawInput)
  case input of
    Left err -> print err
    Right inp -> do
      print $ part1 inp
      print $ part2 inp
