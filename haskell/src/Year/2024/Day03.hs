{-# Language DerivingStrategies #-}
{-# Language OverloadedStrings #-}
module Day03 where

import Prelude hiding (lines, words)

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Utils (getPuzzleInput)

type Parser = Parsec Void Text

type Input = [Instruction]

data Instruction
  = Mul Int Int
  | Enable
  | Disable
  deriving stock (Show, Eq)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

number :: (Integral a) => Parser a
number = L.signed (pure ()) L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

string :: Parser Text
string = pack <$> lexeme (some alphaNumChar)

parser :: Parser Input
parser = catMaybes <$> some (go <* optional eol) <* eof
  where
    go =
      choice
        [ Just <$> try parseMul,
          Just Enable <$ try (symbol "do()"),
          Just Disable <$ try (symbol "don't()"),
          anySingle $> Nothing
        ]

parseMul :: Parser Instruction
parseMul = symbol "mul" *> parens (Mul <$> (lexeme L.decimal <* symbol ",") <*> lexeme L.decimal)

instructionValue :: Instruction -> Int
instructionValue (Mul x y) = x * y
instructionValue _ = 0

part1 :: Input -> Int
part1 = foldl' (\accumulator instruction -> accumulator + instructionValue instruction) 0

part2 :: Input -> Int
part2 instructions = foldl' (\accumulator instruction -> accumulator + instructionValue instruction) 0 (go instructions [] True)
  where
    go (mul@(Mul _ _) : muls) accumulator process = if process then go muls (mul : accumulator) process else go muls accumulator process
    go (Enable : muls) accumulator _ = go muls accumulator True
    go (Disable : muls) accumulator _ = go muls accumulator False
    go [] acc _ = acc

solve :: IO ()
solve =  do
  input <- getPuzzleInput 2024 3
  let instructions = fromRight [] $ parse parser "" input
  print $ part1 instructions 
  print $ part2 instructions 
