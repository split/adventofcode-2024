module Main where

import Control.Applicative ((*>), (<*), (<*>))
import Control.Monad (msum, void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Functor

type Parser = Parsec Void String

main = interact (unlines . sequence [part1, part2])

part1 = ("Part 1: " ++) . show . sum . msum . parse (parser anySingle) ""
part2 = ("Part 2: " ++) . show . sum . msum . parse (parser (skipDisabled <|> void anySingle)) ""

parser skip = many $ try (manyTill skip (lookAhead (try mul)) *> mul)

mul :: Parser Int
mul = string "mul" *> between (char '(') (char ')') ((*) <$> decimal <* char ',' <*> decimal)

skipDisabled = string "don't()" *> manyTill anySingle (void (string "do()") <|> eof) $> ()