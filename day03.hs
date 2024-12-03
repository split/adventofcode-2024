{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((*>), (<*), (<*>))
import Control.Monad (msum)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

main = interact (unlines . sequence [part1])

part1 = ("Part 1: " ++) . show . sum . msum . parse parser ""

parser = many (try $ manyTill anySingle (lookAhead (try mul)) *> mul <* optional eol)

mul :: Parser Int
mul = string "mul" *> between (char '(') (char ')') ((*) <$> decimal <* char ',' <*> decimal)