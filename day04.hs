module Main where
import Control.Monad (ap)
import Data.List (tails, transpose, elemIndices, isPrefixOf)

main = interact (unlines . sequence [part1] . lines)

part1 :: [String] -> String
part1 = ("Part 1: " ++) . show . count "XMAS" . ap (++) mirror . ap (++) diagonal . rotations

rotations = concat . take 4 . iterate transpose
mirror = map reverse
diagonal station = [drop i row | (row, i) <- zip station [0..]]
count word = length . filter (isPrefixOf word) . tails . concat
