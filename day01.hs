module Main where

import Data.List (groupBy, sort, transpose)

main = interact (unlines . sequence [part1, part2] . parse)

part1 :: [[Int]] -> String
part1 = ("Part 1: " ++) . show . sum . map abs . (\[left, right] -> zipWith (-) left right)

part2 :: [[Int]] -> String
part2 = ("Part 2: " ++) . show . sum . (\[left, right] -> map (\n -> n * (length $ filter (== n) right)) left)

parse :: String -> [[Int]]
parse = map sort . transpose . map (map read . words) . lines