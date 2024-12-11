module Main where

import Data.IntMap.Strict qualified as M

main = interact (unlines . sequence [part1, part2] . map read . words)

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . M.foldr (+) 0 . (!! 25) . iterate blink . inc 1 M.empty
part2 = ("Part 2: " ++) . show . M.foldr (+) 0 . (!! 75) . iterate blink . inc 1 M.empty

blink = M.foldrWithKey (\n c s -> inc c s (rule n)) M.empty

inc c = foldr (\x -> M.insertWith (+) x c)

rule n = case n of
  0 -> [1]
  n | even digits -> [n `div` base, n `mod` base]
  n -> [n * 2024]
  where
    digits = length (show n)
    base = 10 ^ (digits `div` 2)
