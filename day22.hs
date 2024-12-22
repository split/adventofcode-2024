module Main where

import Data.Bits (xor)

main = interact (unlines . sequence [part1] . lines)

part1 = ("Part 1: " ++) . show . sum . map ((!! 2000). secret . read)

secret :: Int -> [Int]
secret n = dp
    where
        dp = [evolve i | i <- [0..]]
        ops = [(* 64), (`div` 32), (* 2048)]
        evolve 0 = n
        evolve i = foldl (\n' op -> (op n' `xor` n') `mod` 16777216) (dp !! (i - 1)) ops
