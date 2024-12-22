module Main where

import Data.Bits (xor)
import Data.Map.Strict qualified as M

main = interact (unlines . sequence [part1, part2] . map (secret . read) . lines)

part1, part2 :: [[Int]] -> String
part1 = ("Part 1: " ++) . show . sum . map (!! 2000)
part2 = ("Part 2: " ++) . show . maximum . M.unionsWith (+) . map (bananas . take 2001)

bananas = M.fromListWith (\_ v -> v) . sequences 4 . prices
  where
    prices = map (`mod` 10)
    sequences n = zip . consecutive n . changes <*> drop n
    changes = zipWith (flip (-)) <*> drop 1

secret :: Int -> [Int]
secret = iterate evolve
  where
    evolve n = foldl (\n' op -> (op n' `xor` n') `mod` 16777216) n ops
    ops = [(* 64), (`div` 32), (* 2048)]

consecutive n xs
  | length (take n xs) < n = []
  | otherwise = take n xs : consecutive n (drop 1 xs)
