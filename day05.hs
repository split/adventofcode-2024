

module Main where

import Data.Either (lefts, rights)
import Data.List (partition, sortBy)
import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: [Either [Int] [Int]] -> String
part1 = ("Part 1: " ++) . show . sum . map mid . lefts
part2 = ("Part 2: " ++) . show . sum . map mid . rights

parts rules updates = zipWith cmp (map (sortBy rules) updates) updates
  where
    cmp sorted updates
      | sorted == updates = Left updates
      | otherwise = Right sorted

mid xs = xs !! (length xs `div` 2)

sorter :: (Ord a) => [(a, a)] -> a -> a -> Ordering
sorter rules a b = if (a, b) `elem` rules then LT else GT

parse = (\[r, u] -> parts (rules r) (updates u)) . map lines . splitOn "\n\n"
    where
        rules = sorter . map ((\[a,b] -> (read a, read b)) . splitOn "|")
        updates = map (map read . splitOn ",")