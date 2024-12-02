module Main where
import Control.Monad (ap)

main = interact (unlines . sequence [part1, part2] . map (map read . words) . lines)

part1 :: [[Int]] -> String
part1 = ("Part 1: " ++) . show . length . filter safe

part2 :: [[Int]] -> String
part2 = ("Part 2: " ++) . show . length . filter dumper

safe :: [Int] -> Bool
safe = ap ((&&) . sameSign) (all ((<= 3) . abs)) . (zipWith (-) <*> drop 1)

dumper = any safe . choices
  where
    choices [] = []
    choices (x : xs) = xs : map (x :) (choices xs)

sameSign :: (Eq a, Num a) => [a] -> Bool
sameSign [] = True
sameSign (x : xs) = all ((== signum x) . signum) xs