module Main where

import Data.List (isSuffixOf)
import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1, part2] . uncurry (map . buildDesign) . parse)

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . length . filter (> 0)
part2 = ("Part 2: " ++) . show . sum

buildDesign :: [String] -> String -> Int
buildDesign towels xs = dp !! length xs
  where
    dp = [tryDesign i | i <- [0 .. length xs]]
    tryDesign i
      | i == 0 = 1
      | otherwise = sum [dp !! (i - length t) | t <- towels, t `isSuffixOf` take i xs]

parse = ((,) . splitOn ", " . head <*> drop 2) . lines