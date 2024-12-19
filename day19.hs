module Main where

import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M

main = interact (unlines . sequence [part1, part2] . uncurry buildDesigns . parse)

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . length . filter (> 0)
part2 = ("Part 2: " ++) . show . sum

buildDesigns :: [String] -> [String] -> [Int]
buildDesigns towels = map (buildDesign towels)

buildDesign :: [String] -> String -> Int
buildDesign towels = fst . go M.empty
  where
    go dp xs
      | null xs = (1, dp)
      | xs `M.member` dp = (dp M.! xs, dp)
      | otherwise = let (result, dp') = foldr design (0, dp) towels in (result, M.insert xs result dp')
      where
        design t (acc, dp)
          | t `isPrefixOf` xs = let (dc, dp') = go dp (drop (length t) xs) in (acc + dc, dp <> dp')
          | otherwise = (acc, dp)

parse = ((,) . splitOn ", " . head <*> drop 2) . lines
