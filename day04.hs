module Main where

import Control.Monad (ap, guard)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S

type Point = (Int, Int)

type Station = M.Map Point Char

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Station -> String
part1 = ("Part 1: " ++) . show . length . (\s -> concatMap (\o -> filter (\d -> has s d o "XMAS") dirs) (M.keys s))
part2 = ("Part 2: " ++) . show . count . (\s -> concatMap (\o -> move o <$> filter (\d -> has s d o "MAS") diag) (M.keys s))

count = M.size . M.filter (> 1) . M.fromListWith (+) . map (,1)

has :: Station -> Point -> Point -> String -> Bool
has station dir@(dx, dy) origin = go
  where
    go "" = True
    go s@(c' : rest) = isJust $ do
      c <- station M.!? move origin (dx * length rest, dy * length rest)
      guard (c == c' && go rest)

parse :: String -> M.Map Point Char
parse input = M.fromList [((x, y), c) | (l, y) <- zip (lines input) [0 ..], (c, x) <- zip l [0 ..]]

diag, dirs :: [(Int, Int)]
diag = [(1, 1), (-1, 1), (-1, -1), (1, -1)]
dirs = diag ++ [(1, 0), (0, 1), (-1, 0), (0, -1)]

move (x, y) (dx, dy) = (x + dx, y + dy)