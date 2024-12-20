module Main where

import Data.Map.Strict qualified as M
import Data.Set qualified as S

main = interact (unlines . sequence [part1, part2] . scan)

part1 = ("Part 1: " ++) . show . length . antinodes [1]
part2 = ("Part 2: " ++) . show . length . antinodes [0 .. 50]

antinodes n = inMap (S.unions . S.map (uncurry offset) . pairs)
  where
    pairs = S.filter (uncurry (/=)) . (S.cartesianProduct <*> id)
    inMap f = (S.intersection . S.unions) <*> (S.unions . M.map f . M.delete '.')
    offset (x, y) (x', y') = S.fromList [(x + i * (x - x'), y + i * (y - y')) | i <- n]

scan input = M.fromListWith S.union [(c, S.singleton (x, y)) | (l, y) <- zip (lines input) [0 ..], (c, x) <- zip l [0 ..]]