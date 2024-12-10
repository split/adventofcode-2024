module Main where

import Data.Char (digitToInt)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (..), ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as S

type Point = (Int, Int)

type TypographicMap = M.Map Point Int

type State = (Point, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: TypographicMap -> String
part1 = ("Part 1: " ++) . show . trailheads . trails
part2 = ("Part 2: " ++) . show . length . trails

trailheads = S.size . S.fromList . map route

starts = M.assocs . M.filter (== 0)

trails :: TypographicMap -> [Seq State]
trails tm = go (Seq.fromList (Seq.singleton <$> starts tm)) []
  where
    go Seq.Empty trails = trails
    go (trail :<| queue) trails = case viewr trail of
      _ :> (_, 9) -> go queue (trail : trails)
      _ :> (pos, height) -> go (queue >< foldMap (nextStates . move pos) dirs) trails
        where
          nextStates pos' = case M.lookup pos' tm of
            Just h | h == height + 1 -> Seq.singleton (trail |> (pos', h))
            _ -> Seq.empty

route trail = case (viewl trail, viewr trail) of
  (x :< _, _ :> y) -> (x, y)

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]

move (x, y) (dx, dy) = (x + dx, y + dy)

parse :: String -> TypographicMap
parse input = M.fromList [((x, y), digitToInt c) | (y, line) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] line]