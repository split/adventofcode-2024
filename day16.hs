module Main where

import Control.Monad (ap, guard)
import Data.Foldable (find)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)

type Point = (Int, Int)

type Grid a = Map Point a

main = interact (unlines . sequence [part1] . grid . lines)

part1 = ("Part 1: " ++) . show . lowestScore

lowestScore grid = fromMaybe 0 $ do
  start <- fst <$> find ((== 'S') . snd) (M.toList grid)
  end <- fst <$> find ((== 'E') . snd) (M.toList grid)
  dijkstra grid start end

dijkstra :: Map Point Char -> Point -> Point -> Maybe Int
dijkstra grid start end = dijkstra' initialHeap initialScores
  where
    initialHeap = H.singleton (0, (start, (1, 0)))
    initialScores = M.insert (start, (1, 0)) 0 (M.unions (map initScore dirs))
    initScore dir = M.fromList $ (\p -> ((p, dir), maxBound :: Int)) <$> M.keys grid
    toHeap = H.fromList . map swap . M.toList
    dijkstra' heap scores = do
      ((score, (point, dir)), heap') <- H.uncons heap
      if end == point
        then return score
        else
          let better = M.differenceWithKey pickBetter (neighbors (point, dir) scores) scores
              pickBetter key inc score' = do
                guard (score + inc <= score')
                return (score + inc)
           in dijkstra' (toHeap better <> heap') (better <> scores)

grid :: [String] -> Grid Char
grid rows = M.fromList [((x, y), col) | (cols, y) <- zip rows [0 ..], (col, x) <- zip cols [0 ..], col /= '#']

neighbors :: (Point, Point) -> Map (Point, Point) Int -> Map (Point, Point) Int
neighbors (point, dir) grid = M.fromList $ mapMaybe step $ neighborPoints point
  where
    step (point', dir') = do
      grid M.!? (point', dir')
      if dir == dir'
        then return ((point', dir), 1)
        else return ((point, dir'), 1000)

neighborPoints (x, y) = [((x + dx, y + dy), dir) | dir@(dx, dy) <- dirs]

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]