module Main where

import Control.Monad (ap, guard)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Heap qualified as H
import Data.List (find, inits)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing, mapMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)
import Debug.Trace

type Point = (Int, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: [Point] -> String
part1 points = ("Part 1: " ++) . maybe "" show . (!! steps points) $ bytes points
part2 points = ("Part 2: " ++) . show . search . drop (steps points) . zip points $ bytes points

search :: [(Point, Maybe Int)] -> Point
search xs = go 0 (length xs - 1)
  where
    go lo hi
      | lo == hi = fst (xs !! lo)
      | isNothing (snd (xs !! mid)) = go lo mid
      | otherwise = go (mid + 1) hi
      where
        mid = (lo + hi) `div` 2

bytes :: [Point] -> [Maybe Int]
bytes points = bit (end points) <$> (drop 1 . inits) points

bit :: Point -> [Point] -> Maybe Int
bit end = dijkstra (0, 0) end . bitmap end

dijkstra :: Point -> Point -> Map Point Int -> Maybe Int
dijkstra start end points = step initialHeap initialDists S.empty
  where
    initialHeap = H.singleton (0, start)
    initialDists = M.insert start 0 (maxBound <$ points)
    toHeap = H.fromList . map swap . M.toList
    step heap dists seen = do
      ((dist, point), heap') <- H.uncons heap
      case point of
        _ | point == end -> return dist
        _ | S.member point seen -> step heap' dists seen
        _ -> step (toHeap better <> heap') (better <> dists) (S.insert point seen)
          where
            better = M.differenceWith pick (neighbors point dists) dists
            pick inc dist' = do
              guard (dist + inc <= dist')
              return (dist + inc)

-- neighbors :: Point -> Map Point Int -> Map Point Int
neighbors point grid = M.fromList $ mapMaybe (\a -> (a, 1) <$ M.lookup a grid) $ neighborPoints point

neighborPoints (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]

bitmap :: Point -> [Point] -> M.Map Point Int
bitmap (mx, my) points = M.fromList [((x, y), 0) | x <- [0 .. mx], y <- [0 .. my], (x, y) `notElem` points]

end = bool (6, 6) (70, 70) . any ((> 7) . fst)

steps = bool 11 1023 . any ((> 7) . fst)

parse :: String -> [(Int, Int)]
parse = (bimap read (read . drop 1) . span (/= ',') <$>) . lines