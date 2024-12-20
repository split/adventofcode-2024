module Main where

import Control.Monad (guard)
import qualified Data.Heap as H
import Data.List (find, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

type Point = (Int, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Map Point Char -> String
part1 = ("Part 1: " ++) . maybe "Not found" show . race 2
part2 = ("Part 2: " ++) . maybe "Not found" show . race 20

race :: Int -> Map Point Char -> Maybe Int
race maxSkip track = do
  let trackValues = swap <$> M.toList track
  start <- lookup 'S' trackValues
  end <- lookup 'E' trackValues
  (dist, path, dists) <- dijkstra start end track
  let cheatDists = zipWith (\se p -> cheat se p dists) [0 ..] path
   in return (length $ concat cheatDists)
  where
    -- cheat :: (Int, Point) -> Map Point Int -> [Int]
    cheat skipEnd point = filter (<= save) . sort . M.elems . skips
      where
        skips = M.differenceWithKey check (cheatRadius maxSkip point)
        save = if M.size track < 100 then (-1) else (-100)
        check p skipLen skipStart = do
          return $ skipStart - skipEnd + skipLen

cheatRadius :: Int -> Point -> Map Point Int
cheatRadius maxDist (x, y) =
  M.fromList
    [ ((x + dx, y + dy), dist)
      | dx <- [-maxDist .. maxDist],
        dy <- [-maxDist .. maxDist],
        let dist = abs dx + abs dy,
        dist <= maxDist
    ]

dijkstra start end points = step initialHeap initialDists M.empty
  where
    initialHeap = H.singleton (0, start)
    initialDists = M.insert start 0 ((maxBound :: Int) <$ points)
    toHeap = H.fromList . map swap . M.toList
    step heap dists pred = do
      ((dist, point), heap') <- H.uncons heap
      case point of
        _ | point == end -> return (dist, reverse $ backtrack pred end, dists)
        _ -> step (toHeap better <> heap') (better <> dists) pred'
          where
            pred' = pred <> M.map (const point) better
            better = M.differenceWith pick (neighbors point dists) dists
            pick inc dist' = do
              guard (dist + inc <= dist')
              return (dist + inc)

backtrack :: Map Point Point -> Point -> [Point]
backtrack preds end = end : maybe [] (backtrack preds) (end `M.lookup` preds)

-- neighbors :: Point -> Map Point Int -> Map Point Int
neighbors point grid = M.fromList $ mapMaybe (\a -> (a, 1) <$ M.lookup a grid) $ neighborPoints point

neighborPoints (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]

parse :: String -> Map Point Char
parse i = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] (lines i), (x, c) <- zip [0 ..] l, c /= '#']
