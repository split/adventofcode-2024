module Main where

import Control.Monad (guard, when)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

type Pos = (Int, Int)

type Warehouse = M.Map Pos Char

type Command = Char

main = interact (unlines . sequence [part1 . parse, part2 . parse . scaleUp])

part1, part2 :: ((Warehouse, Pos), [Command]) -> String
part1 = ("Part 1: " ++) . show . sum . gps . uncurry simulate
part2 = ("Part 2: " ++) . show . sum . gps . uncurry simulate

gps = map (uncurry (+) . ((* 100) <$>)) . M.keys . M.filter (`elem` "O[")

simulate :: (Warehouse, Pos) -> [Command] -> Warehouse
simulate = (fst .) . foldl (\acc c -> fromMaybe acc (move acc c))

move :: (Warehouse, Pos) -> Command -> Maybe (Warehouse, Pos)
move state c = go state
  where
    go (wh, pos) = do
      from <- wh M.!? pos
      let pos' = newPos pos c
      wh' <- attempt (wh, pos')
      return (M.insert pos' from $ M.insert pos '.' wh', pos')

    attempt (wh, pos') = do
      to <- wh M.!? pos'
      let branch dir = do (wh', _) <- go (wh, pos'); go (wh', newPos pos' dir)
      fst <$> case to of
          '.' -> return (wh, pos')
          '[' | c `elem` "^v" -> branch '>'
          ']' | c `elem` "^v" -> branch '<'
          _ -> go (wh, pos')

newPos :: (Num a, Num b) => (a, b) -> Char -> (a, b)
newPos (x, y) c = case c of
  '>' -> (x + 1, y)
  '<' -> (x - 1, y)
  '^' -> (x, y - 1)
  'v' -> (x, y + 1)

scaleUp = concatMap go
  where
    go '#' = "##"
    go 'O' = "[]"
    go '.' = ".."
    go '@' = "@."
    go c = [c]

parse :: String -> ((Warehouse, Pos), [Command])
parse input = ((warehouse, start warehouse), concat $ lines commands)
  where
    [state, commands] = splitOn "\n\n" input
    warehouse = M.fromList [((x, y), c) | (y, row) <- zip [0 ..] (lines state), (x, c) <- zip [0 ..] row, c /= '#']
    start = (!! 0) . M.keys . M.filter (== '@')