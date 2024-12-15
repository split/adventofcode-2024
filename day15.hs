module Main where

import Control.Monad (guard)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

type Pos = (Int, Int)

type Warehouse = M.Map Pos Char

type Command = Char

main = interact (unlines . sequence [part1 . parse])

part1 :: ((Warehouse, Pos), [Command]) -> [Char]
part1 = ("Part 1: " ++) . show . sum . gps . uncurry simulate

gps = map (uncurry (+) . ((* 100) <$>)) . M.keys . M.filter (== 'O')

simulate :: (Warehouse, Pos) -> [Command] -> Warehouse
simulate = (fst .) . foldl (\acc c -> fromMaybe acc (move acc c))

move :: (Warehouse, Pos) -> Command -> Maybe (Warehouse, Pos)
move (wh, pos) c = do
  f <- wh M.!? pos
  guard (f /= '.')
  let pos' = newPos pos c
  let wh' = maybe wh fst (move (wh, pos') c)
  t <- wh' M.!? pos'
  guard (t == '.')
  return (M.insert pos' f $ M.insert pos '.' wh', pos')

newPos (x, y) c = case c of
  '>' -> (x + 1, y)
  '<' -> (x - 1, y)
  '^' -> (x, y - 1)
  'v' -> (x, y + 1)

parse :: String -> ((Warehouse, Pos), [Command])
parse input = ((warehouse, start warehouse), concat $ lines commands)
  where
    [state, commands] = splitOn "\n\n" input
    warehouse = M.fromList [((x, y), c) | (y, row) <- zip [0 ..] (lines state), (x, c) <- zip [0 ..] row, c /= '#']
    start = (!! 0) . M.keys . M.filter (== '@')

draw :: Warehouse -> String
draw wh = unlines [[M.findWithDefault '#' (x, y) wh | x <- [0 .. 8]] | y <- [0 .. 8]] ++ "\n\n"