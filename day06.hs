module Main where

import qualified Data.Map as M
import Data.Foldable (find)
import Data.List (unfoldr, nub)

type Vec = (Int, Int)
type Guard = Maybe (Vec, Vec)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . length . nub . uncurry walkAll

walkAll grid start = case start of
    Just first -> fst first : unfoldr (fmap (\state -> (fst state, Just state)) . walk grid) start

walk :: M.Map Vec Char -> Guard -> Guard
walk grid guard = do
    (pos, dir) <- guard
    c <- grid M.!? move pos dir
    case c of
        '#' -> return (move pos (right dir), right dir)
        _   -> return (move pos dir, dir)

right (x, y) = (-y, x)
move (x, y) (dx, dy) = (x + dx, y + dy)


parse :: String -> (M.Map Vec Char, Guard)
parse input = (M.fromList grid, start)
    where
        grid = [((x, y), c) | (l, y) <- zip (lines input) [0..], (c, x) <- zip l [0..]]
        start = fmap (const (0, -1)) <$> find ((== '^') . snd) grid