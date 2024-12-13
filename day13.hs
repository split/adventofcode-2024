module Main where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

data Machine = Machine {a :: (Int, Int), b :: (Int, Int), prize :: (Int, Int)} deriving (Show)

main = interact (unlines . sequence [part1, part2] . parse)

e = 10000000000000

part1, part2 :: [Machine] -> String
part1 = ("Part 1: " ++) . show . sum . map spend
part2 = ("Part 2: " ++) . show . sum . map (spend . conversionError)

spend m = if valid then y * 3 + x else 0
  where
    valid = (ax * y + bx * x, ay * y + by * x) == prize m
    y = (pzy * bx - by * pzx) `div` (ay * bx - by * ax)
    x = (pzx - ax * y) `div` bx
    (pzx, pzy) = prize m
    (ax, ay) = a m
    (bx, by) = b m

conversionError m = m {prize = move (prize m) (e, e)}

parse :: String -> [Machine]
parse = map (mkMachine . map (point . map clean <$> splitOn ", ") <$> lines) . splitOn "\n\n"
  where
    mkMachine [a, b, prize] = Machine a b prize
    clean = read . dropWhile (not . isDigit)
    point [x, y] = (x, y)

move (x, y) (dx, dy) = (x + dx, y + dy)
