{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.Char (isDigit)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.Map qualified as M


data Robot = Robot {p :: (Int, Int), v :: (Int, Int)} deriving (Show, Eq, Ord)
data Tiles = Tiles {w :: Int, h :: Int} deriving (Show)

main = interact (unlines . sequence [uncurry part1] . parse)

part1 :: Tiles -> [Robot] -> String
part1 tiles = ("Part 1: " ++) . show . product . M.map length . quadrants tiles . map (move tiles 100)


quadrants :: Tiles -> [Robot] -> M.Map Int [Robot]
quadrants (Tiles {..}) = M.fromListWith (++) . mapMaybe quadrant
    where
        quadrant r = do
            x <- divide w (fst r.p)
            y <- divide h (snd r.p)
            return (2 * y + x, [r])

divide :: Int -> Int -> Maybe Int
divide size v
    | v == mid = Nothing
    | otherwise = return (v `div` (mid + 1))
    where
        mid = size `div` 2

move :: Tiles -> Int -> Robot -> Robot
move (Tiles {..}) t r = r { p = (dx, dy) }
    where
        teleport upperBound v = v `mod` upperBound
        dx = teleport w (fst r.p + t * fst r.v)
        dy = teleport h (snd r.p + t * snd r.v)

parse :: String -> (Tiles, [Robot])
parse = mkTiles . map mkRobot . lines
    where
        vec = bimap (read . drop 2) (read . tail) . span (/= ',')
        mkRobot input = let (p : v : _) = words input in
            Robot (vec p) (vec v)
        mkTiles robots
            | any ((> 11) . fst . p) robots = (Tiles { w = 101, h = 103 }, robots)
            | otherwise = (Tiles { w = 7, h = 11 }, robots)