module Main where

import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)

type Garden = M.Map (Int, Int) Char
type Area = [(Int, Int)]

data Plot = Plot { plant :: Char, area :: Area, perimeter :: Int } deriving (Show)

main = interact (unlines . sequence [part1] . parse)

part1 :: Garden -> String
part1 = ("Part 1: " ++) . show . sum . map price . plots
price :: Plot -> Int
price plot = length (area plot) * perimeter plot

plots :: Garden -> [Plot]
plots g = go [(0, 0)] []
    where
        go [] plots = plots
        go (p:ps) plots
            | any ((p `elem`) . area) plots = go ps plots
            | otherwise = fromMaybe (go ps plots) $ do
                (plot, outer) <- growPlot g p
                return $ go (ps ++ outer) (plot:plots)

growPlot :: Garden -> (Int, Int) -> Maybe (Plot, Area)
growPlot g start = do
        plot <- initPlot start
        return (grow [start] [] plot)
    where
        initPlot :: (Int, Int) -> Maybe Plot
        initPlot start = do
            plant <- g M.!? start
            return (Plot plant [] 0)

        grow :: [(Int, Int)] -> [(Int, Int)] -> Plot -> (Plot, Area)
        grow [] outer plot = (plot, outer)
        grow (p:ps) outer plot
            | p `elem` area plot = grow ps outer plot
            | otherwise = case g M.!? p of
                Just plant' | plant' == plant plot -> 
                    grow (ps ++ map (move p) dirs) outer (plot { area = p : area plot })
                Just plant' -> 
                    grow ps (p:outer) (plot { perimeter = perimeter plot + 1 })
                Nothing ->
                    grow ps outer (plot { perimeter = perimeter plot + 1 })
dirs = [(1, 0), (0, -1), (-1, 0), (0, 1)]
move (x, y) (dx, dy) = (x + dx, y + dy)

parse input = M.fromList [((x, y), c) | (y, l) <- zip [0..] (lines input), (x, c) <- zip [0..] l]
