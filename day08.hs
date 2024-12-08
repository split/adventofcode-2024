module Main where
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad (ap)

main = interact (unlines . sequence [part1] . scan)

part1 = ("Part 1: " ++) . show . length . antinodes

antinodes = inMap (S.unions . S.map (uncurry offset) . pairs)
    where
        pairs = S.filter (uncurry (/=)) . (S.cartesianProduct <*> id)
        inMap f = ap (S.intersection . S.unions) (S.unions . M.map f . M.delete '.')
        offset (x, y) (x', y') = S.singleton (x + (x - x'), y + (y - y'))

scan input = M.fromListWith S.union [(c, S.singleton (x, y)) | (l, y) <- zip (lines input) [0..], (c, x) <- zip l [0..]]