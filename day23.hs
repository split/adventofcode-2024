module Main where

import Data.List (intercalate, isPrefixOf, nub, tails, maximumBy)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)
import Data.Ord (comparing)

type Vertex = String

type Edge = (Vertex, Vertex)

type Graph = [Edge]

main = interact (unlines . sequence [part1, part2] . undirected . parse)

part1, part2 :: Graph -> String
part1 = ("Part 1: " ++) . show . (`div` 3) . length . filter (any ("t" `isPrefixOf`)) . setsOfThree
part2 = ("Part 2: " ++) . password . (map . clique <*> vertices)

clique :: Graph -> Vertex -> S.Set Vertex
clique graph = flip go S.empty
  where
    get = S.fromList . getVertices graph
    go v cq
      | cq `S.isSubsetOf` get v = foldr go (S.insert v cq) (get v)
      | otherwise = cq

setsOfThree graph = mapMaybe go . concatMap trie $ vertices graph
  where
    get = getVertices graph
    trie v = (v,) <$> pairs (get v)
    go (v1, (v2, v3))
      | v2 `elem` get v3 = return [v1, v2, v3]
      | otherwise = Nothing

getVertices :: Graph -> Vertex -> [Vertex]
getVertices =
  flip (M.findWithDefault [])
    . M.fromListWith (++)
    . map ((: []) <$>)

undirected :: Graph -> Graph
undirected = (>>= \x -> [x, swap x])

vertices :: Graph -> [Vertex]
vertices = nub . (fst <$>)

password = intercalate "," . S.elems . maximumBy (comparing S.size)

pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

parse = map ((drop 1 <$>) . span (/= '-')) . lines