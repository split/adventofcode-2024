module Main where

import Data.Char (digitToInt)
import Data.Either
import Data.Sequence (Seq, ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import Data.Sequence qualified as Seq

main = interact (unlines . sequence [part1] . head . map (map digitToInt) . lines)

part1 :: [Int] -> String
part1 = ("Part 1: " ++) . show . checksum . pack

checksum = sum . zipWith (*) [0 ..]

pack :: [Int] -> [Int]
pack = consume . block 0
  where
    block i n = case n of
      [] -> Seq.fromList []
      (x : xs) -> Seq.fromList (replicate x (Right i)) >< space i xs

    space i n = case n of
      [] -> Seq.fromList []
      (x : xs) -> Left x <| block (i + 1) xs

    consume n = case viewl n of
      EmptyL -> []
      (Right x :< xs) -> x : consume xs
      (Left x :< xs) -> let (blocks, xs') = popBack xs x in blocks ++ consume xs'

popBack :: Seq (Either a b) -> Int -> ([b], Seq (Either a b))
popBack b = go ([], b)
  where
    go (b, rb) n = case viewr rb of
      EmptyR -> (b, rb)
      _ | n == 0 -> (b, rb)
      xs :> Right x -> go (b ++ [x], xs) (n - 1)
      xs :> x -> (|> x) <$> go (b, xs) n