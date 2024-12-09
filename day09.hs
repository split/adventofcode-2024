module Main where

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Either
import Data.Foldable (foldr, toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as S

main = interact (unlines . sequence [part1, part2] . (!! 0) . map (map digitToInt) . lines)

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . checksum . unpack . moveBlock . pack
part2 = ("Part 2: " ++) . show . checksum . unpack . moveFile . pack

checksum = sum . zipWith (*) [0 ..]

pack :: [Int] -> Seq (Either Int (Int, Int))
pack = block 0
  where
    block i n = case n of
      [] -> Seq.empty
      (x : xs) -> Right (x, i) <| space i xs

    space i n = case n of
      [] -> Seq.empty
      (x : xs) -> Left x <| block (i + 1) xs

unpack :: Seq (Either Int (Int, Int)) -> [Int]
unpack = concatMap (either (`replicate` 0) (uncurry replicate))

moveBlock :: Seq (Either Int (Int, Int)) -> Seq (Either Int (Int, Int))
moveBlock drive = case viewl drive of
  EmptyL -> Seq.empty
  (Right b :< xs) -> Right b <| moveBlock xs
  (Left x :< xs) -> let (blocks, xs') = popBack xs x in blocks >< moveBlock xs'

moveFile :: Seq (Either Int (Int, Int)) -> Seq (Either Int (Int, Int))
moveFile drive = foldr (\block -> fromMaybe <*> move block) drive blocks
  where
    blocks = rights (toList drive)
    space n = if n > 0 then Seq.singleton (Left n) else Seq.empty
    move block@(x, i) xs = do
      from <- Seq.findIndexR (== Right block) xs
      let xs' = replaceAt from (space x) xs
      to <- Seq.findIndexL ((>= x) . fromLeft 0) xs'
      guard (from > to)
      (Left x') <- xs Seq.!? to
      return $ replaceAt to (Right block <| space (x' - x)) xs'

replaceAt :: Int -> Seq a -> Seq a -> Seq a
replaceAt i new xs = Seq.take i xs >< new >< Seq.drop (i + 1) xs

popBack b = go (Seq.empty, b)
  where
    go (b, rb) n = case viewr rb of
      EmptyR -> (b, rb)
      _ | n == 0 -> (b, rb)
      xs :> Right (x, i) -> case x - n of
        x' | x' > 0 -> (b |> Right (n, i), xs |> Right (x', i))
        x' -> go (b |> Right (x, i), xs) (n - x)
      xs :> x -> (|> x) <$> go (b, xs) n