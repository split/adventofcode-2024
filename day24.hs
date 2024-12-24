module Main where

import Control.Applicative qualified as Map
import Control.Monad (msum)
import Data.Bifunctor (bimap)
import Data.Bits (Bits, xor, (.&.), (.|.))
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)

data Op = AND | OR | XOR deriving (Enum, Eq, Read, Show)

type Gates = [(String, Op, String, String)]

type Mem = M.Map String Int

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . toDec . uncurry runGates

runGates :: Mem -> Gates -> Mem
runGates initialMem gates = go initialMem gates
  where
    go mem [] = mem
    go mem (g : gx) =
      maybe (go mem (gx ++ [g])) (`go` gx) (step mem g)

    step mem (x', op, y', z') = do
      x <- mem M.!? x'
      y <- mem M.!? y'
      return $ M.insert z' (runOp x op y) mem

runOp :: (Bits a) => a -> Op -> a -> a
runOp x op y = case op of
  AND -> x .&. y
  OR -> x .|. y
  XOR -> x `xor` y

toDec = foldr go 0 . M.toList
  where
    go  ((k : _), bit) acc
      | k == 'z' = acc * 2 + bit
      | otherwise = acc

parse = bimap (M.fromList . map value) (map gate) . separate
  where
    value = bimap init read . toTuple
    gate (x : op : y : _ : z : _) = (x, read op, y, z)
    separate = toTuple . map (map words . lines) . splitOn "\n\n"
    toTuple (a : b : _) = (a, b)