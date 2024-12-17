{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bits (xor)
import Data.Char (isDigit)
import Data.List (intercalate)

data Mem = Mem
  { ptr :: Int,
    regA :: Int,
    regB :: Int,
    regC :: Int,
    program :: [Int],
    output :: [Int]
  }
  deriving (Show)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . intercalate "," . map show . output . runProgram

runProgram :: Mem -> Mem
runProgram mem = case readMem mem of
  Just (inst, mem') -> runProgram (runInst mem' inst)
  Nothing -> mem

runInst :: Mem -> (Int, Int) -> Mem
runInst mem@(Mem {..}) (opcode, literal) =
  let combo = mem `readCombo` literal
   in case opcode of
        0 -> mem {regA = regA `div` (2 ^ combo)} -- adv
        1 -> mem {regB = regB `xor` literal} -- bxl
        2 -> mem {regB = combo `mod` 8} -- bst
        3 -> if regA == 0 then mem else mem {ptr = literal} -- jnz
        4 -> mem {regB = regB `xor` regC} -- bxc
        5 -> mem {output = output ++ [combo `mod` 8]} -- out
        6 -> mem {regB = regA `div` (2 ^ combo)} -- bdv
        7 -> mem {regC = regA `div` (2 ^ combo)} -- cdv
        _ -> error "Invalid opcode"

readMem :: Mem -> Maybe ((Int, Int), Mem)
readMem mem@(Mem {..})
  | ptr < length program = Just ((program !! ptr, program !! (ptr + 1)), mem {ptr = ptr + 2})
  | otherwise = Nothing

readCombo :: Mem -> Int -> Int
readCombo (Mem {..}) literal = case literal of
  4 -> regA
  5 -> regB
  6 -> regC
  7 -> error "Invalid operand"
  o -> o

parse :: String -> Mem
parse = (\(a : b : c : program) -> Mem 0 a b c program []) . digits

digits :: String -> [Int]
digits input
  | null input = []
  | null x = digits (drop 1 xs)
  | otherwise = read x : digits xs
  where
    (x, xs) = span isDigit input
