module Main where

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: [[Int]] -> String
part1 = ("Part 1: " ++) . show . calibration . filter (solve [(+), (*)])
part2 = ("Part 2: " ++) . show . calibration . filter (solve [(+), (*), cc])

calibration = sum . concatMap (take 1)

cc x y = x * 10 ^ length (show y) + y

solve ops (target : coeff) = go 0 coeff
  where
    go sum f = case f of
      [] -> sum == target
      (x : xs) -> any (\op -> go (sum `op` x) xs) ops

parse :: String -> [[Int]]
parse = map (map read . words . filter (/= ':')) . lines