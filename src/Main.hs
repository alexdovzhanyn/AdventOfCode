module Main (main) where

import qualified Y2023.Day1.SolutionPart1 as D1P1

main :: IO ()
main = do
  putStrLn "---------------------------"
  putStrLn "    Advent of Code 2023"
  putStrLn "---------------------------"
  d1p1Solution <- D1P1.getSolution
  putStrLn ("Day 1, part 1 solution is: " ++ show d1p1Solution)
