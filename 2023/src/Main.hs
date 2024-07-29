module Main (main) where

import Control.Monad (forM_)
import qualified Day1.SolutionPart1 as D1P1
import qualified Day1.SolutionPart2 as D1P2
import qualified Day2.SolutionPart1 as D2P1
import qualified Day2.SolutionPart2 as D2P2
import qualified Day3.SolutionPart1 as D3P1
import qualified Day3.SolutionPart2 as D3P2

solutions :: [[IO Int]]
solutions = [
    [ D1P1.getSolution, D1P2.getSolution ],
    [ D2P1.getSolution, D2P2.getSolution ],
    [ D3P1.getSolution, D3P2.getSolution ]
  ]


main :: IO ()
main = do
  putStrLn "---------------------------"
  putStrLn "    Advent of Code 2023"
  putStrLn "---------------------------"

  forM_ (zip [1..] solutions) $ outputSolutionParts


outputSolutionParts :: (Int, [IO Int]) -> IO ()
outputSolutionParts (dayNum, parts) = do
  forM_ (zip ([1..] :: [Int]) parts) $ \(partNum, solution) -> do
    sol <- solution
    putStrLn ("Day " ++ show dayNum ++ ", part " ++ show partNum ++ " solution is: " ++ show sol)
