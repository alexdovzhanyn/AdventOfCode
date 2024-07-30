--- Part Two ---
-- The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.
-- 
-- You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.
-- 
-- Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.
-- 
-- The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.
-- 
-- This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.
-- 
-- Consider the same engine schematic again:
-- 
-- 467..114..
-- ...*......
-- ..35..633.
-- ......#...
-- 617*......
-- .....+.58.
-- ..592.....
-- ......755.
-- ...$.*....
-- .664.598..
-- In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.

module Day3.SolutionPart2 (getSolution) where
import Data.Char (isAlpha, isDigit)
import Data.List (nub)

getSolution :: IO Int
getSolution = do
  fileContents <- readFile "inputs/day3.txt"
  
  let fileContentLines = lines fileContents
  let symbols = nub [c | c <- fileContents, not . isAlpha $ c, not . isDigit $ c, c /= '.', c /= '\n']

  let findConnected = findConnectedPartNumbers (checkIsSymbol symbols) fileContentLines

  pure $ sum $ map findConnected (zip [0..] fileContentLines)

findConnectedPartNumbers :: (Char -> Bool) -> [String] -> (Int, String) -> Int
findConnectedPartNumbers isSymbol fileLines (lineNum, line) = sum $ foldl collectPartNums [] (zip line [0..]) 
  where collectPartNums = collectConnectedPartNums isSymbol fileLines lineNum

collectConnectedPartNums :: (Char -> Bool) -> [String] -> Int -> [Int] -> (Char, Int) -> [Int]
collectConnectedPartNums isSymbol fileLines lineNum collected (c, i) = 
  if isSymbol c 
  then let partConnections = (concat $ map (parsePartNums fileLines) [(lineNum - 1, i), (lineNum + 1, i), (lineNum, i)])
    in case partConnections of
      [] -> collected
      [a, b] -> collected ++ [a * b]
      _ -> collected
  else collected

parsePartNums :: [String] -> (Int, Int) -> [Int]
parsePartNums fileLines (lineNum, i)
  | lineNum < 0 || lineNum > length fileLines || i < 0 || i > lineLength = []
  | otherwise = 
    let charAtTarget = (fileLines !! lineNum) !! i
        (leftOfTarget, rightOfTarget) = splitAt i (fileLines !! lineNum)
        left = reverse (takeWhile isDigit (reverse leftOfTarget))
        right = takeWhile isDigit (drop 1 rightOfTarget)
    in if isDigit charAtTarget
      then [ parseNum (left ++ [charAtTarget] ++ right) ]
      else filter (/=0) [ parseNum left, parseNum right]
  where lineLength = length $ fileLines !! 0

parseNum :: [Char] -> Int
parseNum [] = 0
parseNum n = read n

checkIsSymbol :: [Char] -> Char -> Bool
checkIsSymbol symbols c = c `elem` symbols

