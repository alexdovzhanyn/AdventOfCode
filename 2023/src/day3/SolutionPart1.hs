--- Day 3: Gear Ratios ---
-- You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.
-- 
-- It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.
-- 
-- "Aaah!"
-- 
-- You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.
-- 
-- The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.
-- 
-- The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
-- 
-- Here is an example engine schematic:
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
-- In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
-- 
-- Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

module Day3.SolutionPart1 (getSolution) where
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
  then collected ++ (map (parsePartNums fileLines) [(lineNum - 1, i), (lineNum + 1, i), (lineNum, i)])
  else collected

parsePartNums :: [String] -> (Int, Int) -> Int
parsePartNums fileLines (lineNum, i)
  | lineNum < 0 || lineNum > length fileLines || i < 0 || i > lineLength = 0
  | otherwise = 
    let charAtTarget = (fileLines !! lineNum) !! i
        (leftOfTarget, rightOfTarget) = splitAt i (fileLines !! lineNum)
        left = reverse (takeWhile isDigit (reverse leftOfTarget))
        right = takeWhile isDigit (drop 1 rightOfTarget)
    in if isDigit charAtTarget
      then parseNum (left ++ [charAtTarget] ++ right)
      else parseNum left + parseNum right
  where lineLength = length $ fileLines !! 0

parseNum :: [Char] -> Int
parseNum [] = 0
parseNum n = read n

checkIsSymbol :: [Char] -> Char -> Bool
checkIsSymbol symbols c = c `elem` symbols

