--- Part Two ---
-- Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
-- 
-- Equipped with this new information, you now need to find the real first and last digit on each line. For example:

module Y2023.Day1.SolutionPart2 (getSolution) where

import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

getSolution :: IO Int
getSolution = do
  fileContents <- readFile "src/y2023/day1/input.txt"

  return $ sum $ map parseCalibrationFromLine (lines fileContents)

parseCalibrationFromLine :: String -> Int
parseCalibrationFromLine line = do
  let digits = getDigits line 

  read [head digits, last digits]

getDigits :: String -> [Char]
getDigits line
  | line == "" = []
  | isDigit $ head line = head line : getDigits (drop 1 line)
  | length matchedWords > 0 =
    let word = head matchedWords
        digit = fromJust (Map.lookup word digitByWordMap)
    in digit : getDigits (drop 1 line)
  | otherwise = getDigits $ drop 1 line 
  where matchedWords = filter (\word -> isPrefixOf word line) numericalWords

numericalWords :: [String]
numericalWords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitByWordMap :: Map.Map String Char
digitByWordMap = Map.fromList $ zip numericalWords ['0'..] 


