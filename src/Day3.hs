module Day3 where

import Data.Char (digitToInt)

run :: IO ()
run = do
  input <- readFile "inputs/day3.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 lns = sum $ map maxJoltage (banks lns)

part2 :: [String] -> Int
part2 lns = sum $ map (maxNJoltage 12) (banks lns)

joltDigits :: (Maybe Int, Maybe Int) -> [Int] -> (Int, Int)
joltDigits (Nothing, Nothing) xs = joltDigits (Just (maximum $ init xs), Nothing) xs
joltDigits (Just x, Nothing) xs = (x, maximum (tail $ dropWhile (/= x) xs))
joltDigits _ _ = (0, 0)

-- | Takes an size var and input int array to determine the maximum 
-- retrievable value within the list
--
-- Arguments
-- * @n@: indicates the nth digit to be found. This will exclude n values
-- from the end of the list
-- * @xs@: list to perform operation
--
-- Returns
-- * Maximum returnable digit
nDigit :: Int -> [Int] -> Int
nDigit n xs = maximum $ take (length xs - n + 1) xs 

joltNDigits :: Int -> [Int] -> [Int]
joltNDigits n xs
  | length xs < n = []
  | otherwise = go n [] xs
    where
      go :: Int -> [Int] -> [Int] -> [Int]
      go 0 ys _ = reverse ys
      go n' ys xs' = do
        let d = nDigit n' xs'
        go (n'-1) (d:ys) (tail $ dropWhile (/=d) xs')

maxJoltage :: [Int] -> Int
maxJoltage xs = (\(x, y) -> x * 10 + y) (joltDigits (Nothing, Nothing) xs)

maxNJoltage :: Int -> [Int] -> Int
maxNJoltage n xs = read $ concatMap show $ joltNDigits n xs

parseLine :: String -> [Int]
parseLine = map digitToInt

banks :: [String] -> [[Int]]
banks = map parseLine
