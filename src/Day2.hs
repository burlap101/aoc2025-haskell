module Day2 where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

run :: IO ()
run = do
  input <- readFile "inputs/day2.txt"
  let lns = splitOn "," input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 = sum . invalidIds . concatMap numList . parsedLines

part2 :: [String] -> Int
part2 = sum . invalidIds2 . concatMap numList . parsedLines

splitAtDash :: String -> Maybe (String, String)
splitAtDash s =
  case break (== '-') s of
    (a, '-' : b) -> Just (a, b)
    _ -> Nothing

parsedLines :: [String] -> [(Int, Int)]
parsedLines = mapMaybe parseLine

parseLine :: String -> Maybe (Int, Int)
parseLine s =
  case splitAtDash s of
    Just (a, b) -> Just (read a :: Int, read b :: Int)
    _ -> Nothing

digitCount :: Int -> Int
digitCount = length . show

-- | Produces list of digits comparisons to perform for n length
--
-- Arguments
-- * @n@: the number of digits to take per chunk
-- * @s@: string representation of the original number
--
-- Returns:
-- * Sets of character chunks over the length of the number string
digitComparisonOperands :: Int -> String -> [[String]]
digitComparisonOperands n s
  | n <= 0 = []
  | otherwise = [digitsForString i s | i <- [0 .. length s]]

-- | Produces sets of chunks of strings per chunk size
digitsForString :: Int -> String -> [String]
digitsForString n s
  | n <= 0 = []
  | length s < n = []
  | length s `mod` n /= 0 = []
  | otherwise = [take n (drop i s) | i <- [0, n .. length s - n]]

-- | Produces a list of lists of strings for each lenghth of comparison operands possible
--
-- Arguments
-- * @n@: The number undergoing validity scrutiny
--
-- Returns:
-- * nested list of operands, each list containing string lengths from 1 to half the length
-- of the string representation of @n@
allDigitComparisons :: Int -> [[String]]
allDigitComparisons n = [digitsForString i (show n) | i <- [1 .. digitCount n]]

-- | List of tuples containing the results of determining an id is invalid
--
-- Arguments
-- * @ns@: ids to be tested
--
-- Returns
-- * list of tuples, same length as ns, the first member being whether the id was invalid
-- and the second is the number itself
idResults :: [Int] -> [(Bool, Int)]
idResults ns = zip (map repeats ns) ns

-- | Reduce style function to determine if a list of strings has
-- two consecutive elements the same
--
-- Arguments
-- * Bool: Initial and accumulitive state variable
-- * [String]: strings to be compared
--
-- Returns
-- * Whether a repeat was found.
hasRepeat :: Bool -> [String] -> Bool
hasRepeat True _ = True
hasRepeat False (x : y : rest) = hasRepeat (x == y) (y : rest)
hasRepeat _ _ = False

isRepeatsOnly :: [String] -> Bool
isRepeatsOnly [] = False
isRepeatsOnly [_] = False
isRepeatsOnly (x:xs) = all (==x) xs

-- | Determines if a number has a repeat in it (i.e. is invalid)
--
-- Arguments
-- * @n@: id to be tested
--
-- Returns
-- * Whether a repeat is found
repeats :: Int -> Bool
repeats n = or [isRepeatsOnly y | y <- allDigitComparisons n]

-- Determines if a supplied arg has an even character count
hasEvenLength :: Int -> Bool
hasEvenLength = even . digitCount

-- Produces list of all nums that have an even amount of characters
evenLengthNums :: [Int] -> [Int]
evenLengthNums = filter hasEvenLength

-- Determines if a supplied number is a doubled
isDoubledNum :: Int -> Bool
isDoubledNum n = do
  let l = digitCount n
  let (a, b) = splitAt (l `div` 2) (show n)
  a == b

-- List producer
numList :: (Int, Int) -> [Int]
numList (m, n) = [m .. n]

invalidIds :: [Int] -> [Int]
invalidIds = filter isDoubledNum . evenLengthNums

invalidIds2 :: [Int] -> [Int]
invalidIds2 = map snd . filter fst . idResults
