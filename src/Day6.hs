module Day6 where

import Data.List (foldl1', transpose)
import qualified Data.Text as T

run :: IO ()
run = do
  input <- readFile "inputs/day6.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 lns = sum $ map solveProblem $ problems lns

part2 :: [String] -> Int
part2 lns = sum $ map solveProblem $ problemsPart2 lns

solveProblem :: ([Int], Char) -> Int
solveProblem (rs, '+') = foldl1' (+) rs
solveProblem (rs, '*') = foldl1' (*) rs
solveProblem (_, op) = error ("unknown op: " ++ show op)

-- | Convert lines of integer strings into list of list of integers
--
-- Arguments
-- * @lns@ - integer string lines e.g. ["1 2 3", "4 5 6"]
--
-- Returns
-- * List of list of integers e.g. [[1, 2, 3], [4, 5, 6]]
numLines :: [String] -> [[Int]]
numLines lns = go lns [[]]
  where
    go :: [String] -> [[Int]] -> [[Int]]
    go [] acc = acc
    go (sn : sns) acc = go sns (map read (words sn) : acc)

-- | Construct problems from input lines
--
-- Arguments
-- * @lns@ - all string lines from input
--
-- Returns
-- * list of tuples, first element numbers to perform operation over, and second operator type char
problems :: [String] -> [([Int], Char)]
problems lns = do
  let nls = transpose $ numLines $ init lns
  let ops = map head $ words $ last lns
  [(x, y) | (x, y) <- zip nls ops]

-- ============ Part2 Specific ==============

-- | For a given list of integers calculate each's digit size
populateSize :: [Int] -> [Int]
populateSize = map (length . show) 

-- | Determine the sizes of each grid across supplied line of integers
--
-- Arguments
-- * @is@ - resulting `numlines` output
--
-- Returns
-- * List of number for a given column's highest digit count
gridSizes :: [[Int]] -> [Int]
gridSizes is = go (transpose is) []
  where
    go :: [[Int]] -> [Int] -> [Int]
    go [] acc = reverse acc
    go (i':is') acc = go is' (maximum (populateSize i'):acc)

-- | Gets ops from a line containing them
--
-- Arguments
-- * @ln@ - op string
--
-- Returns
-- * List of op characters
opsList :: String -> [Char]
opsList os = [head o | o <- words os]

-- | Splits out the next grid of chars from an array of num strings
--
-- Arguments
-- * @lns@ - list of num strings
-- * @n@ - number of characters to take each line
--
-- Returns
-- * List of characters to transform into numbered grid columnar and remaining strings list
charGrid :: Int -> [String] -> ([[Char]], [String])
charGrid n lns = unzip ([ (take n l, drop (n+1) l) | l <- lns ])

charGrids :: [String] -> [[[Char]]]
charGrids lns = go lns (gridSizes $ numLines lns) []
  where
  go :: [String] -> [Int] -> [[[Char]]] -> [[[Char]]]
  go _ [] acc = reverse acc
  go ls (n:ns) acc = (\(cg, lns') -> go lns' ns (cg : acc)) (charGrid n ls)

-- | Takes a character grid and converts its columns into numbers
--
-- Arguments
-- * @cg@ - character grid
--
-- Returns
-- * List of numbers to then use in a problem
numGrid :: [[Char]] -> [Int]
numGrid cg = [(read . clean) c | c <- transpose cg]

problemsPart2 :: [String] -> [([Int], Char)]
problemsPart2 lns = zip ([numGrid cg | cg <- charGrids (init lns)]) (opsList $ last lns)


-- | Helper function for cleaning whitespace from a string
clean :: String -> String
clean = T.unpack . T.strip . T.pack
