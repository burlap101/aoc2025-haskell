module Day5 where

import Data.List.Split (splitOn)
import Data.List (nub, sort)

run :: IO ()
run = do
  input <- readFile "inputs/day5.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))


part1 :: [String] -> Int
part1 lns = do
  let igdts = ingredients lns
  let rngs = freshRanges lns
  length $ filter (isFresh rngs) igdts 

part2 :: [String] -> Int
part2 lns = sum $ map (\(x, y) -> y - x + 1) $ mergeRanges $ freshRanges lns

freshRanges :: [String] -> [(Int, Int)]
freshRanges lns = [(read x, read y) | (x:y:_) <- map (splitOn "-") $ takeWhile (/="") lns] 

ingredients :: [String] -> [Int]
ingredients lns = map read $ tail $ dropWhile (/="") lns

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh rngs igdt = or [igdt >= fst rng && igdt <= snd rng | rng <- rngs]

-- | Expands the entries on a list based on the supplied range
--
-- The count of ranges will always be the same
--
-- Arguments
-- * @rng@ - range to use for comparison
-- * @rngs@ - list of ranges to be compared against
-- 
-- Returns
-- * List of expanded ranges
expandRange :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
expandRange rng rngs = go rng rngs []
  where 
  -- | Here go modifies top element of the list to meet the range supplied
    go :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    go _ [] acc = reverse acc
    go rng' (r:rs) acc
      | fst rng' < fst r && snd rng' > fst r = go rng' ((fst rng', snd r):rs) acc  -- This means the beginning of the range r lies between the rng
      | snd rng' > snd r && fst rng' < snd r = go rng' ((fst r, snd rng'):rs) acc  -- this means the end of the range r lies between the rng
      | otherwise = go rng' rs (r:acc)

-- | Determines if a range is a subrange of another within the list
--
-- Arguments
-- * @rng@ - range under scrutiny
-- * @(r:rs)@ - range list to compare against
-- 
-- Returns
-- * True if is a subrange else false
isSubRange :: (Int, Int) -> [(Int, Int)] -> Bool
isSubRange _ [] = False
isSubRange rng (r:rs)
  | fst rng >= fst r && snd rng < snd r = True
  | fst rng > fst r && snd rng <= snd r = True
  | otherwise = isSubRange rng rs

-- / Remove ranges that are duplicates or subranges of others
--
-- Arguments
-- * @rngs@ - list of ranges to deduplicate
--
-- Returns 
-- * list of ranges with subranges removed
dedupRanges :: [(Int, Int)] -> [(Int, Int)]
dedupRanges rngs = nub [ r | r <- rngs, not $ isSubRange r rngs]

-- | Expands all ranges for a given list
expandRanges :: [(Int, Int)] -> [(Int, Int)]
expandRanges rngs = go rngs 0
  where
    go lst i
      | i >= length lst = lst
      | otherwise = do
          let pivot = lst !! i
          let rest = drop (i+1) lst
          let newRest = fixpoint (expandRange pivot) rest
          let newList = take (i+1) lst ++ newRest
          go newList (i+1)

refactoredRanges :: [(Int, Int)] -> [(Int, Int)]
refactoredRanges = fixpoint (dedupRanges . expandRanges)

-- | Helper function to apply a function to an operable and 
-- continue until it is equal its previous state
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
  in if x' == x
    then x
    else fixpoint f x'


-- | This was implemented when the above failed. I was off by 7 somehow on part 2
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = foldr merge [] . sort
  where
    merge r [] = [r]
    merge (a,b) ((c,d):rs)
      | b < c - 1 = (a,b):(c,d):rs
      | otherwise = (min a c, max b d) : rs
