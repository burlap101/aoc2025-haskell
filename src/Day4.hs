module Day4 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

run :: IO ()
run = do
  input <- readFile "inputs/day4.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))

--  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 lns = length $ filter (<4) $ keyScores (locToRolls lns)

-- | Maps locations on the floor to it coordinate and whether
-- it contains a roll
--
-- Arguments 
-- * lines from input data
--
-- Returns
-- * Map of coordinate with value of 1 if contains a roll else 0
locToRolls :: [String] -> HM.HashMap String Int
locToRolls = go 1 HM.empty
  where
    go :: Int -> HM.HashMap String Int -> [String] -> HM.HashMap String Int
    go _ hm [] = hm
    go row hm (ln : lns) =
      let mlist = [(show row ++ "-" ++ show col, if val == '@' then 1 else 0) | (col, val) <- zip ([1 ..] :: [Int]) ln]
          hm' = HM.union (HM.fromList mlist) hm
       in go (row + 1) hm' lns

-- | Determines all potential surrounding keys for the floor layout
--
-- Arguments
-- * @s@: key at center
--
-- Returns
-- * Potential surrounding keys (could be non existant on edges)
surroundingKeys :: String -> [String]
surroundingKeys s = do
  let strs = splitOn "-" s
      mstr = head strs
      nstr = strs!!1
      n = read nstr :: Int
      m = read mstr :: Int
  filter (/=s) [show i ++ "-" ++ show j | i <- [m - 1 .. m + 1], j <- [n - 1 .. n + 1]]

-- | Produces a score for each key
--
-- Arguments
-- * @k@ coordinate key of the location on the floor
-- * @hm@ hashmap of the floorplan
--
-- Returns
-- * Sum of all surrounding locations
keyScore :: String -> HM.HashMap String Int -> Int
keyScore k hm = sum $ catMaybes [HM.lookup k' hm | k' <- surroundingKeys k]

-- | Produces a list of all scores for the floor
--
-- Arguments
-- * @hm@ floor plan map
--
-- Returns
-- list of sum of all surrounding locations for each coordinate
keyScores :: HM.HashMap String Int -> [Int]
keyScores hm = [ keyScore k hm | k <- HM.keys hm, (/=0)(HM.findWithDefault 0 k hm)]
