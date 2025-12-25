module Day4 where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

run :: IO ()
run = do
  input <- readFile "inputs/day4.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 lns = length $ filter (< 4) $ HM.elems $ keyScores (locToRolls lns)

part2 :: [String] -> Int
part2 lns = sum $ map snd $ removalSteps (locToRolls lns) []

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
      nstr = strs !! 1
      n = read nstr :: Int
      m = read mstr :: Int
  filter (/= s) [show i ++ "-" ++ show j | i <- [m - 1 .. m + 1], j <- [n - 1 .. n + 1]]

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

-- | Produces a hashmap of all scores for the floor
--
-- Arguments
-- * @hm@ floor plan map
--
-- Returns
-- * hashmap of sum of all surrounding locations for each coordinate
keyScores :: HM.HashMap String Int -> HM.HashMap String Int
keyScores hm = HM.fromList [(k, keyScore k hm) | k <- HM.keys hm, (/= 0) (HM.findWithDefault 0 k hm)]

-- | Takes a scored floor and determines the keys removable
-- 
-- Arguments
-- * @hm@ scored floor plan
--
-- Returns
-- * List of keys that are removable
removableKeys :: HM.HashMap String Int -> [String]
removableKeys hm = HM.keys $ HM.filter (< 4) hm

-- | Removes the keys and resets the floor map
--
-- Arguments
-- * @ks@ keys to remove
-- * @hm@ floor map (unscored)
--
-- Returns
-- * floor map with removed locations
removeRolls :: [String] -> HM.HashMap String Int -> HM.HashMap String Int
removeRolls ks = HM.mapWithKey (\k v -> if k `elem` ks then 0 else v)

-- | Produces tuple with resulting floor map and how many
-- rolls were removed
--
-- Arguments
-- * @hm@ unscored floor map
--
-- Returns
-- * tuple with resulting unscored floor map and count of keys removed
removeResult :: HM.HashMap String Int -> (HM.HashMap String Int, Int)
removeResult hm = do
  let scoredHM = keyScores hm
  let rks = removableKeys scoredHM
  (removeRolls rks hm, length rks)

-- | Recursively moves through map for all roll removals
--
-- Arguments
-- * @hm@ floor map (unscored)
-- * @acc@ accumulated results
--
-- Returns
-- * List of tupled results from removeResult
removalSteps :: HM.HashMap String Int -> [(HM.HashMap String Int, Int)] -> [(HM.HashMap String Int, Int)]
removalSteps _ ((_, 0) : xs) = xs  -- Don't really need the last result
removalSteps hm acc = do
  let (resMap, i) = removeResult hm
  removalSteps resMap ((resMap, i):acc)
