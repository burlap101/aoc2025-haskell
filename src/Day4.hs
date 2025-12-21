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

locToRolls :: [String] -> HM.HashMap String Int
locToRolls = go 1 HM.empty
  where
    go :: Int -> HM.HashMap String Int -> [String] -> HM.HashMap String Int
    go _ hm [] = hm
    go row hm (ln : lns) =
      let mlist = [(show row ++ "-" ++ show col, if val == '@' then 1 else 0) | (col, val) <- zip ([1 ..] :: [Int]) ln]
          hm' = HM.union (HM.fromList mlist) hm
       in go (row + 1) hm' lns

surroundingKeys :: String -> [String]
surroundingKeys s = do
  let strs = splitOn "-" s
      mstr = head strs
      nstr = strs!!1
      n = read nstr :: Int
      m = read mstr :: Int
  filter (/=s) [show i ++ "-" ++ show j | i <- [m - 1 .. m + 1], j <- [n - 1 .. n + 1]]

-- | Produces a score for each key
keyScore :: String -> HM.HashMap String Int -> Int
keyScore k hm = sum $ catMaybes [HM.lookup k' hm | k' <- surroundingKeys k]

keyScores :: HM.HashMap String Int -> [Int]
keyScores hm = [ keyScore k hm | k <- HM.keys hm, (/=0)(HM.findWithDefault 0 k hm)]
