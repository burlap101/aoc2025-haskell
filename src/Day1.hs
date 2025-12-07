module Day1 where

import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)

data Combo = Combo {direction :: Char, count :: Int}
  deriving (Show, Eq)

run :: IO ()
run = do
  input <- readFile "inputs/day1.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 ls =
  case evPositions ls of
    Just posns -> length (filter (== 0) posns)
    Nothing -> 0

part2 :: [String] -> Int
part2 ls = sum (rotationsMapMaybe (rotations (mapMaybe parseLine ls)))

rotationsMapMaybe :: [Maybe Int] -> [Int]
rotationsMapMaybe = map (fromMaybe 0)

evPositions :: [String] -> Maybe [Int]
evPositions ls = positions (mapMaybe parseLine ls)

positionsVCombos :: [Int] -> [Combo] -> [(Int, Maybe Combo)]
positionsVCombos posns cs = zip posns (map Just cs)

rotations :: [Combo] -> [Maybe Int]
rotations cs =
  case positions cs of
    Just posns -> map (uncurry turnRotationCount) (positionsVCombos posns cs)
    Nothing -> []

-- Determines the final positions for a series of turns
positions :: [Combo] -> Maybe [Int]
positions = go 50
  where
    go :: Int -> [Combo] -> Maybe [Int]
    go pos [] = Just [pos]
    go pos (c : cs) = do
      newPos <- turn pos (Just c)
      rest <- go newPos cs
      return (pos : rest)

turn :: Int -> Maybe Combo -> Maybe Int
turn pos (Just (Combo 'R' n)) = Just ((pos + n) `mod` 100)
turn pos (Just (Combo 'L' n)) = Just ((pos - n) `mod` 100)
turn _ _ = Nothing

turnRotationCount :: Int -> Maybe Combo -> Maybe Int
turnRotationCount pos (Just (Combo dir n)) =
  let step = if dir == 'R' then 1 else -1
      -- simulate the clicks mathematically
      hits = length [k | k <- [1..n], (pos + k*step) `mod` 100 == 0]
  in Just hits
turnRotationCount _ _ = Nothing

parseLine :: String -> Maybe Combo
parseLine (d : ds) = do
  n <- readMaybe ds
  return (Combo d n)
parseLine [] = Nothing

parseInt :: String -> Maybe Int
parseInt = readMaybe
