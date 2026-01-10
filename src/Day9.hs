module Day9 where

import Data.List.Split (splitOn)

type Coord = (Int, Int)

run :: IO ()
run = do
  input <- readFile "inputs/day9.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
--  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 lns = maximum $ areas (corners lns)

corners :: [String] -> [Coord]
corners = map parseLine

-- | Creates coordinate from input line
--
-- Arguments
-- * @s@ input line
--
-- Returns
-- * corner coordinate
parseLine :: String -> Coord
parseLine s = case [read v | v <- take 2 $ splitOn "," s] of
  x : y : _ -> (x, y)
  _ -> error "unable to extract coordinates "

-- | Area of rectangle made by two coordinates
--
-- Arguments
-- * @c1@ first corner
-- * @c2@ second corner
--
-- Returns
-- * Area
area :: Coord -> Coord -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

-- | Returns all pairs of corners
--
-- Arguments
-- * @cnrs@ list of all coordinates that a corner exists
--
-- Returns
-- * all areas from each pair of corners
areas :: [Coord] -> [Int]
areas cnrs = go cnrs []
  where
    go [] acc = acc
    go (x : xs) acc = go xs ([area x y | y <- xs] ++ acc)
