module Day8 where

import Data.Foldable (minimumBy)
import Data.List (findIndex, sort, sortBy, union)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)

type Coord = (Int, Int, Int)

type PairDistance = ((Coord, Coord), Double)

run :: IO ()
run = do
  input <- readFile "inputs/day8.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns 1000))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int -> Int
-- part1 lns = 5
part1 lns n = product $ map length $ top3 $ sortedCircuits $ createCircuits $ take n $ rankedPairs (junctionBoxes lns)

part2 :: [String] -> Int
part2 lns = 
  let 
    jbs = junctionBoxes lns
    jbprs = rankedPairs jbs
  in
    multiplyXCoords $ determineSingleCircuitPoint jbprs jbs

joinAllCircuits :: [[a]] -> [a]
joinAllCircuits ccts = go ccts []
  where
    go [] jbs = jbs
    go (cs : css) jbs = go css (cs ++ jbs)

jbCounts :: (Ord a, Eq a) => [[a]] -> [(a, Int)]
jbCounts ccts = go (sort $ joinAllCircuits ccts) []
  where
    go [] jbs = jbs
    go (c : cs) [] = go cs [(c, 1)]
    go (c : cs) ((jb, cnt) : jbcnts)
      | jb == c = go cs ((jb, cnt + 1) : jbcnts)
      | otherwise = go cs ((c, 1) : (jb, cnt) : jbcnts)

top3 :: (Show a) => [[a]] -> [[a]]
top3 xs = take 3 $ sortBy (comparing (Down . length)) xs

sortedCircuits :: (Show a) => [[a]] -> [[a]]
sortedCircuits = sortBy (comparing (Down . length))

junctionBoxes :: [String] -> [Coord]
junctionBoxes = map junctionBox

-- | Creates coordinate from input line
--
-- Arguments
-- * @s@ input line
--
-- Returns
-- * 3d coordinate
junctionBox :: String -> Coord
junctionBox s = case [read v | v <- take 3 $ splitOn "," s] of
  x : y : z : _ -> (x, y, z)
  _ -> error "unable to extract coordinates "

-- | 3D distance calculation
--
-- Arguments
-- * @a@ first coordinate
-- * @b@ second coordinate
--
-- Returns
-- * distance
distance :: (Floating a) => (a, a, a) -> (a, a, a) -> a
distance (x, y, z) (x', y', z') = sqrt $ (x' - x) ^ (2 :: Int) + (y' - y) ^ (2 :: Int) + (z' - z) ^ (2 :: Int)

-- | Performs conversion of int to double coordinates
toD :: Coord -> (Double, Double, Double)
toD (a, b, c) = (fromIntegral a, fromIntegral b, fromIntegral c)

-- | Distances per jb for a given junction box
--
-- Arguments
-- * @jbs@ - list of junction boxes
-- * @jb@ - junction box under test
--
-- Returns
-- * All distances between junction boxes
jbDistances :: [Coord] -> Coord -> [(Coord, Double)]
jbDistances jbs jb = [(x, distance (toD jb) (toD x)) | x <- jbs, x /= jb]

normalizePairJbDistance :: PairDistance -> PairDistance
normalizePairJbDistance ((x, y), d) = if x <= y then ((x, y), d) else ((y, x), d)

deduplicatePairs :: [PairDistance] -> [PairDistance]
deduplicatePairs pds = go (sortBy (comparing fst) pds) []
  where
    go [] acc = acc
    go (x : xs) [] = go xs [x]
    go (x : xs) (y : ys)
      | x == y = go xs (y : ys)
      | otherwise = go xs (x : y : ys)

-- | Determines the closest junction box
findClosest :: [Coord] -> Coord -> Coord
findClosest jbs jb = fst $ minimumBy (comparing snd) [x | x <- jbDistances jbs jb, snd x /= 0]

-- | Determines the closest junction box, returns the pair and the distance
findClosestWithPair :: [Coord] -> Coord -> PairDistance
findClosestWithPair jbs jb =
  let (jb', d) =
        minimumBy
          (comparing snd)
          [x | x <- jbDistances jbs jb, snd x /= 0]
      (a, b) = if jb <= jb' then (jb, jb') else (jb', jb)
   in ((a, b), d)

-- | All pair distances per coordinate pair duplicated
--
-- Arguments
-- * @jbs@ list of junction boxes
--
-- Returns
-- * All pairs and their distance
allPairDistances :: [Coord] -> [PairDistance]
allPairDistances jbs = go jbs []
  where
    go [] acc = acc
    go (x : xs) acc = go xs ([normalizePairJbDistance ((x, y), d) | (y, d) <- jbDistances xs x] ++ acc)

-- | Provides pairs sorted by their distance ascending returning only the pair of jbs
--
-- Arguments
-- * @jbs@ list of all junction boxes
--
-- Returns
-- * List of all pairs ranked by their distance ascending
rankedPairs :: [Coord] -> [(Coord, Coord)]
rankedPairs jbs = map fst $ sortBy (comparing snd) (allPairDistances jbs)

-- A helper to remove an element at a given index
removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

-- A helper to update an element at a given index
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = case splitAt idx xs of
  (before, at : after) -> before ++ (f at : after)
  _ -> xs -- Index out of bounds, return original list

-- | Takes pair of junction boxes and determines which circuit the new jb belongs
--
-- Arguments
-- * @ccts@ all currently known circuits
-- * (jb, jb') new junction box and its closest junction box
--
-- Returns
-- * Adjusted list of circuits
manageCircuits :: (Eq a) => [[a]] -> (a, a) -> [[a]]
manageCircuits ccts (jb, jb') =
  let idx_jb = findIndex (elem jb) ccts
      idx_jb' = findIndex (elem jb') ccts
   in case (idx_jb, idx_jb') of
        -- Neither is in a circuit, create a new one.
        (Nothing, Nothing) -> [jb, jb'] : ccts
        -- One is in a circuit, the other isn't. Add the other to it.
        (Just i, Nothing) -> updateAt i (union [jb']) ccts
        (Nothing, Just j) -> updateAt j (union [jb]) ccts
        -- Both are in circuits.
        (Just i, Just j)
          -- Both in the same circuit, nothing to do.
          | i == j -> ccts
          -- In different circuits. Merge them.
          | otherwise ->
              let cct_i = ccts !! i
                  cct_j = ccts !! j
                  -- remove the two old circuits and add the new merged one.
                  -- remove smaller index first to not shift the smaller one.
                  (idxMax, idxMin) = if i > j then (i, j) else (j, i)
                  ccts' = removeAt idxMin (removeAt idxMax ccts)
               in union cct_i cct_j : ccts'

-- | Group up junction boxes
--
-- Arguments
-- * @jbprs@ paired junction boxes to be further grouped
--
-- Returns
-- * All junction boxes in their circuits
createCircuits :: [(Coord, Coord)] -> [[Coord]]
createCircuits jbprs = go jbprs []
  where
    go [] ccts = ccts
    go (jpr' : jprs') ccts = go jprs' $ manageCircuits ccts jpr'

-- | Determines the pair that causes the first instance of an entire single circuit
-- for all junction boxes
--
-- Arguments
-- * @jbprs@ pairs of jb boxes sorted by distance ascending
-- * @jbs@ list of all jbs
--
-- Returns
-- * Coordinate pair evaluation that produces single circuit
determineSingleCircuitPoint :: [(Coord, Coord)] -> [Coord] -> (Coord, Coord)
determineSingleCircuitPoint jbprs jbs = go jbprs []
  where
    go [x] _ = x
    go (jbpr' : jbprs') ccts =
      let newCcts = manageCircuits ccts jbpr'
       in if length newCcts == 1 && length (head newCcts) == length jbs
            then jbpr'
            else go jbprs' newCcts
    go [] _ = error "unexpected circuit state"

-- | Simply multiplies the X coords of a pair of junction boxes
--
-- Arguments
-- * @jbpr@ pair of junction boxes
--
-- Returns
-- * product of x coordinates
multiplyXCoords :: (Coord, Coord) -> Int
multiplyXCoords ((x1, _, _), (x2, _, _)) = x1 * x2
