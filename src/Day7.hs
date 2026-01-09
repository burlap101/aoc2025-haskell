module Day7 where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (elemIndex, find, groupBy, sort, sortBy)
import Data.Maybe (catMaybes, fromMaybe)

run :: IO ()
run = do
  input <- readFile "inputs/day7.txt"
  let lns = lines input
  putStrLn ("Part1: " ++ show (part1 lns))
  putStrLn ("Part2: " ++ show (part2 lns))

part1 :: [String] -> Int
part1 lns = HS.size (traverseSplits (rootLoc lns) (byCols $ splitterLocs lns))

part2 :: [String] -> Int
part2 lns = pathCount (rootLoc lns) (byCols $ splitterLocs lns)

-- | Finds the start
--
-- Arguments
-- * @lns@ - all lines of the input
--
-- Returns
-- * row and column of the start
rootLoc :: [String] -> (Int, Int)
rootLoc lns = case elemIndex 'S' (head lns) of
  Just j -> (0, j)
  Nothing -> (0, 0)

-- | All splitter location from input
--
-- Arguments
-- * @lns@ - all lines of the input
--
-- Returns
-- * List of row,column coordinate tuples
splitterLocs :: [String] -> [(Int, Int)]
splitterLocs lns = go lns 0 []
  where
    go :: [String] -> Int -> [(Int, Int)] -> [(Int, Int)]
    go [] _ acc = acc
    go (l : ls) m acc = go ls (m + 1) ([(m, n) | (n, _) <- filter (\(_, y) -> y == '^') (zip [0 ..] l)] ++ acc)

-- | Produces a grouped by sorted cols list of lists of splitter locations
--
-- Arguments
-- * @slocs@ - original list of splitter locations
--
-- Returns
-- * List of columnised lists all sorted
byCols :: [(Int, Int)] -> [[(Int, Int)]]
byCols slocs = sort [sortBy (\(a, _) (b, _) -> compare a b) slocs' | slocs' <- groupBy (\x y -> snd x == snd y) slocs]

-- | Locates the next two split locations from a given split
--
-- Arguments
-- * @sloc@ - position split to occur
-- * @slocs@ - sorted grouped by column lists of splitter locations
--
-- Returns
-- * next two splitter locations
splits :: (Int, Int) -> [[(Int, Int)]] -> (Maybe (Int, Int), Maybe (Int, Int))
splits _ [] = (Nothing, Nothing)
splits (i, j) slocs = go (filter (\((y, _) : _) -> y > i) slocs) (Nothing, Nothing)
  where
    go :: [[(Int, Int)]] -> (Maybe (Int, Int), Maybe (Int, Int)) -> (Maybe (Int, Int), Maybe (Int, Int))
    go [] acc = acc
    go (x : xs) (Nothing, Nothing) = go xs (find (\(_, j') -> j' == j - 1) x, find (\(_, j') -> j' == j + 1) x)
    go (x : xs) (Nothing, r) = go xs (find (\(_, j') -> j' == j - 1) x, r)
    go (x : xs) (l, Nothing) = go xs (l, find (\(_, j') -> j' == j + 1) x)
    go _ landr = landr -- both left and right splitters found

-- | Adds splitter location to set of seen
--
-- Arguments
-- * @seen@ - all splitters seen
-- * @sps@ - pair of splitters to be considered to be added
--
-- Returns
-- * New set of splitters seen
addSplitsToSeen :: HS.HashSet (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int)) -> HS.HashSet (Int, Int)
addSplitsToSeen seen (Just x, Just y) = HS.unions [HS.insert x seen, HS.insert y seen]
addSplitsToSeen seen (Nothing, Just y) = HS.insert y seen
addSplitsToSeen seen (Just x, Nothing) = HS.insert x seen
addSplitsToSeen seen (Nothing, Nothing) = seen

-- | Manages the addition of splits to a splitter discovery queue
--
-- Arguments
-- * @seen@ - set of seen splitters
-- * @lst@ - current queue of splitters to traverse
-- * @(mx,my)@ - splitter location to potentially add for exploration
--
-- Returns
-- * List of splitters to explore
addSplitsToQueue :: (Eq a, Hashable a) => HS.HashSet a -> [a] -> (Maybe a, Maybe a) -> [a]
addSplitsToQueue seen lst (mx, my) = [l | l <- catMaybes [mx, my], not $ HS.member l seen] ++ lst

-- | Travels through the tree to gather all splitters seen
--
-- Arguments
-- * @rt@ - start point
-- * @slocs@ - grouped and sorted splitter locations
--
-- Returns
-- * Set of all splitter locations seen
traverseSplits :: (Int, Int) -> [[(Int, Int)]] -> HS.HashSet (Int, Int)
traverseSplits rt slocs = go [rt] HS.empty
  where
    go :: [(Int, Int)] -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int)
    go [] seen = seen
    go (x : xs) seen = do
      let sps = splits x slocs
      go (addSplitsToQueue seen xs sps) (HS.insert x seen)

incrementEdgeCount :: (Eq a) => Int -> (Maybe a, Maybe a) -> Int
incrementEdgeCount curr (mx, my) = curr + length (filter (== Nothing) [mx, my])

-- | Manages scoring of the nodes
--
-- Arguments
-- * @ndscrs@ - map of nodes currently scored
-- * @pos@ - splitter position under scrutiny
-- * @mx,my@ - child splitter result
--
-- Returns
-- * Rescored map of nodes
manageNodeScores :: (Eq a, Hashable a) => HM.HashMap a Int -> a -> (Maybe a, Maybe a) -> HM.HashMap a Int
manageNodeScores ndscrs pos (Nothing, Nothing) = HM.insert pos 2 ndscrs
manageNodeScores ndscrs pos (Just x, Nothing)
  | HM.member x ndscrs = HM.insert pos ((ndscrs HM.! x) + 1) ndscrs
  | otherwise = ndscrs
manageNodeScores ndscrs pos (Nothing, Just x)
  | HM.member x ndscrs = HM.insert pos ((ndscrs HM.! x) + 1) ndscrs
  | otherwise = ndscrs
manageNodeScores ndscrs pos (Just x, Just y)
  | HM.member x ndscrs && HM.member y ndscrs = HM.insert pos ((ndscrs HM.! x) + (ndscrs HM.! y)) ndscrs 
  | otherwise = ndscrs

-- | Manages splits on the stack
--
-- Arguments
-- * @scored@ set of all nodes currently scored
-- * @stk@ current stack with head the current node being split
-- * @splts@ children splitters of current splitter
--
-- Returns
-- * Resulting stack
manageSplitStack :: (Eq a, Hashable a) => HS.HashSet a -> [a] -> (Maybe a, Maybe a) -> [a] 
manageSplitStack _ [] _ = []
manageSplitStack scored (x:xs) chn
  | HS.member x scored = xs
  | otherwise = addSplitsToQueue scored (x:xs) chn

-- | Counts the times that beams hit the bottom edge
--
-- Arguments
-- * @rt@ - start point
-- * @slocs@ - grouped and sorted splitter locations
--
-- Returns
-- * Count of times beam hits bottom edge
pathCount :: (Int, Int) -> [[(Int, Int)]] -> Int
pathCount rt slocs = go [rt] HM.empty
  where
    go :: [(Int, Int)] -> HM.HashMap (Int, Int) Int -> Int
    go [] nodeScores = fromMaybe 0 (HM.lookup rt nodeScores)
    go (x:xs) nodeScores = do
      let sps = splits x slocs
      let newScores = manageNodeScores nodeScores x sps
      go (manageSplitStack (HM.keysSet newScores) (x:xs) sps) newScores
