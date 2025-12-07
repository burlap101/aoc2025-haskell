-- module Main (main) where

import Day1
import Test.HUnit

tests :: Test
combos :: [Combo]
combos = [Combo 'R' 10, Combo 'L' 5, Combo 'R' 45]

posns :: [Int]
posns = [50, 60, 55, 0]

tests =
  TestList
    [ "parses R10 correctly" ~: parseLine "R10" ~?= Just (Combo 'R' 10),
      "turns R10 correctly past 99" ~: turn 98 (Just (Combo 'R' 10)) ~?= Just 8,
      "turns L10 correctly past 0" ~: turn 5 (Just (Combo 'L' 10)) ~?= Just 95,
      "turns R10 correctly below 99" ~: turn 10 (Just (Combo 'R' 10)) ~?= Just 20,
      "turns L10 correctly above 0" ~: turn 55 (Just (Combo 'L' 10)) ~?= Just 45,
      "turns R10 correctly to 0" ~: turn 90 (Just (Combo 'R' 10)) ~?= Just 0,
      "turns L10 correctly to 0" ~: turn 10 (Just (Combo 'L' 10)) ~?= Just 0,
      "produces correct positions" ~: positions combos ~?= Just posns,
      -- Scenario 1: R50, L50 → password 1
      "scenario 1" ~: part1 ["R50", "L50"] ~?= 1,
      -- Scenario 2: R49, R1, L51 → password 1
      "scenario 2" ~: part1 ["R49", "R1", "L51"] ~?= 1,
      -- Scenario 3: R50, R50, L100 → password 1
      "scenario 3" ~: part1 ["R50", "R50", "L100"] ~?= 1,
      -- Scenario 4: R50, L50, R50, L50 → password 2
      "scenario 4" ~: part1 ["R50", "L50", "R50", "L50"] ~?= 2,
      -- Scenario 5: L150, R250 → password 1
      "scenario 5" ~: part1 ["L150", "R250"] ~?= 1,
      "turns L851 correctly below 0" ~: turn 50 (Just (Combo 'L' 851)) ~?= Just 99,
      "counts rotations correctly for R500" ~: turnRotationCount 50 (Just (Combo 'R' 500)) ~?= Just 5,
      "counts rotations correctly for R150" ~: turnRotationCount 50 (Just (Combo 'R' 150)) ~?= Just 2,
      "counts rotations correctly for R200" ~: turnRotationCount 0 (Just (Combo 'R' 200)) ~?= Just 2,
      "counts rotations correctly for L500" ~: turnRotationCount 50 (Just (Combo 'L' 500)) ~?= Just 5,
      "counts rotations correctly for L150" ~: turnRotationCount 50 (Just (Combo 'L' 150)) ~?= Just 2,
      "counts rotations correctly for L50" ~: turnRotationCount 25 (Just (Combo 'L' 50)) ~?= Just 1,
      "counts rotations correctly for L50" ~: turnRotationCount 50 (Just (Combo 'L' 50)) ~?= Just 1
    ]

main :: IO ()
main = runTestTT tests >>= print
