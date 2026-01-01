module Main (main) where

import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import Test.HUnit

tests :: Test
testsDay1 :: Test
testsDay2 :: Test
testsDay3 :: Test
testsDay4 :: Test
testsDay5 :: Test
testsDay6 :: Test
testsDay7 :: Test
combos :: [Day1.Combo]
combos = [Day1.Combo 'R' 10, Day1.Combo 'L' 5, Day1.Combo 'R' 45]

posns :: [Int]
posns = [50, 60, 55, 0]

testsDay1 =
  TestList
    [ "parses R10 correctly" ~: Day1.parseLine "R10" ~?= Just (Day1.Combo 'R' 10),
      "turns R10 correctly past 99" ~: Day1.turn 98 (Just (Day1.Combo 'R' 10)) ~?= Just 8,
      "turns L10 correctly past 0" ~: Day1.turn 5 (Just (Day1.Combo 'L' 10)) ~?= Just 95,
      "turns R10 correctly below 99" ~: Day1.turn 10 (Just (Day1.Combo 'R' 10)) ~?= Just 20,
      "turns L10 correctly above 0" ~: Day1.turn 55 (Just (Day1.Combo 'L' 10)) ~?= Just 45,
      "turns R10 correctly to 0" ~: Day1.turn 90 (Just (Day1.Combo 'R' 10)) ~?= Just 0,
      "turns L10 correctly to 0" ~: Day1.turn 10 (Just (Day1.Combo 'L' 10)) ~?= Just 0,
      "produces correct positions" ~: Day1.positions combos ~?= Just posns,
      -- Scenario 1~: R50, L50 → password 1
      "scenario 1" ~: Day1.part1 ["R50", "L50"] ~?= 1,
      -- Scenario 2~: R49, R1, L51 → password 1
      "scenario 2" ~: Day1.part1 ["R49", "R1", "L51"] ~?= 1,
      -- Scenario 3~: R50, R50, L100 → password 1
      "scenario 3" ~: Day1.part1 ["R50", "R50", "L100"] ~?= 1,
      -- Scenario 4~: R50, L50, R50, L50 → password 2
      "scenario 4" ~: Day1.part1 ["R50", "L50", "R50", "L50"] ~?= 2,
      -- Scenario 5~: L150, R250 → password 1
      "scenario 5" ~: Day1.part1 ["L150", "R250"] ~?= 1,
      "turns L851 correctly below 0" ~: Day1.turn 50 (Just (Day1.Combo 'L' 851)) ~?= Just 99,
      "counts rotations correctly for R500" ~: Day1.turnRotationCount 50 (Just (Day1.Combo 'R' 500)) ~?= Just 5,
      "counts rotations correctly for R150" ~: Day1.turnRotationCount 50 (Just (Day1.Combo 'R' 150)) ~?= Just 2,
      "counts rotations correctly for R200" ~: Day1.turnRotationCount 0 (Just (Day1.Combo 'R' 200)) ~?= Just 2,
      "counts rotations correctly for L500" ~: Day1.turnRotationCount 50 (Just (Day1.Combo 'L' 500)) ~?= Just 5,
      "counts rotations correctly for L150" ~: Day1.turnRotationCount 50 (Just (Day1.Combo 'L' 150)) ~?= Just 2,
      "counts rotations correctly for L50" ~: Day1.turnRotationCount 25 (Just (Day1.Combo 'L' 50)) ~?= Just 1,
      "counts rotations correctly for L50" ~: Day1.turnRotationCount 50 (Just (Day1.Combo 'L' 50)) ~?= Just 1
    ]

testsDay2 =
  TestList
    [ "parseLine 10-15" ~: Day2.parseLine "10-15" ~?= Just (10, 15),
      "numList (10, 15)" ~: Day2.numList (10, 15) ~?= [10, 11, 12, 13, 14, 15],
      "isDoubledNum 11221122" ~: Day2.isDoubledNum 11221122 ~?= True,
      "isDoubledNum 33444433" ~: Day2.isDoubledNum 33444433 ~?= False,
      "digitCount 1122334455" ~: Day2.digitCount 1122334455 ~?= 10,
      "invalidIds2 1" ~: Day2.invalidIds2 [11, 12, 22] ~?= [11, 22],
      "invalidIds2 2" ~: Day2.invalidIds2 [123123, 12, 12412] ~?= [123123],
      "invalidIds2 3" ~: Day2.invalidIds2 [1231234, 12341234] ~?= [12341234],
      "repeats 1212" ~: Day2.repeats 1212 ~?= True,
      "repeats 123123" ~: Day2.repeats 123123 ~?= True,
      "digitsForString 3 12312312" ~: Day2.digitsForString 3 "12312312" ~?= [],
      "digitsForString 2 12312312" ~: Day2.digitsForString 2 "12312312" ~?= ["12", "31", "23", "12"],
      --      "digitComparisonOperands 2 1212" ~: Day2.digitComparisonOperands 2 "1212" ~?= [["12", "12"], ["21"], ["12"]],
      --      "digitComparisonOperands 1 11" ~: Day2.digitComparisonOperands 1 "11" ~?= [["1", "1"], ["1"]],
      --      "digitComparisonOperands 3 1231231234" ~: Day2.digitComparisonOperands 3 "1231231234" ~?= [["123", "123", "123"], ["231", "231", "234"], ["312", "312"], ["123", "123"], ["231", "234"], ["312"], ["123"], ["234"]],
      "allDigitComparisons 123" ~: Day2.allDigitComparisons 123 ~?= [["1", "2", "3"], [], ["123"]],
      "allDigitComparisons 123123"
        ~: Day2.allDigitComparisons 123123
        ~?= [ ["1", "2", "3", "1", "2", "3"],
              ["12", "31", "23"],
              ["123", "123"],
              [],
              [],
              ["123123"]
            ],
      "allDigitComparisons 1188511885"
        ~: Day2.allDigitComparisons 1188511885
        ~?= [ ["1", "1", "8", "8", "5", "1", "1", "8", "8", "5"],
              ["11", "88", "51", "18", "85"],
              [],
              [],
              ["11885", "11885"],
              [],
              [],
              [],
              [],
              ["1188511885"]
            ],
      "hasRepeat False [\"12\", \"12\"]" ~: Day2.hasRepeat False ["12", "12"] ~?= True,
      "hasRepeat False [\"112\", \"12\"]" ~: Day2.hasRepeat False ["112", "12"] ~?= False,
      "hasRepeat False [\"1\", \"1\"]" ~: Day2.hasRepeat False ["1", "1"] ~?= True,
      "isRepeatsOnly [\"1\", \"1\"]" ~: Day2.isRepeatsOnly ["1", "1"] ~?= True,
      "isRepeatsOnly [\"1\", \"1\", \"2\"]" ~: Day2.isRepeatsOnly ["1", "1", "2"] ~?= False,
      "isRepeatsOnly [\"1\"]" ~: Day2.isRepeatsOnly ["1"] ~?= False,
      "isRepeatsOnly [\"123\", \"123\"]" ~: Day2.isRepeatsOnly ["123", "123"] ~?= True,
      "repeats 2121212118" ~: Day2.repeats 2121212118 ~?= False,
      TestCase $ do
        input <- readFile "inputs/day2_test.txt"
        let lns = splitOn "," input
        let result = Day2.part2 lns
        assertEqual "part2 on day2_test.txt" 4174379265 result
    ]

testsDay3 =
  TestList
    [ "maxNJoltage 12 987654321111111" ~: Day3.maxNJoltage 12 [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1] ~?= 987654321111,
      "maxNJoltage 12 811111111111119" ~: Day3.maxNJoltage 12 [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9] ~?= 811111111119,
      "nDigit 1 [1, 1, 1, 9]" ~: Day3.nDigit 1 [1, 1, 1, 9] ~?= 9,
      TestCase $ do
        input <- readFile "inputs/day3_test.txt"
        let lns = lines input
        let result = Day3.part2 lns
        assertEqual "part2 on day3_test.txt" 3121910778619 result
    ]

scoredFloor0 :: HM.HashMap String Int
scoredFloor0 = HM.fromList [("2-9", 5), ("10-1", 1), ("8-5", 6), ("10-6", 5), ("2-5", 4), ("6-7", 6), ("3-10", 4), ("5-5", 8), ("7-2", 4), ("4-4", 7), ("2-10", 4), ("3-3", 6), ("10-9", 2), ("5-4", 7), ("9-4", 7), ("9-8", 7), ("8-3", 6), ("9-6", 7), ("1-9", 3), ("6-5", 7), ("10-3", 3), ("8-8", 7), ("4-5", 7), ("10-5", 4), ("4-9", 4), ("1-6", 3), ("4-3", 6), ("8-10", 4), ("3-7", 2), ("5-1", 3), ("8-9", 7), ("3-4", 7), ("9-7", 6), ("3-5", 5), ("1-4", 3), ("1-3", 3), ("3-9", 4), ("4-1", 4), ("10-7", 4), ("4-6", 6), ("9-5", 6), ("8-4", 6), ("1-8", 4), ("2-3", 6), ("7-10", 4), ("6-10", 4), ("6-3", 6), ("9-2", 5), ("6-8", 5), ("3-1", 4), ("7-8", 6), ("6-4", 5), ("7-6", 5), ("2-7", 4), ("5-7", 5), ("6-6", 6), ("2-1", 3), ("9-3", 5), ("5-10", 3), ("5-6", 7), ("7-9", 7), ("1-7", 3), ("7-4", 6), ("5-2", 5), ("2-2", 6), ("6-2", 4), ("9-9", 5), ("8-7", 6), ("3-2", 7), ("5-9", 4), ("8-1", 2)]

unscoredFloor0 :: HM.HashMap String Int
unscoredFloor0 = HM.fromList [("2-9", 1), ("10-1", 1), ("8-5", 1), ("10-6", 1), ("2-5", 1), ("6-7", 1), ("10-2", 0), ("3-10", 1), ("5-5", 1), ("7-2", 1), ("4-4", 1), ("2-10", 1), ("3-3", 1), ("10-9", 1), ("2-8", 0), ("5-4", 1), ("9-4", 1), ("2-4", 0), ("7-7", 0), ("3-6", 0), ("9-8", 1), ("8-3", 1), ("9-6", 1), ("1-9", 1), ("6-5", 1), ("8-6", 0), ("5-3", 0), ("9-1", 0), ("1-2", 0), ("4-10", 0), ("4-2", 0), ("10-3", 1), ("8-8", 1), ("4-5", 1), ("4-7", 0), ("10-5", 1), ("4-9", 1), ("1-5", 0), ("1-6", 1), ("4-3", 1), ("8-10", 1), ("3-7", 1), ("5-1", 1), ("8-9", 1), ("3-4", 1), ("9-7", 1), ("3-5", 1), ("1-4", 1), ("1-10", 0), ("6-9", 0), ("5-8", 0), ("1-3", 1), ("3-9", 1), ("10-10", 0), ("4-1", 1), ("10-7", 1), ("4-6", 1), ("9-5", 1), ("8-4", 1), ("1-8", 1), ("2-3", 1), ("7-10", 1), ("6-10", 1), ("6-3", 1), ("9-2", 1), ("6-8", 1), ("4-8", 0), ("3-1", 1), ("7-8", 1), ("6-4", 1), ("7-6", 1), ("2-7", 1), ("5-7", 1), ("6-6", 1), ("2-1", 1), ("10-4", 0), ("9-3", 1), ("5-10", 1), ("5-6", 1), ("7-5", 0), ("1-1", 0), ("7-9", 1), ("1-7", 1), ("7-4", 1), ("8-2", 0), ("5-2", 1), ("2-6", 0), ("2-2", 1), ("7-3", 0), ("3-8", 0), ("6-2", 1), ("9-10", 0), ("6-1", 0), ("7-1", 0), ("9-9", 1), ("10-8", 0), ("8-7", 1), ("3-2", 1), ("5-9", 1), ("8-1", 1)]

unscoredFloor1 :: HM.HashMap String Int
unscoredFloor1 = HM.fromList [("2-9", 1), ("10-1", 0), ("8-5", 1), ("10-6", 1), ("2-5", 1), ("6-7", 1), ("10-2", 0), ("3-10", 1), ("5-5", 1), ("7-2", 1), ("4-4", 1), ("2-10", 1), ("3-3", 1), ("10-9", 0), ("2-8", 0), ("5-4", 1), ("9-4", 1), ("2-4", 0), ("7-7", 0), ("3-6", 0), ("9-8", 1), ("8-3", 1), ("9-6", 1), ("1-9", 0), ("6-5", 1), ("8-6", 0), ("5-3", 0), ("9-1", 0), ("1-2", 0), ("4-10", 0), ("4-2", 0), ("10-3", 0), ("8-8", 1), ("4-5", 1), ("4-7", 0), ("10-5", 1), ("4-9", 1), ("1-5", 0), ("1-6", 0), ("4-3", 1), ("8-10", 1), ("3-7", 0), ("5-1", 0), ("8-9", 1), ("3-4", 1), ("9-7", 1), ("3-5", 1), ("1-4", 0), ("1-10", 0), ("6-9", 0), ("5-8", 0), ("1-3", 0), ("3-9", 1), ("10-10", 0), ("4-1", 1), ("10-7", 1), ("4-6", 1), ("9-5", 1), ("8-4", 1), ("1-8", 1), ("2-3", 1), ("7-10", 1), ("6-10", 1), ("6-3", 1), ("9-2", 1), ("6-8", 1), ("4-8", 0), ("3-1", 1), ("7-8", 1), ("6-4", 1), ("7-6", 1), ("2-7", 1), ("5-7", 1), ("6-6", 1), ("2-1", 0), ("10-4", 0), ("9-3", 1), ("5-10", 0), ("5-6", 1), ("7-5", 0), ("1-1", 0), ("7-9", 1), ("1-7", 0), ("7-4", 1), ("8-2", 0), ("5-2", 1), ("2-6", 0), ("2-2", 1), ("7-3", 0), ("3-8", 0), ("6-2", 1), ("9-10", 0), ("6-1", 0), ("7-1", 0), ("9-9", 1), ("10-8", 0), ("8-7", 1), ("3-2", 1), ("5-9", 1), ("8-1", 0)]

keysRemovedIter1 :: [String]
keysRemovedIter1 = ["10-1", "10-9", "1-9", "10-3", "1-6", "3-7", "5-1", "1-4", "1-3", "2-1", "5-10", "1-7", "8-1"]

testsDay4 =
  TestList
    [ "removableKeys iteration 1" ~: Day4.removableKeys scoredFloor0 ~?= keysRemovedIter1,
      "removeRolls iteration 1" ~: Day4.removeRolls keysRemovedIter1 unscoredFloor0 ~?= unscoredFloor1,
      "removeResult iteration 1" ~: Day4.removeResult unscoredFloor0 ~?= (unscoredFloor1, 13),
      TestCase $ do
        input <- readFile "inputs/day4_test.txt"
        let lns = lines input
        let result = Day4.part2 lns
        assertEqual "part2 on day4_test.txt" 43 result
    ]

testsDay5 =
  TestList
    [ "freshRanges [1-6, 7-9, \"\", something]" ~: Day5.freshRanges ["1-6", "7-9", "", "something"] ~?= [(1, 6), (7, 9)],
      "ingredients [1-6, 7-9, \"\", 5, 16]" ~: Day5.ingredients ["1-6", "7-9", "", "5", "16"] ~?= [5, 16],
      TestCase $ do
        input <- readFile "inputs/day5_test.txt"
        let lns = lines input
        let result = Day5.part1 lns
        assertEqual "part1 on day5_test.txt" 3 result,
      "expandRange (5, 10) [(5, 10), (3, 6), (7, 15)]" ~: Day5.expandRange (5, 10) [(5, 10), (3, 6), (7, 15)] ~?= reverse [(5, 15), (3, 10), (5, 10)],
      "expandRange (5, 10) [(5, 10), (3, 16), (7, 15)]" ~: Day5.expandRange (5, 10) [(5, 10), (3, 16), (7, 15)] ~?= reverse [(5, 15), (3, 16), (5, 10)],
      "expandRange (5, 10) [(5, 10), (5, 10), (7, 15)]" ~: Day5.expandRange (5, 10) [(5, 10), (5, 10), (7, 15)] ~?= reverse [(5, 15), (5, 10), (5, 10)],
      "dedupRanges [(1, 10), (3,6), (11,15), (12,13)]" ~: Day5.dedupRanges [(1, 10), (3, 6), (11, 15), (12, 13)] ~?= [(1, 10), (11, 15)],
      "isSubRange [(1, 10), (3,6), (11,15), (12,13)]" ~: Day5.dedupRanges [(1, 10), (3, 6), (11, 15), (12, 13)] ~?= [(1, 10), (11, 15)],
      "expandRanges [(1, 5), (4, 6), (7, 8)]" ~: Day5.expandRanges [(1, 5), (4, 6), (7, 8)] ~?= [(1, 5), (1, 6), (7, 8)],
      TestCase $ do
        input <- readFile "inputs/day5_test.txt"
        let lns = lines input
        let result = Day5.part2 lns
        assertEqual "part2 on day5_test.txt" 14 result
    ]

testsDay6 =
  TestList
    [ "gridSizes [[1, 2, 33], [11, 22, 333], [111, 2, 3333]]" ~: Day6.gridSizes [[1, 2, 33], [11, 22, 333], [111, 2, 3333]] ~?= [3, 2, 4],
      "charGrid 2 [\"11 222 3333\", \" 1 22  33  \", \"1  2    3  \"]" ~: Day6.charGrid 2 ["11 222 3333", " 1 22  33  ", "1  2    3  "] ~?= (["11", " 1", "1 "], ["222 3333", "22  33  ", "2    3  "]),
      "numGrid [64, 23, 314]" ~: Day6.numGrid ["64 ", "23 ", "314"] ~?= [623, 431, 4],
      "charGrids [\"11 222 3333\", \" 1 22  33  \", \"1  2    3  \"]" ~: Day6.charGrids ["11 222 3333", " 1 22  33  ", "1  2    3  "] ~?= [["11", " 1", "1 "], ["222", "22 ", "2  "], ["3333", "33  ", " 3  "]],
      TestCase $ do
        input <- readFile "inputs/day6_test.txt"
        let lns = lines input
        let result = Day6.part2 lns
        assertEqual "part2 on day6_test.txt" 3263827 result
    ]

manifold1 :: [String]
manifold1 = ["...^...", ".......", "..^...^.."]

testsDay7 =
  TestList
    [ "splitterLocs manifold1" ~: Day7.splitterLocs manifold1 ~?= [(2, 2), (2, 6), (0, 3)],
      TestCase $ do
        input <- readFile "inputs/day7_test.txt"
        let lns = lines input
        let result = Day7.part1 lns
        assertEqual "part1 on day7_test.txt" 21 result,
      TestCase $ do
        input <- readFile "inputs/day7_test.txt"
        let lns = lines input
        let result = Day7.part2 lns
        assertEqual "part2 on day7_test.txt" 40 result
    ]

tests =
  TestList
    [ testsDay1,
      testsDay2,
      testsDay3,
      testsDay4,
      testsDay5,
      testsDay6,
      testsDay7
    ]

main :: IO ()
main = runTestTT tests >>= print
