module Main (main) where

import Data.List.Split (splitOn)
import qualified Day1
import qualified Day2
import qualified Day3
import Test.HUnit

tests :: Test
testsDay1 :: Test
testsDay2 :: Test
testsDay3 :: Test
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
      "allDigitComparisons 123123" ~: Day2.allDigitComparisons 123123 ~?= [
          ["1", "2", "3", "1", "2", "3"]
        , ["12", "31", "23"]
        , ["123", "123"]
        , []
        , []
        , ["123123"]
      ],
      "allDigitComparisons 1188511885" ~: Day2.allDigitComparisons 1188511885 ~?= [
          ["1", "1", "8", "8", "5", "1", "1", "8", "8", "5"]
        , ["11", "88", "51", "18", "85"]
        , []
        , []
        , ["11885", "11885"]
        , []
        , []
        , []
        , []
        , ["1188511885"]
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
    [
        "maxNJoltage 12 987654321111111" ~: Day3.maxNJoltage 12 [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1] ~?= 987654321111
      , "maxNJoltage 12 811111111111119" ~: Day3.maxNJoltage 12 [8,1,1,1,1,1,1,1,1,1,1,1,1,1,9] ~?= 811111111119
      , "nDigit 1 [1, 1, 1, 9]" ~: Day3.nDigit 1 [1, 1, 1, 9] ~?= 9
      , TestCase $ do
          input <- readFile "inputs/day3_test.txt"
          let lns = lines input
          let result = Day3.part2 lns
          assertEqual "part2 on day3_test.txt" 3121910778619 result
    ]

tests = TestList [testsDay1, testsDay2, testsDay3]

main :: IO ()
main = runTestTT tests >>= print
