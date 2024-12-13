module Main where

import CommandLineParser (Settings (..), parseArgs)
import Interpolation
  ( Algorithm (..),
    Point,
    lagrangeInterpolation,
    linearInterpolation,
  )
import StreamProcessing (parsePoint)
import Test.HUnit
import Text.Read (readMaybe)

-- Unit test for `parsePoint` function
parsePointTests :: Test
parsePointTests =
  TestList
    [ TestCase (assertEqual "parsePoint \"1.0 2.0\"" (Just (1.0, 2.0)) (parsePoint "1.0 2.0")),
      TestCase (assertEqual "parsePoint \"invalid input\"" Nothing (parsePoint "invalid input"))
    ]

-- Unit test for `parseArgs` function
parseArgsTests :: Test
parseArgsTests =
  TestList
    [ TestCase (assertEqual "parseArgs [\"linear\", \"0.1\"]" (Right (Settings Linear 0.1)) (parseArgs ["linear", "0.1"])),
      TestCase (assertEqual "parseArgs [\"lagrange\", \"0.2\"]" (Right (Settings Lagrange 0.2)) (parseArgs ["lagrange", "0.2"])),
      TestCase (assertEqual "parseArgs [\"both\", \"0.5\"]" (Right (Settings Both 0.5)) (parseArgs ["both", "0.5"])),
      TestCase (assertEqual "parseArgs [\"invalid\"]" (Left "Usage: program <algorithm> <rate>") (parseArgs ["invalid"]))
    ]

-- Unit test for `linearInterpolation`
linearInterpolationTests :: Test
linearInterpolationTests =
  TestList
    [ TestCase (assertEqual "linearInterpolation [ (1, 2), (3, 4) ]" [(1.0, 2.0), (2.0, 3.0), (3.0, 4.0)] (linearInterpolation 1.0 [(1.0, 2.0), (3.0, 4.0)])),
      TestCase (assertEqual "linearInterpolation with different rate" [(1.0, 2.0), (1.5, 3.0), (2.0, 4.0), (2.5, 5.0), (3.0, 6.0)] (linearInterpolation 0.5 [(1.0, 2.0), (3.0, 6.0)]))
    ]

-- Unit test for `lagrangeInterpolation`
lagrangeInterpolationTests :: Test
lagrangeInterpolationTests =
  TestList
    [ TestCase
        ( assertEqual
            "lagrangeInterpolation [ (1, 2), (3, 6), (5, 10) ]"
            [(1.0, 2.0), (3.0, 6.0), (5.0, 10.0)] -- Expected values
            (lagrangeInterpolation 2.0 [(1.0, 2.0), (3.0, 6.0), (5.0, 10.0)])
        ),
      TestCase
        ( assertEqual
            "lagrangeInterpolation with higher number of points"
            [(0.0, 0.0), (1.0, 1.0), (2.0, 4.0), (3.0, 9.0)] -- Expected values
            (lagrangeInterpolation 1.0 [(0.0, 0.0), (1.0, 1.0), (2.0, 4.0), (3.0, 9.0)])
        )
    ]

-- Main function to run all the tests
main :: IO Counts
main = runTestTT $ TestList [parsePointTests, parseArgsTests, linearInterpolationTests, lagrangeInterpolationTests]
