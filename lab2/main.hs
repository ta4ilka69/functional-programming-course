module Main where

import PropertyBasedTest (runPropertyTests)
import Test.HUnit
import UnitTest (unitTests)

main :: IO ()
main = do
  putStrLn "Running Unit tests..."
  _ <- runTestTT unitTests
  putStrLn "Running Property based tests..."
  runPropertyTests
