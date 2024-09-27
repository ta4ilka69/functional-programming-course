module Main where

import Fibonacci
import Powers

main :: IO ()
main = do
  print $ fibonacciSum1 4000000
  print $ fibonacciSum2 4000000
  print $ fibonacciSum3 4000000
  print $ fibonacciSum4 4000000
  print $ fibonacciSum5 4000000
  print $ powersCount1 100 100
