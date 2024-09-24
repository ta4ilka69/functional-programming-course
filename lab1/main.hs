module Main where

import Lab1

main :: IO ()
main = do
  print $ fibbonachiSum1 4000000
  print $ fibbonachiSum2 4000000
