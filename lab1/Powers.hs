module Powers where

import Data.List (nub)
import Data.Set (empty, insert, size)

-- Tail recursion solution
powersCount1 :: Integer -> Integer -> Int
powersCount1 a b = powersCount' empty 2 2
  where
    powersCount' acc n m
      | n > a = size acc
      | m > b = powersCount' acc (n + 1) 2
      | otherwise = powersCount' (insert (n ^ m) acc) n (m + 1)

-- Modular solution with infinity list
powersCount2 :: Integer -> Integer -> Int
powersCount2 a b = size $ foldr insert empty [x ^ y | x <- [2 .. a], y <- [2 .. b]]

-- map-generated or iterate-generated solution, whats simplier
powersCount3 :: Integer -> Integer -> Int
powersCount3 a b = size $ foldr insert empty $ map (^) [2 .. a] <*> [2 .. b]
