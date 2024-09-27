module Fibonacci where

-- Tail recursion solution
fibonacciSum1 :: Integer -> Integer
fibonacciSum1 n = fibonacciSum' 0 2 1
  where
    fibonacciSum' :: Integer -> Integer -> Integer -> Integer
    fibonacciSum' acc prev prevPrev
      | prev > n = acc
      | even prev = fibonacciSum' (acc + prev) (prev + prevPrev) prev
      | otherwise = fibonacciSum' acc (prev + prevPrev) prev

-- Recursion solution
fibonacciSum2 :: Integer -> Integer
fibonacciSum2 n = fibonacciSum'' 2 1
  where
    fibonacciSum'' prev prevPrev
      | prev > n = 0
      | even prev = prev + fbs
      | otherwise = fbs
      where
        fbs = fibonacciSum'' (prev + prevPrev) prev

-- Modular solution
fibonacciSum3 :: Integer -> Integer
fibonacciSum3 n = sum $ filter even $ getArray [2, 1]
  where
    getArray :: [Integer] -> [Integer]
    getArray array@(x : (y : zs))
      | x > n = tail array
      | otherwise = getArray (x + y : array)

-- map-generated solution with iterations
fibonacciSum4 :: Integer -> Integer
fibonacciSum4 n = sum $ filter even $ takeWhile (<= n) $ map fst $ iterate (\(a, b) -> (b, a + b)) (1, 2)

-- special with infinite lazy list
fibonacciSum5 :: Integer -> Integer
fibonacciSum5 n = sum [x | x <- takeWhile (<= n) fibonacciInf, even x]
  where
    fibonacciInf = 1 : 2 : zipWith (+) fibonacciInf (tail fibonacciInf)
