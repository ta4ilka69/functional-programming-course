module Fibonacci where

-- Tail recursion solution
fibonacciSum1 :: Integer -> Integer
fibonacciSum1 n = fibonacciSum' 0 2 1 n
  where
    fibonacciSum' :: Integer -> Integer -> Integer -> Integer -> Integer
    fibonacciSum' acc prev prevPrev n
      | prev > n = acc
      | even prev = fibonacciSum' (acc + prev) (prev + prevPrev) prev n
      | otherwise = fibonacciSum' acc (prev + prevPrev) prev n

-- Recursion solution
fibonacciSum2 :: Integer -> Integer
fibonacciSum2 n = fibonacciSum'' 2 1 n
  where
    fibonacciSum'' prev prevPrev n
      | prev > n = 0
      | even prev = prev + fbs
      | otherwise = fbs
      where
        fbs = fibonacciSum'' (prev + prevPrev) prev n

-- Modular solution
fibonacciSum3 :: Integer -> Integer
fibonacciSum3 n = sum $ filter even $ getArray [2, 1] n
  where
    getArray :: [Integer] -> Integer -> [Integer]
    getArray array@(x : (y : zs)) n
      | x > n = tail array
      | otherwise = getArray (x + y : array) n

-- map-generated solution
fibonacciSum4 :: Integer -> Integer
fibonacciSum4 n = sum $ filter even $ takeWhile (<= n) $ map fst $ iterate (\(a, b) -> (b, a + b)) (1, 2)

--
