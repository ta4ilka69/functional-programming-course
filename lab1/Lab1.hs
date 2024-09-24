module Lab1 where

-- Tail recursion solution
fibbonachiSum1 :: Integer -> Integer
fibbonachiSum1 n = fibbonachiSum' 0 2 1 n
  where
    fibbonachiSum' :: Integer -> Integer -> Integer -> Integer -> Integer
    fibbonachiSum' acc prev prevPrev n
      | prev > n = acc
      | even prev = fibbonachiSum' (acc + prev) (prev + prevPrev) prev n
      | otherwise = fibbonachiSum' acc (prev + prevPrev) prev n

-- Recursion solution
fibbonachiSum2 :: Integer -> Integer
fibbonachiSum2 n = fibbonachiSum'' 2 1 n
  where
    fibbonachiSum'' prev prevPrev n
      | prev > n = 0
      | even prev = prev + fbs
      | otherwise = fbs
      where
        fbs = fibbonachiSum'' (prev + prevPrev) prev n

-- Modular solution
fibbonachiSum3 :: Integer -> Integer
