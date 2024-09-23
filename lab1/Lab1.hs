module Lab1 where

-- Tail recursion solution
fibbonachiSum :: Integer -> Integer
fibbonachiSum n = fibbonachiSum' 0 2 1 n
  where
    fibbonachiSum' :: Integer -> Integer -> Integer -> Integer -> Integer
    fibbonachiSum' acc prev prevPrev n
      | prev > n = acc
      | even prev = fibbonachiSum' (acc + prev) (prev + prevPrev) prev n
      | otherwise = fibbonachiSum' acc (prev + prevPrev) prev n
