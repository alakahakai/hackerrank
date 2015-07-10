{-
  Different Ways
  https://www.hackerrank.com/challenges/different-ways-fp

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 1st, 2015
-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_)

ways :: [[Integer]]
ways = [[count k n | k <- [0..1000]] | n <- [0..]] where
  count k n
    | k > n || n < 1 = 0
    | k == 0    = 1
    | k == n    = 1
    | otherwise = ways !! (n-1) !! (k-1) + ways !! (n-1) !! k

solve :: Int -> Int -> Int
solve n k = fromIntegral (ways !! n !! k `mod` (10^8+7))

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    [n, k] <- liftA (map (\x -> read x :: Int) . words) getLine
    print $ solve n k
