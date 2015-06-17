{-
  Fibonacci
  https://www.hackerrank.com/challenges/fibonacci-fp

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)

fibs :: Integral a => [a]
fibs = map fib [1..]

fib :: Integral a => Int -> a
fib =  go (0, 1)
  where
    go (!a, !b) !n
      | n == 0    = a
      | otherwise = go (b, a + b) (n-1)

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  forM_ [1..n] $ \_ -> do
    m <- (\x -> read x :: Int) <$> getLine
    print $ fib m `mod` (10^8 + 7)
