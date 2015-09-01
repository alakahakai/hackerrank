{-
  Project Euler+ 007 - 10001st prime
  https://www.hackerrank.com/contests/projecteuler/challenges/euler007

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 21st, 2015
-}
import           Control.Monad (forM_)

primes :: Integral a => [a]
primes = 2 : sieve primes [3,5..] where
  sieve [] _      = []
  sieve (p:ps) xs = let (h,t) = span (< p * p) xs
                    in  h ++ sieve ps (filter (\n -> n `mod` p /= 0) t)

prime :: Integral a => Int -> a
prime n = primes !! (n-1)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print $ prime n
