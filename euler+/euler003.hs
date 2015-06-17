{-
  Project Euler+ 003
  https://www.hackerrank.com/contests/projecteuler/challenges/euler003

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}
import           Control.Monad (forM_)
import           Debug.Trace

primes :: Integral a => [a]
primes = 2 : sieve primes [3,5..] where
  sieve [] _      = []
  sieve (p:ps) xs = let (h,t) = span (< p * p) xs
                    in  h ++ sieve ps (filter (\n -> n `mod` p /= 0) t)

reducePrime :: [Int] -> Int -> Int
reducePrime= go [] where
  go acc [] x
    | null acc || x > head acc = x
    | otherwise = head acc
  go acc (p:ps) x
    | x `mod` p == 0  = let x' = x `div` p in go (p:acc) (reduceToSqrt x' primes) x'
    | otherwise       = go acc ps x

reduceToSqrt :: Int -> [Int] -> [Int]
reduceToSqrt n = takeWhile (<= (ceiling . sqrt $ fromIntegral n))

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print $ reducePrime (reduceToSqrt n primes) n
