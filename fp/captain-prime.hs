{-
  Captain Prime
  https://www.hackerrank.com/challenges/captain-prime

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
{-# OPTIONS -O2 #-}
import           Data.Char           (digitToInt)
import qualified Data.Set            as S (Set, fromList, member)

import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)

data Fate = CENTRAL | LEFT | RIGHT | DEAD
  deriving (Eq, Show)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\x y -> 10 * x + y) 0

primes :: Integral a => [a]
primes = 2 : sieve primes [3,5..] where
  sieve [] _      = []
  sieve (p:ps) xs = let (h,t) = span (< p * p) xs
                    in  h ++ sieve ps (filter (\n -> n `mod` p /= 0) t)

primeSet :: Integral a => Int -> S.Set a
primeSet n = S.fromList (take n primes)

primeSetP :: Integral a => (a -> Bool) -> S.Set a
primeSetP p = S.fromList (takeWhile p primes)

isPrime :: Integral a => S.Set a -> a -> Bool
isPrime = flip S.member

checkPrime :: Integral a => S.Set a -> a -> Fate
checkPrime s n
  | not (isPrime s n) || elem 0 digits = DEAD
  | otherwise = if leftPrime n
                  then if not (rightPrime n)
                    then LEFT
                    else CENTRAL
                  else if rightPrime n
                    then RIGHT
                    else DEAD
      where
        digits = toDigits (fromIntegral n)
        numDigits = length digits
        leftPrime n = all (==True) $
          map (\x -> isPrime s (n `mod` (10 ^ x))) [1..numDigits-1]
        rightPrime n = all (==True) $
          map (\x -> isPrime s (n `div` (10 ^ x))) [1..numDigits-1]

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  let set1 = primeSetP (< 1000000)
  forM_ [1..n] $ \_ -> do
    num <- (\x -> read x :: Int) <$> getLine
    print $ checkPrime set1 num
