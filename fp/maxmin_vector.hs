{-
  Max Min
  https://www.hackerrank.com/challenges/angry-children

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 10th, 2015
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 #-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM)
import           Data.List           (sort)
import qualified Data.Vector         as V (Vector, fromList, last, length, (!))
import           System.IO

-- | Get the numbers that produce the mimimum unfairness
maxMin :: Int -> V.Vector Int -> Int
maxMin k vs = go (V.last vs) 0 vs where
  go !acc !pos !vs
    | pos > len - k = acc
    | r < acc = go r (pos+1) vs
    | otherwise = go acc (pos+1) vs
      where
        r = vs V.! (pos + k - 1) - vs V.! pos
        len = V.length vs

main :: IO ()
main = do
  withFile "maxmin_input.txt" ReadMode $ \handle -> do
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    k <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- n <- readLn :: IO Int
    -- k <- readLn :: IO Int
    xs <- forM [1..n] $ \_ ->
      liftA (\x -> read x :: Int) (hGetLine handle)
      -- readLn :: IO Int
    let sorted = sort xs
    print . maxMin k . V.fromList $ sort xs
