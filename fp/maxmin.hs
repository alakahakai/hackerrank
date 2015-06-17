{-
  Max Min
  https://www.hackerrank.com/challenges/angry-children

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 10th, 2015
-}
{-# OPTIONS -O2 #-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM)
import           Data.List           (sort)
import           System.IO

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
    print . minimum $ zipWith (-) (drop (k-1) sorted) sorted
