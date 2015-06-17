{-
  Common Divisors
  https://www.hackerrank.com/challenges/common-divisors

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
{-# OPTIONS -O2 #-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           System.IO

divisors :: Int -> [Int]
divisors n = foldr (\x xs -> if n `mod` x == 0
                               then if x /= n `div` x
                                 then x:(n `div` x):xs
                                 else x:xs
                               else xs) []
                   [1..(floor . sqrt . fromIntegral) n]

main :: IO ()
main = do
  n <- liftA (\x -> read x :: Int) getLine
  forM_ [1..n] $ \_ -> do
    [l, m] <- liftA (map (\x -> read x :: Int)) (liftA words getLine)
    let s1 = S.fromList (divisors l)
        s2 = S.fromList (divisors m)
    print . length . S.toList $ S.intersection s1 s2
