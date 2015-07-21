{-
  Project Euler+ 005 - Sum square difference
  https://www.hackerrank.com/contests/projecteuler/challenges/euler005

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}
import           Control.Monad (forM_)

sm :: [Integer] -> Integer
sm = f' 1 where
  f' n []     = n
  f' n (x:xs) = let m = gcd n x
                in  f' (n * (x `div` m)) xs

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print $ sm [1..n]
