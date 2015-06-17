{-
  Project Euler+ 002
  https://www.hackerrank.com/contests/projecteuler/challenges/euler002

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}
import           Control.Monad (forM_)

fib :: Integer -> Integer
fib n = g n 0 1 where
  g x a b
   | x == 0    = a
   | x == 1    = b
   | otherwise = g (x-1) b (a+b)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print . sum . filter even . takeWhile (<= n) $ map fib [1..]
