{-
  Project Euler+ 001
  https://www.hackerrank.com/contests/projecteuler/challenges/euler001

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}
import           Control.Monad (forM_)

quickSum :: Int -> Int -> Int
quickSum n e = n * e' * (e' + 1) `div` 2 where e' = e `div` n

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print $ quickSum 3 (n-1) + quickSum 5 (n-1) - quickSum 15 (n-1)
