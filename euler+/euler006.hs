{-
  Project Euler+ 006 - Sum square difference
  https://www.hackerrank.com/contests/projecteuler/challenges/euler006

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 21st, 2015
-}
import           Control.Monad (forM_)

solve :: [Integer] -> Integer
solve = go 0 0 where
  go acc _ [] = acc
  go acc s (x:xs) = go (acc + 2 * s * x) (s+x) xs

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print $ solve [1..n]
