{-
  Project Euler+ 004 - Largest palindrome product
  https://www.hackerrank.com/contests/projecteuler/challenges/euler004

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}
import           Control.Monad (forM_)

palindrome :: Int -> Bool
palindrome n = p (show n) where
  p xs = xs == reverse xs

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print . maximum . filter palindrome $
      [x * y | x <- [100..999], y <- [x+1..999], x * y < n]
