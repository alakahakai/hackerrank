{-
  Project Euler+ 008 - Largest product in a series
  https://www.hackerrank.com/contests/projecteuler/challenges/euler008

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: August 31st, 2015
-}
import           Control.Applicative (liftA)
import           Control.Monad       (forM_)

numbers :: String -> [Int]
numbers s = map (\x -> read [x] :: Int) s

maxProduct :: [Int] -> Int -> Int
maxProduct xs k
  | k == 1    = maximum xs
  | otherwise = f 0 xs where
                  f m xs
                    | length xs < k = m
                    | otherwise = let p = product (take k xs)
                                  in  if p > m
                                        then f p (tail xs)
                                        else f m (tail xs)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    [_, k] <- liftA (map (\x -> read x :: Int) . words) getLine
    ns <- liftA numbers getLine
    print $ maxProduct ns k
