{-
  Sherlock and the Maze
  https://www.hackerrank.com/challenges/sherlock-and-the-maze

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 23rd, 2015
-}
import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import           Data.Array

countArray :: Array (Int, Int, Int, Int) Integer
countArray =
  listArray ((0,0,0,0), (100,100,100,1)) $
    [f n m k o | n <- [0..100], m <- [0..100], k <- [0..100], o <- [0, 1]] where
    -- ^ n, m, k: as defined by question; o: last move, 0 is MoveDown, 1 is MoveRight.
    f 0 0 _ _ = 0
    f _ 0 0 0 = 1
    f 0 _ 0 1 = 1
    f n m k 0
      | k > 0 && n > 0 = countArray ! (n-1, m, k-1, 1) + countArray ! (n-1, m, k, 0)
      | n > 0 = countArray ! (n-1, m, k, 0)
      | otherwise = 0
    f n m k 1
      | k > 0 && m > 0 = countArray ! (n, m-1, k-1, 0) + countArray ! (n, m-1, k, 1)
      | m > 0 = countArray ! (n, m-1, k, 1)
      | otherwise = 0

count :: Int -> Int -> Int -> Integer
count 0 0 _ = 1
count n m k = foldr f 0 [0..k] where
  f k' acc = (acc + countArray ! (n, m, k', 0) + countArray ! (n, m, k', 1)) `rem` (10^9 + 7)

main :: IO ()
main = do
    t <- readLn
    forM_ [1..t] $ \_ -> do
      [n, m, k] <- liftA (map (\x -> read x :: Int) . words) getLine
      print $ count (n-1) (m-1) k
