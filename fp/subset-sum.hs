{-
  Subset Sum
  https://www.hackerrank.com/challenges/subset-sum

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 10th, 2015
-}
import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import           Data.List           (scanr, sort)
import           Data.Ord
import qualified Data.Set            as S (Set, fromList, size, split)
import           System.IO

subset :: S.Set Int -> Int -> Int
subset ss k = let (ss', _) = S.split k ss
              in S.size ss'

main :: IO ()
main = do
  withFile "subset-sum_input2.txt" ReadMode $ \handle -> do
    _ <- hGetLine handle
    ns <- liftA (map (\x -> read x :: Int)) (liftA words (hGetLine handle))
    -- _ <- readLn :: IO Int
    -- ns <- liftA (map (\x -> read x :: Int)) (liftA words getLine)
    let sums = scanr (+) 0 $ sort ns
        ss = S.fromList sums
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- n <- readLn :: IO Int
    forM_ [1..n] $ \_ -> do
      m <- liftA (\x -> read x :: Int) (hGetLine handle)
      -- m <- readLn :: IO Int
      if m > head sums
        then putStrLn "-1"
        else print $ subset ss m
