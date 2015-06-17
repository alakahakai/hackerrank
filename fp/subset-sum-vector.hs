{-
  Subset Sum
  https://www.hackerrank.com/challenges/subset-sum

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 10th, 2015
-}
import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import           Data.List           (sortBy)
import           Data.Ord
import qualified Data.Vector         as V (Vector, fromList, last, length, (!))
import           System.IO

sums :: V.Vector Int -> V.Vector Int
sums = go [] 0 0 where
  go rs pos s vs
    | pos >= V.length vs = V.fromList (reverse rs)
    | otherwise = let s' = s + vs V.! pos
                  in  go (s':rs) (pos+1) s' vs

subset :: V.Vector Int -> Int -> Int
subset vs k
  | k > V.last vs = -1
  | otherwise     = go ((V.length vs - 1) `div` 2) 0 (V.length vs - 1) where
      go p s e
        | p == s        = if k <= vs V.! p then p + 1 else p + 2
        | k == vs V.! p = p + 1
        | k > vs V.! p  = go ((p + e) `div` 2) p e
        | k < vs V.! p  = go ((p + s) `div` 2) s p

main :: IO ()
main = do
  withFile "subset-sum_input2.txt" ReadMode $ \handle -> do
    _ <- hGetLine handle
    ns <- liftA (map (\x -> read x :: Int)) (liftA words (hGetLine handle))
    -- _ <- readLn :: IO Int
    -- ns <- liftA (map (\x -> read x :: Int)) (liftA words getLine)
    let sorted = V.fromList $ sortBy (flip compare) ns
        summed = sums sorted
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- n <- readLn :: IO Int
    forM_ [1..n] $ \_ -> do
      m <- liftA (\x -> read x :: Int) (hGetLine handle)
      -- m <- readLn :: IO Int
      print $ subset summed m
