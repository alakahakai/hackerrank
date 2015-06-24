{-
  Diagonal Difference
  https://www.hackerrank.com/challenges/diagonal-difference

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 20th, 2015
-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM)
import           Debug.Trace

type Matrix = [[Int]]

row :: Matrix -> Int
row = length

col :: Matrix -> Int
col = length . head

-- | Get an inner matrix which is one layer in of the original matrix
innerMatrix :: Matrix -> Maybe Matrix
innerMatrix mx
  | row mx <= 2 || col mx <= 2 = Nothing
  | otherwise                  = Just . map (init . tail) . init . tail $ mx

diffSum :: Matrix -> Int
diffSum = go 0 where
  go acc mx =
    let s = head (head mx) - last (head mx) + last (last mx) - head (last mx)
    in  case innerMatrix mx of
          Nothing -> abs (s + acc)
          Just mx' -> go (s + acc) mx'

main :: IO ()
main = do
  n <- readLn
  mx <- forM [1..n] $ \_ -> do
    liftA (map (\x -> read x :: Int) . words) getLine
  print $ diffSum mx
