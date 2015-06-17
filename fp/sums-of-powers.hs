{-
  Sum of Powers
  https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 9th, 2015
-}
import           Control.Applicative (liftA)

searchPowers :: Int -> [Int] -> Int
searchPowers = go where
  go 0 _      = 1
  go _ []     = 0
  go t (x:xs) = if x > t
                  then go t xs
                  else go (t-x) xs + go t xs

powers :: Int -> Int -> Int
powers m n = searchPowers m cs where
    cs = reverse . takeWhile (<= m) . map (^n) $ [1..]

main :: IO ()
main = do
  m <- liftA (\x -> read x :: Int) getLine
  n <- liftA (\x -> read x :: Int) getLine
  print $ powers m n
