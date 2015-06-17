{-
  Area Under Curves and Volume of Revolving a Curve
  https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 15th, 2015
-}
import           Control.Applicative (liftA)
import           Text.Printf         (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area a b l r, volume a b l r]

area :: [Int] -> [Int] -> Int -> Int -> Double
area a b l r =
  let g x = sum $ map (\(a', b') -> fromIntegral a' * x ** (fromIntegral b')) (zip a b)
  in  (*0.001) . sum . map g $
          [x | x <- [fromIntegral l, fromIntegral l + 0.001..fromIntegral r]]

volume :: [Int] -> [Int] -> Int -> Int -> Double
volume a b l r =
  let g x = sum $ map (\(a', b') -> fromIntegral a' * x ** (fromIntegral b')) (zip a b)
  in  (*0.001) . sum . map ((\x -> pi * x ** 2) . g) $
          [x | x <- [fromIntegral l, fromIntegral l + 0.001..fromIntegral r]]

main :: IO ()
main = do
    a <- liftA (map (\x -> read x :: Int) . words) getLine
    b <- liftA (map (\x -> read x :: Int) . words) getLine
    [l, r] <- liftA (map (\x -> read x :: Int) . words) getLine
    mapM_ (printf "%.1f\n") $ solve l r a b
