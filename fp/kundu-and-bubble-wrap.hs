{-
  Kundu And Bubble Wrap
  https://www.hackerrank.com/challenges/kundu-and-bubble-wrap

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 14th, 2015
-}
import           Control.Applicative (liftA)

main :: IO ()
main = do
  ns <- liftA (map (\x -> read x :: Int) . words) getLine
  let n = product ns
  print . sum $ map (\x -> (fromIntegral n) / (fromIntegral x)) [1..n]
