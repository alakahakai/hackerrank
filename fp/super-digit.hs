{-
  Super Digit
  https://www.hackerrank.com/challenges/super-digit

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative (liftA)
import           Data.Char           (digitToInt)
import           System.IO

superDigit :: String -> Integer
superDigit = go 0 where
  go !acc [] = if acc < 10
                 then acc
                 else go 0 (show acc)
  go !acc !(n:ns) = go (acc + fromIntegral (digitToInt n)) ns

main :: IO ()
main = do
  -- withFile "super-digit_input.txt" ReadMode $ \handle -> do
  --   [n, k] <- liftA words (hGetLine handle)
    [n, k] <- liftA words getLine
    let kn = read k :: Int
    print $ superDigit (show (superDigit n * (fromIntegral kn)))
