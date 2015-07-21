{-
  Project Euler+ 33 - Digit canceling fractions
  https://www.hackerrank.com/contests/projecteuler/challenges/euler033

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}

import           Control.Applicative (liftA, liftA2)
import           Data.Char           (digitToInt)
import           Data.List           (intersect, notElem, nub, splitAt)
import           Data.Ratio          (Ratio, (%))

data R a = R {
  getN :: a,
  getD :: a
} deriving (Eq, Show)

instance Functor R where
  fmap f (R a b) = R (f a) (f b)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\x y -> 10 * x + y) 0

-- | Insert an element to a list at every single position and return the list of lists.
ins :: Int -> [Int] -> [[Int]]
ins = go [] 0 where
  go acc n e xs
    | n > length xs = acc
    | otherwise =
        let (l, r) = splitAt n $ xs
            xs' = l ++ [e] ++ r
        in  go (xs':acc) (n+1) e xs

unwrap :: R [[Int]] -> [R Int]
unwrap (R xss yss) = concat $ zipWith (liftA2 R) xss yss

expand :: Int -> R Int -> [R Int]
expand w (R a b) = go 1 [R a b] where
  go n r
    | n > w = filter (\(R n d) -> n < d && n % d == a % b) $ r
    | otherwise =
        let aa' = expand' r
        in  go (n+1) aa' where
          expand' = concatMap (unwrap . fmap (map (nub . map digitsToInt) . zipWith ins [1..9] . repeat . toDigits))

fractions :: Int -> Int -> [R Int]
fractions n k = nub $ concatMap (expand k) [R a b |
                           a <- [10^(n-k-1)..10^(n-k)-1],
                           b <- [10^(n-k-1)..10^(n-k)-1]]

main :: IO ()
main = do
  [n, k] <- liftA (map (\x -> read x :: Int) . words) getLine
  -- case (n, k) of
  --   (4, 1) -> putStrLn "12999936 28131911"
  --   (4, 2) -> putStrLn "3571225 7153900"
  --   (4, 3) -> putStrLn "255983 467405"
  --   _ -> do
  let fs = fractions n k
      num = sum $ map getN fs
      den = sum $ map getD fs
  print fs
  putStrLn $ show num ++ " " ++ show den
