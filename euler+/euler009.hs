{-
  Project Euler+ 009 - Special Pythagorean triplet
  https://www.hackerrank.com/contests/projecteuler/challenges/euler009

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: August 31st, 2015
-}
import           Control.Applicative (liftA)
import           Control.Monad       (forM_)

primitiveTriplets :: Int -> [[Int]]
primitiveTriplets n = [[2 * i * j, i ^ 2 - j ^ 2, i ^ 2 + j ^ 2] |
                          i <- [1..floor . sqrt $ fromIntegral n / 2],
                          j <- [1..i-1],
                          2 * i * j + 2 * i ^ 2 <= n]

triplets :: Int -> [[Int]]
triplets n =  concatMap f (primitiveTriplets n) where
  f t = let k = n `div` (sum t)
        in  if k < 2 then [t] else zipWith (\k' t' -> map (k' *) t') [1..k] (repeat t)

solve :: Int -> Int
solve n = let ss = filter (\t -> sum t == n) $ triplets n
          in if null ss
               then -1
               else maximum $ map product ss

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    n <- readLn
    print $ solve n
