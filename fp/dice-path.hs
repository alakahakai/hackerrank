{-
  Dice Path
  https://www.hackerrank.com/challenges/dice-path

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 29th, 2015
-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_, guard)
import           System.IO

data Dice = Dice {
  top   :: !Int,
  front :: !Int,
  left  :: !Int
} deriving (Eq, Ord, Show)

validDices :: [Dice]
validDices = do
  t <- [1..6]
  f <- [1..6]
  l <- [1..6]
  let b = 7 - t
      bk = 7 - f
      r = 7 - l
      t' = min t b
      f' = min f bk
      l' = min l r
  guard (t' + f' + l' == 6)
  guard (t' * f' * l' == 6)
  return (Dice t f l)

memoizedSum :: [[[[[Int]]]]]
memoizedSum = [[[[[getSum m n t f l | l <- [0..6]] | f <- [0..6]] | t <- [0..6]] | n <- [0..]] | m <- [0..]] where
  getSum 0 _ _ _ _ = 0
  getSum _ 0 _ _ _ = 0
  getSum 1 1 1 2 3 = 1
  getSum 1 1 _ _ _ = 0
  getSum m' n' t' f' l' =
    let sum1 = memoizedSum !! (m' - 1) !! n' !! f' !! (7 - t') !! l'  -- move up
        sum2 = memoizedSum !! m' !! (n' - 1) !! (7 - l') !! f' !! t'  -- move left
    in  max (if sum1 > 0 then sum1 + t' else 0) (if sum2 > 0 then sum2 + t' else 0)

sums :: Int -> Int -> [Int]
sums m n = [memoizedSum !! m !! n !! t !! f !! l | Dice t f l <- validDices]

main :: IO ()
main = do
  -- withFile "dice-path_input3.txt" ReadMode $ \handle -> do
    -- t <- liftA (\x -> read x :: Int) (hGetLine handle)
    t <- readLn
    forM_ [1..t] $ \_ -> do
      -- [m, n] <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
      [m, n] <- liftA (map (\x -> read x :: Int) . words) getLine
      print . maximum $ sums m n
