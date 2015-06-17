{-
  Substring Searching
  https://www.hackerrank.com/challenges/kmp-fp

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 11th, 2015
-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import           Data.Array
import           Data.List           (inits, intersect, tails)
import           System.IO


data PatternTable a = PTable {
  pattern  :: Array Int a,
  jmpTable :: Array Int Int
} deriving (Show, Eq)

buildTable :: (Eq a) => [a] -> PatternTable a
buildTable xs =
  let p = array (0, length xs - 1) (zip [0..] xs)
      jmp = listArray (bounds p) $ map f (indices p)
      f 0 = -1
      f i = let k = jmp ! (i-1)
                loop j = if j > -1 && p ! (j+1) /= p ! i
                           then loop (jmp ! j)
                           else j
                l = loop k
            in if p ! (l+1) == p ! i then l+1 else l
  in PTable {pattern = p, jmpTable = jmp}

kmpSearch :: (Eq a) => [a] -> [a] -> [Int]
kmpSearch word ss =
  let pt = buildTable word
      t = array (0, length ss - 1) $ zip [0..] ss
      go acc i k
        | i >= length ss || k >= length word = reverse acc
        | otherwise =
            let l = loop k
                loop j = if j > -1 && pattern pt ! (j+1) /= t ! i
                           then loop (jmpTable pt ! j)
                           else j
            in  if pattern pt ! (l+1) == t ! i
                  then if l+1 >= length word - 1
                         then go (i-l-1:acc) (i+1) (-1)
                         else go acc (i+1) (l+1)
                  else go acc (i+1) l
  in go [] 0 (-1)

main :: IO ()
main =
  withFile "kmp-fp_input.txt" ReadMode $ \handle -> do
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- n <- readLn :: IO Int
    forM_ [1..n] $ \_ -> do
      s <- hGetLine handle
      w <- hGetLine handle
      -- s <- getLine
      -- w <- getLine
      if not . null $ w `kmpSearch` s
        then putStrLn "YES"
        else putStrLn "NO"
