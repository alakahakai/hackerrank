{-
  Filter Elements
  https://www.hackerrank.com/challenges/filter-elements

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}

import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.List           (sortBy)
import qualified Data.Map            as M (filter, fromListWith, toList)
import           Data.Ord            (compare)
import           System.IO

repeatNums :: Int -> [Int] -> [(Int, (Int, Int))]
repeatNums n = sortBy (\(a, (i, n1)) (b, (i', n2)) -> compare i i') .
               M.toList .
               M.filter (\(_, m) -> m >= n) .
               M.fromListWith combine .
               zipWith (\x y -> (y, (x, 1))) [1..] where
  combine (i, n1) (i', n2)
    | i <= i'   = (i, (n1+n2))
    | otherwise = (i', (n1+n2))

dispNums :: [(Int, (Int, Int))] -> String
dispNums [] = "-1"
dispNums xs = foldr (\(a, _) s -> if null s
                                    then show a
                                    else show a ++ " " ++ s) [] xs

main :: IO ()
main = do
  -- withFile "filter_elements_input.txt" ReadMode $ \handle -> do
  --   n <- (\x -> read x :: Int) <$> hGetLine handle
  --   putStrLn $ "n is: " ++ show n
  --       [n, rpt] <- fmap (map (\x -> read x :: Int)) $ words <$> hGetLine handle
  --       ns <- fmap (map (\x -> read x :: Int) . take n) $ words <$> hGetLine handle
  --       dispNums $ repeatNums rpt ns
  n <- (\x -> read x :: Int) <$> getLine
  forM_ [1..n] $ \_ -> do
    [n, rpt] <- fmap (map (\x -> read x :: Int)) $ words <$> getLine
    ns <- fmap (map (\x -> read x :: Int) . take n) $ words <$> getLine
    putStrLn . dispNums $ repeatNums rpt ns
