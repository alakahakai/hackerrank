{-
  Pentagonal Numbers
  https://www.hackerrank.com/challenges/pentagonal-numbers

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 #-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import           Control.Monad.State
import qualified Data.Map            as M
import           System.IO

pentagonal :: M.Map Int Int -> Int -> (M.Map Int Int, Int)
pentagonal d 1 = (d, 1)
pentagonal d x =
  let r = (x * (3 * x - 1)) `div` 2
  in (M.insert x r d, r)

main :: IO ()
main = do
  -- withFile "pentagonal-numbers_input.txt" ReadMode $ \handle -> do
  --   n <- liftA (\x -> read x :: Int) (hGetLine handle)
    n <- liftA (\x -> read x :: Int) getLine
    flip evalStateT M.empty $ do
      forM_ [1..n] $ \_ -> do
        -- m <- liftIO $ liftA (\x -> read x :: Int) (hGetLine handle)
        m <- liftIO $ liftA (\x -> read x :: Int) getLine
        d <- get
        case M.lookup m d of
          Nothing -> do
            let (d', v) = pentagonal d m
            put d'
            liftIO $ putStrLn (show v)
          Just v -> do
            liftIO $ putStrLn (show v)
