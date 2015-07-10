{-
  Bitter Chocolate
  https://www.hackerrank.com/challenges/bitter-chocolate

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 9th, 2015
-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_)

choices = [[[win r1 r2 r3 | r3 <- [0..25]] | r2 <- [0..25]] | r1 <- [0..25]] where
  win 0 _ _ = False
  win 1 0 0 = False
  win 1 1 0 = True
  win 1 1 1 = True
  win x y z = moveRow1 || moveRow2 || moveRow3 where
    moveRow1 = any not [choices !! x' !! min x' y  !! min x' z | x' <- [1..x-1]]
    -- ^ One move on row1, look for False move so opponent loses
    moveRow2 = any not [choices !! x !! y' !! min y' z | y' <- [0..y-1]]
    -- ^ One move on row3
    moveRow3 = any not [choices !! x !! y !! z' | z' <- [0..z-1]]
    -- ^ One move on row3

solve :: Int -> Int -> Int -> Bool
solve r1 r2 r3 = choices !! r1 !! r2 !! r3

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \_ -> do
    [r1, r2, r3] <- liftA (map (\x -> read x :: Int) . words) getLine
    if solve r1 r2 r3
      then putStrLn "WIN"
      else putStrLn "LOSE"
