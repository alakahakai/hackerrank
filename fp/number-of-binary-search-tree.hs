{-
  Number of Binary Search Trees
  https://www.hackerrank.com/challenges/number-of-binary-search-tree

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 9th, 2015
-}

import           Control.Applicative    (liftA)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, evalStateT, get, put)
import qualified Data.Map               as M

factorial :: Monad m => Int -> (StateT (M.Map Int Integer) m) Integer
factorial 0 = return 1
factorial n = do
  d <- get
  case M.lookup n d of
    Nothing -> do
      r' <- factorial (n - 1)
      let r = fromIntegral n * r'
      put (M.insert n r d)
      return r
    Just r -> return r

main :: IO ()
main = do
  n <- liftA (\x -> read x :: Int) getLine
  flip evalStateT M.empty $ do
    forM_ [1..n] $ \_ -> do
      m <- liftIO $ liftA (\x -> read x :: Int) getLine
      -- | Given a number n, the total number of BSTs that can be created
      --   using numbers from 1 to n: Catalan number Cn = (2n)! / (n+1)! * n!
      n1 <- factorial (2*m)
      n2 <- factorial m
      n3 <- factorial (m+1)
      liftIO $ print (n1 `div` (n2 * n3) `mod` (10^8 + 7))
