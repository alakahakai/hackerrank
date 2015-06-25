{-
  Minimum Multiple
  https://www.hackerrank.com/challenges/minimum-multiple

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 23rd, 2015
-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative    (liftA)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (evalStateT, get, put)
import           Data.Array.IArray
import           System.IO

data QueryUpdate = Q !Int !Int
                 | U !Int !Integer
  deriving (Read, Show)

data SegTree a =
  SegTreeEmpty |
  SegTreeNode {
    value      :: !a,
    range      :: (Int, Int),
    leftChild  :: SegTree a,
    rightChild :: SegTree a
  } |
  SegTreeLeaf {
    value :: !a,
    index :: !Int
  }
  deriving (Eq, Show)

mkSegTree :: (a -> a -> a) -> Array Int a -> SegTree a
mkSegTree f arr = uncurry go (bounds arr) where
  go l r
    | l == r    = SegTreeLeaf (arr ! l) l
    | otherwise = let mid = (l + r) `div` 2
                      lChild = go l mid
                      rChild = go (mid+1) r
                  in  SegTreeNode (f (value lChild) (value rChild))
                                  (l, r)
                                  lChild rChild

query :: (Int, Int) -> SegTree Integer -> Integer
query _ SegTreeEmpty = 1
query (l, r) (SegTreeLeaf v idx) = if idx >= l && idx <= r then v else 1
query (l, r) (SegTreeNode v (l', r') lt rt)
  | l' > r || r' < l = 1
  | l' >= l && r' <= r = v
  | otherwise = minMultiple (query (l, r) lt) (query (l, r) rt)

update :: Int -> Integer -> SegTree Integer -> SegTree Integer
update _ _ SegTreeEmpty = SegTreeEmpty
update i j o@(SegTreeLeaf v idx) = if i == idx
                                     then SegTreeLeaf (v * j) idx
                                     else o
update i j o@(SegTreeNode v (l, r) lt rt)
  | i >= l && i <= (l+r) `div` 2 = let lChild = update i j lt
                                   in  SegTreeNode (minMultiple (value lChild) (value rt))
                                       (l, r)
                                       lChild rt
  | i > (l+r) `div` 2 && i <= r = let rChild = update i j rt
                                   in  SegTreeNode (minMultiple (value lt) (value rChild))
                                       (l, r)
                                       lt rChild
  | otherwise = o

minMultiple :: Integer -> Integer -> Integer
minMultiple !x !y = let m = gcd x y
                    in  x * (y `div` m)

main :: IO ()
main = do
  withFile "minimum-multiple_input0.txt" ReadMode $ \handle -> do
    _ <- hGetLine handle
    ns <- liftA (map (\x -> read x :: Integer) . words) (hGetLine handle)
    let arr = listArray (0, length ns-1) ns
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- _ <- getLine
    -- ns <- liftA (map (\x -> read x :: Integer) . words) getLine
    -- n <- readLn
    flip evalStateT (mkSegTree minMultiple arr) $ do
      forM_ [1..n] $ \_ -> do
        -- qr <- liftIO $ liftA (\x -> read x :: QueryUpdate) getLine
        qr <- liftIO $ liftA (\x -> read x :: QueryUpdate) (hGetLine handle)
        st <- get
        case qr of
          Q i j -> do
            liftIO . print $ query (i, j) st `mod` (10^9+7)
          U i j -> do
            put (update i j st)
