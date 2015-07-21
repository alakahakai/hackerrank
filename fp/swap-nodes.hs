{-
  Swap Nodes
  https://www.hackerrank.com/challenges/swap-nodes

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 10th, 2015
-}

import           Control.Applicative       (liftA, (<$>))
import           Control.Monad             (forM, forM_)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (evalStateT, get, put)
import           Data.List                 (intersperse)

data Tree a = Branch a (Tree a) (Tree a)
            | Empty
  deriving (Eq, Show)

traverseInOrder :: Tree a -> [a]
traverseInOrder Empty = []
traverseInOrder (Branch t l r) = traverseInOrder l ++ (t : traverseInOrder r)

depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

swapLevel :: Int -> Tree a -> Tree a
swapLevel n tree = swap n 1 tree where
  swap _ _ Empty = Empty
  swap i j (Branch a l r)
    | i < j     = error $ "Wrong input: level is " ++ show i
    | i == j    = Branch a r l
    | otherwise = Branch a (swap n (j+1) l) (swap n (j+1) r)

buildTree :: [(Int, Int)] -> Tree Int
buildTree xs = go (zip [1..] xs) (Branch 1 Empty Empty) where
  go [] t = t
  go ((i, y):ys) t = go ys (updateTree i y t)
  mkBranch b
    | b == -1   = Empty
    | otherwise = Branch b Empty Empty
  updateTree _ _ Empty = Empty
  updateTree i p@(l, r) (Branch a ll rr)
    | i == a = Branch a (mkBranch l) (mkBranch r)
    | otherwise = Branch a (updateTree i p ll) (updateTree i p rr)

levels :: Int -> Int -> [Int]
levels d i = takeWhile (<= d) $ map (*i) [1..]

main :: IO ()
main = do
  n <- readLn
  ns <- forM [1..n] $ \_ ->
    (\p -> (p!!0, p!!1)) . map (\x -> read x :: Int) <$> liftA words getLine
  let tree = buildTree ns
  m <- readLn
  flip evalStateT tree $ do
    forM_ [1..m] $ \_ -> do
      i <- liftIO $ liftA (\x -> read x :: Int) getLine
      t <- get
      let t' = foldr swapLevel t (levels (depth t) i)
      liftIO . putStrLn . unwords . map show . traverseInOrder $ t'
      put t'
