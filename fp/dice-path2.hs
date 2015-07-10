{-
  Dice Path
  https://www.hackerrank.com/challenges/dice-path

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 29th, 2015
-}

import           Control.Applicative              (liftA)
import           Control.Monad                    (forM_, guard)
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict              as HM
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

sumPath :: Monad m => (Int -> Int -> Dice -> m Int) -> Int -> Int -> Dice -> m Int
sumPath _ 1 1 (Dice 1 2 3) = return 1
sumPath _ 0 _ _ = return 0
sumPath _ _ 0 _ = return 0
sumPath _ 1 1 _ = return 0
sumPath mf m n (Dice t f l) = do
  sum1 <- mf (m-1) n (Dice f (7-t) l)
  sum2 <- mf m (n-1) (Dice (7-l) f t)
  return $ max (if sum1 > 0 then sum1 + t else 0) (if sum2 > 0 then sum2 + t else 0)

memoizedSum :: Int -> Int -> Dice -> Int
memoizedSum m n (Dice t f l) = flip evalState HM.empty $ do
   s <- sumPath recF m n (Dice t f l)
   return s
     where
        recF m' n' (Dice t' f' l') = do
           v <- HM.lookup (m',n',t',f',l') <$> get
           case v of
              Just v' -> return v'
              Nothing -> do
                 v' <- sumPath recF m' n' (Dice t' f' l')
                 modify $ HM.insert (m',n',t',f',l') v'
                 return v'

sums :: Int -> Int -> [Int]
sums m n = map (memoizedSum m n) validDices

main :: IO ()
main = do
  withFile "dice-path_input3.txt" ReadMode $ \handle -> do
    t <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- t <- readLn
    forM_ [1..t] $ \_ -> do
      [m, n] <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
      -- [m, n] <- liftA (map (\x -> read x :: Int) . words) getLine
      print . maximum $ sums m n
