{-
  Valid BST
  https://www.hackerrank.com/challenges/valid-bst

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 22nd, 2015
-}

import           Control.Applicative (liftA)
import           Control.Monad       (forM_)

data Stack a = Empty
             | StackElem a (Stack a)
  deriving (Eq, Show)

empty :: Stack a
empty = Empty

push :: a -> Stack a -> Stack a
push x Empty = StackElem x Empty
push x st = StackElem x st

pop :: Stack a -> (Maybe a, Stack a)
pop Empty = (Nothing, Empty)
pop (StackElem x st) = (Just x, st)

peek :: Stack a -> Maybe a
peek = fst . pop

isValidBST :: [Int] -> Bool
isValidBST = go empty (minBound :: Int) where
  go _ _ [] = True
  go st minV (x:xs)
    | x < minV = False
    | st == Empty = go (push x st) minV xs
    | otherwise =
      let Just r = peek st
      in if r < x
           then let (Just minV', st') = pop st
                in go st' minV' (x : xs)
           else go (push x st) minV xs

main :: IO ()
main = do
  n <- readLn
  forM_ [1..n] $ \_ -> do
    _ <- getLine
    ns <- liftA (map (\x -> read x :: Int) . words) getLine
    if isValidBST ns then putStrLn "YES" else putStrLn "NO"
