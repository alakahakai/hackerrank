{-
  Missing Numbers (FP)
  https://www.hackerrank.com/challenges/missing-numbers-fp

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 9th, 2015
-}
import           Control.Applicative (liftA)
import qualified Data.Vector         as V

procList :: Int -> [Int] -> V.Vector Int
procList minV = p (V.replicate 100 0) where
  p v [] = v
  p v (x:xs) = p (V.accum (+) v [(x - minV, 1)]) xs

main :: IO ()
main = do
  _ <- liftA (\x -> read x :: Int) getLine
  list1 <- liftA words getLine
  _ <- liftA (\x -> read x :: Int) getLine
  list2 <- liftA words getLine
  let ns = map (\x -> read x :: Int) list1
      ms = map (\x -> read x :: Int) list2
      minV = min (minimum ns) (minimum ms)
      v = procList minV ns
      v' = procList minV ms
  V.mapM_ printVector
    (V.zipWith3 f v v' (V.fromList [minV..minV+100])) where
        f a b c = if b > a then c else 0
        printVector x = if x /= 0 
                          then putStr (show x ++ " ")
                          else return ()
