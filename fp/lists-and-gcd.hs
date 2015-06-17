{-
  List and gcd
  https://www.hackerrank.com/challenges/lists-and-gcd
-}

import           Control.Applicative ((<$>))
import           Control.Monad       (forM)
import           Data.Maybe

newtype Power = Power (Int, Int)
  deriving Show

readNumbers :: String -> [Power]
readNumbers s = go [] (map (\x -> read x :: Int) $ words s) where
  go ps [] = ps
  go ps (x:y:ys) = go (Power (x,y) : ps) ys

listGcd :: [[Power]] -> [Power]
listGcd = foldr1 reduce

reduce :: [Power] -> [Power] -> [Power]
reduce = reduce' [] where
  reduce' rs _ [] = rs
  reduce' rs [] _ = rs
  reduce' rs (x:xs) ys = reduce' (f x ys ++ rs) xs ys where
    f x [] = []
    f x (y:ys) = case reducePower x y of
                   Nothing -> f x ys
                   Just v -> [v]

reducePower :: Power -> Power -> Maybe Power
reducePower (Power (x,y)) (Power (x',y'))
  | x == x' && y > y' = Just (Power (x, y'))
  | x == x' && y < y' = Just (Power (x, y))
  | x == x' && y == y' = Just (Power (x, y))
  | otherwise = Nothing

showGcd :: [Power] -> String
sjowGcd [] = []
showGcd [Power (a, b)] = show a ++ " " ++ show b
showGcd ((Power (a, b)):gs) = show a ++ " " ++ show b ++ " " ++ showGcd gs

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  ns <- forM [1..n] $ \_ -> do
    getLine
  putStrLn . showGcd . listGcd $ map readNumbers ns
