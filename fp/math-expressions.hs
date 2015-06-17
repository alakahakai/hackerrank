{-
  Math Expressions
  https://www.hackerrank.com/challenges/expressions

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative (liftA2, (<$>), (<*>))
import           Data.List           (permutations)
import           Data.Maybe
import           System.IO

data Token = I !Integer | Add | Mul | Sub
  deriving Show

evalExpr :: [Token] -> Maybe Integer
evalExpr [] = Nothing
evalExpr [I n] = Just n
evalExpr (x:op:y:ys) =
  case eval x op y of
    Nothing -> Nothing
    Just v -> evalExpr (v:ys)
    where
      eval (I n) Add (I m) = Just $ I (n+m)
      eval (I n) Mul (I m) = Just $ I (n*m)
      eval (I n) Sub (I m) = Just $ I (n-m)
      eval _ _ _ = Nothing

genSolution :: [Integer] -> [[Token]]
genSolution [] = []
genSolution ns =
  map (++ [I (last ns)]) .
  foldr (liftA2 (++)) [[]] $
  zipWith (liftA2 f) (map (\x -> [I x]) (init ns)) operators where
    operators = concat $ replicate (length ns-1 `div` length mixops + 1) mixops
    mixops = permutations [Add, Mul, Sub]
    f n op = [n, op]

-- | Return the text of the first solution that meets the requirements
evalSolution :: Integer -> [Integer] -> Maybe String
evalSolution n ns = eval' $ genSolution ns where
  eval' [] = Nothing
  eval' (e:es) =
    case evalExpr e of
      Nothing -> eval' es
      Just r ->
        if r `mod` 101 == 0 && length (show r) <= fromIntegral (10*n)
          then Just (showSolution e)
          else eval' es

showSolution :: [Token] -> String
showSolution [] = []
showSolution (I n:ts) = show n ++ " " ++ showSolution ts
showSolution (Add:ts) = "+ " ++ showSolution ts
showSolution (Mul:ts) = "* " ++ showSolution ts
showSolution (Sub:ts) = "- " ++ showSolution ts

main :: IO ()
main = do
  withFile "math-expressions_input1.txt" ReadMode $ \handle -> do
    n <- (\x -> read x :: Integer) <$> hGetLine handle
    s <- words <$> hGetLine handle
    -- n <- (\x -> read x :: Integer) <$> getLine
    -- s <- words <$> getLine
    let ns = map (\x -> read x :: Integer) s
    case evalSolution n ns of
      Nothing -> putStrLn "No solution found"
      Just str -> putStrLn str
