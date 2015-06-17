{-
  String-o-Permute
  https://www.hackerrank.com/challenges/string-o-permute
-}

import           Control.Applicative ((<$>))
import           Control.Monad       (forM)

swap :: String -> String
swap = swap' [] where
  swap' rs [] = reverse rs
  swap' rs (x:y:ys) = swap' (x:y:rs) ys

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  ss <- forM [1..n] $ \_ -> do
    getLine
  mapM_ putStrLn (map swap ss)
