{-
  Sequence full of colors
  https://www.hackerrank.com/challenges/sequence-full-of-colors

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 7th, 2015
-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)

check :: String -> Bool
check = go 0 0 0 0 where
  go r g y b []     = r == g && y == b
  go r g y b (x:xs)
    | x == 'R' = abs (r + 1 - g) <= 1 && go (r+1) g y b xs
    | x == 'G' = abs (g + 1 - r) <= 1 && go r (g+1) y b xs
    | x == 'Y' = abs (y + 1 - b) <= 1 && go r g (y+1) b xs
    | x == 'B' = abs (b + 1 - y) <= 1 && go r g y (b+1) xs

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  forM_ [1..n] $ \_ -> do
    s <- getLine
    if not (check s)
      then putStrLn "False"
      else putStrLn "True"
