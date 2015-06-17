import           Control.Applicative ((<$>))
import           Control.Monad       (mapM_)

pascal :: Integral a => [[a]]
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  if n <= 10 && n >= 2
    then let f x z = if null z then show x else show x ++ " " ++ z
         in mapM_ (putStrLn . foldr f []) . take n $ pascal
    else main
