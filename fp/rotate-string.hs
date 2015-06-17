import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)

rotateL :: Int -> String -> String
rotateL _ [] = []
rotateL 0 xs = xs
rotateL n (x:xs) = rotateL (n-1) (xs ++ [x])

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  forM_ [1..n] $ \_ -> do
    s <- getLine
    putStrLn . foldl1 (\a b -> a ++ " " ++ b) $
      zipWith rotateL [1..] (replicate (length s) s)
