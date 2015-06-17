{-
  Jumping Bunnies
  https://www.hackerrank.com/challenges/jumping-bunnies
-}
import           Control.Applicative ((<$>))

minMultiple :: Integral a => a -> a -> a
minMultiple a b = a * b `div` (gcd a b)

main :: IO ()
main = do
  n <- (\x -> read x :: Int) <$> getLine
  ns <- words <$> getLine
  print . foldr1 minMultiple . map (\x -> read x :: Int) $ take n ns
