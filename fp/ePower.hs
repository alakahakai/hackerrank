{-
  Evaluate e^x using the formula: 1 + x + x2/2! + x3/3! + x4/4! + ...
  https://www.hackerrank.com/challenges/eval-ex
-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.Maybe          (fromJust)
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

ePower :: Double -> Double
ePower x = sum . take 10 $ f x where
  f x = map (\n -> x ** (fromIntegral n) / fromIntegral (factorial n)) [0..]

fourDecimal :: Double -> String
fourDecimal = printf "%.4f"

main :: IO ()
main = do
  ret <- (\x -> readMaybe x :: Maybe Int) <$> getLine
  case ret of
    Nothing -> main
    Just n -> do
      forM_ [1..n] $ \_ -> do
        d <- (\x -> readMaybe x :: Maybe Double) <$> getLine
        putStrLn . fourDecimal $ ePower (fromJust d)
