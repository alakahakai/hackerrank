{-
  Sierpinski triangles
  https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 5th, 2015
-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forever)
import           Data.List
import           Data.Maybe          (fromJust)
import           System.IO           (hFlush, stdout)
import           Text.Read           (readMaybe)

type Matrix = [String]

-- | Create a matrix with size row * col using a default Char for background
newMatrix :: Int -> Int -> Char -> Matrix
newMatrix row col c = replicate row (replicate col c)

-- | Print out a matrix
printMatrix :: Matrix -> IO ()
printMatrix = putStrLn . unlines

-- | Break a list into n lists with equal lengths
breaks :: Int -> [a] -> [[a]]
breaks _ [] = []
breaks 1 xs = [xs]
breaks n xs
  | m /= 0    = []
  | otherwise = fst $ foldr bf ([],[]) xs
    where
      m = length xs `mod` n
      l = length xs `div` n
      bf x (r, z) = let r' = x : z
                    in  if length r' == l then (r':r, []) else (r, r')

-- | Figure out the maximum number of iterations allowed
--   Iteration starts from 0, and the smallest has size (row = 1, col = 1)
maxIteration :: Matrix -> Int
maxIteration mx = floor $ logBase 2 (fromIntegral $ length mx)

-- | Take an iteration # and give out the iteration result
iteration :: Int -> Matrix -> Maybe Matrix
iteration i mx
  | i > maxIteration mx || i < 0 = Nothing
  | i == 0 = let f n rows = take ((length rows - (2*n -1)) `div` 2) rows ++
                            replicate (2*n -1) '1' ++
                            drop ((length rows + (2*n -1)) `div` 2) rows
             in Just (zipWith f [1..length mx] mx)
  | otherwise =
      case iteration (i-1) mx of
        Nothing -> Nothing
        Just mx' -> Just (fDown i mx') where
          fDown n smx = concatMap (foldr1 combineRow) grps where
            grps = map (breaks 2) (breaks (2^(n-1)) smx)
            combineRow xs ys = xs ++ zipWith cf (reverse xs) ys
            cf [] _ = []
            cf _ [] = []
            cf (p:ps) (q:qs)
              | p == '1' && q == '1' = '_' : cf ps qs
              | p == '_' && q == '_' = '_' : cf ps qs
              | otherwise            = '1' : cf ps qs

main :: IO ()
main = forever $ do
  putStr "Input number (0-5): "
  hFlush (stdout)
  ret <- (\x -> readMaybe x :: Maybe Int) <$> getLine
  case ret of
    Nothing -> main
    Just n ->
      if n <= 5 && n >= 0
        then printMatrix . fromJust . iteration n $ newMatrix 32 63 '_'
        else main
