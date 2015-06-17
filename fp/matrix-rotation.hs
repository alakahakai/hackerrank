{-
  Matrix Rotation
  https://www.hackerrank.com/challenges/matrix-rotation

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 7th, 2015
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 #-}

import           Control.Applicative ((<$>))
import           Control.Monad       (forM)
import           Data.List           (intercalate, transpose)
import           Data.Maybe
import           System.IO

type Matrix = [[String]]

newtype Layer = Layer {
  unwrap :: Matrix
} deriving Show

data FlatLayer a = FlatLayer !(Int, Int) !a
  deriving Show

instance Functor FlatLayer where
  fmap f (FlatLayer (m, n) xs) = FlatLayer (m, n) (f xs)

row :: Matrix -> Int
row = length

col :: Matrix -> Int
col = length . head

createMatrix :: [String] -> Matrix
createMatrix = map words

-- | Print out a matrix
printMatrix :: Matrix -> IO ()
printMatrix = putStr . unlines . map unwords

-- | Get the outter/top layer of the matrix
topLayer :: Matrix -> FlatLayer [String]
topLayer = flatten . Layer

-- | Flatten a layer
flatten :: Layer -> FlatLayer [String]
flatten (Layer mx) = FlatLayer (row mx, col mx) $
  head (transpose mx) ++
  case col mx of
    1 -> []
    2 -> reverse . last $ transpose mx
    _ -> case row mx of
           1 -> []
           _ -> init (tail (last mx)) ++ reverse (last (transpose mx)) ++
                reverse (init (tail (head mx)))

-- | Unflatten a layer
unflatten :: FlatLayer [String] -> Layer
unflatten (FlatLayer (m, n) ss) = Layer $
  case m of
    1 -> [head ss : reverse (tail ss)]
    2 -> [head ss : reverse (drop (n+1) ss), take n (tail ss)]
    _ -> case n of
           1 -> transpose [ss]
           2 -> transpose [take m ss, reverse (drop m ss)]
           _ -> (head ss : take (n-1) (reverse ss)) :
                ((tail . init $ transpose [take m ss, reverse (take m ((drop (m+n-2) ss)))]) ++
                [take n (drop (m-1) ss)])

joinLayers :: Layer -> Matrix -> Matrix
joinLayers (Layer ox) [] = ox
joinLayers (Layer ox) ix =
  head ox :
  ((zipWith intercalate ix (map (map (:[])) (init $ tail ox))) ++ [last ox])

rotateR :: Int -> [a]-> [a]
rotateR _ [] = []
rotateR _ [x] = [x]
rotateR 0 xs = xs
rotateR n xs = drop (length xs - n) xs ++ take (length xs - n) xs

rotateMatrix :: Int -> Matrix -> Matrix
rotateMatrix n mx
  | n <= 0    = mx
  | otherwise = foldr joinLayers [] $
      map (unflatten . fmap (\xs -> rotateR (n `mod` length xs) xs)) (allLayers mx)

-- | Get an inner matrix which is one layer in of the original matrix
innerMatrix :: Matrix -> Maybe Matrix
innerMatrix mx
  | row mx <= 2 || col mx <= 2 = Nothing
  | otherwise                  = Just . map (init . tail) . init . tail $ mx

allLayers :: Matrix -> [FlatLayer [String]]
allLayers mx = case innerMatrix mx of
                 Nothing -> [topLayer mx]
                 Just mx' -> topLayer mx : allLayers mx'

readInput :: Int -> IO [String]
readInput n = forM [1..n] $ \_ -> getLine

main :: IO ()
main = do
  withFile "matrix_rotations_input.txt" ReadMode $ \handle -> do
    ns <- words <$> hGetLine handle
    let [m, n, r] = map (\x -> read x :: Int) ns
    ss <- forM [1..m] $ \_ -> do
            hGetLine handle
    -- ns <- words <$> getLine
    -- let [m, n, r] = map (\x -> read x :: Int) ns
    -- ss <- readInput m
    let mx = createMatrix ss
    printMatrix $ rotateMatrix r mx
