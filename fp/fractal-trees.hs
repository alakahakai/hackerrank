{-
  Functions and Fractals - Recursive Trees
  https://www.hackerrank.com/challenges/fractal-trees

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 14th, 2015
-}
type Matrix = [String]

data Position = P {
  row :: Int,
  col :: Int
} deriving (Eq, Show)

data Tree = Tree {
  size     :: Int,
  start    :: Position,
  topLeft  :: Position,
  topRight :: Position
} deriving (Show)

-- | Create a matrix with size row * col using a default Char for background
newMatrix :: Int -> Int -> Char -> Matrix
newMatrix r c char = replicate r (replicate c char)

-- | Print out a matrix
printMatrix :: Matrix -> IO ()
printMatrix = putStrLn . unlines

-- | Give a start point and itreation #
tree :: Position -> Int -> Tree
tree (P r c) n =
  let sz = 32 `div` (2 ^ (n-1))
      r' = r - sz  + 1
  in Tree {
    size = sz,
    start = P r c,
    topLeft = P r' (c - sz `div` 2),
    topRight = P r' (c + sz `div` 2)
  }

draw :: Char -> Matrix -> Position -> Matrix
draw c mx (P x y)
  | otherwise = take x mx ++ [take y (mx !! x) ++ (c : drop (y+1) (mx !! x))] ++ drop (x+1) mx

drawTree :: Tree -> Matrix
drawTree t = foldl (draw '1') base (treeToPositions t)
              where base = newMatrix 63 100 '_'

mergeMatrix :: Matrix -> Matrix -> Matrix
mergeMatrix mx1 mx2 = zipWith mergeRow mx1 mx2
                        where mergeRow [] _ = []
                              mergeRow _ [] = []
                              mergeRow (p:ps) (q:qs)
                                | p == '1' || q == '1' = '1' : mergeRow ps qs
                                | otherwise            = '_' : mergeRow ps qs

treeToPositions :: Tree -> [Position]
treeToPositions t = take sz (iterate (\(P x y) -> P (x-1) y) (P s c)) ++
                    take sz (tail (iterate (\(P x y) -> P (x-1) (y-1)) (P e c))) ++
                    take sz (tail (iterate (\(P x y) -> P (x-1) (y+1)) (P e c)))
                      where
                        s = row (start t)
                        e = s - sz + 1
                        c = col (start t)
                        sz = size t `div` 2

iteration :: Int -> Matrix
iteration n = foldr1 mergeMatrix $ map drawTree trees
                where trees = concatMap (go (P 62 49)) [1..n]
                      go p 1 = [tree p 1]
                      go p n = let t = go p (n-1)
                               in  map (flip tree n)
                                       (map ((\(P x y) -> P (x-1) y) . topLeft) t ++
                                        map ((\(P x y) -> P (x-1) y) . topRight) t)

main :: IO ()
main = do
  n <- readLn :: IO Int
  if n > 0 && n <= 5
    then printMatrix $ iteration n
    else return ()
