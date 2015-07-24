import Control.Monad
import Data.Array

limit = 100

table0 = listArray ((1, 1, 0), (limit, limit, 1)) $
    map f [(n, m, d) | n <- [1..limit], m <- [1..limit], d <- [0, 1]] where
        f (1, 1, 0) = 1
        f (1, 1, 1) = 0
        f (1, m, 0) = 1
        f (n, 1, 1) = 1
        f _ = 0

transition prev = next where
    next = listArray ((1, 1, 0), (limit, limit, 1)) $
        map f [(n, m, d) | n <- [1..limit], m <- [1..limit], d <- [0, 1]] where
            f (1, _, _) = 0
            f (_, 1, _) = 0
            f (n, m, 0) = (next ! (n, m-1, 0) + prev ! (n, m-1, 1)) `mod` p
            f (n, m, 1) = (next ! (n-1, m, 1) + prev ! (n-1, m, 0)) `mod` p

tables = iterate transition table0

p = 1000000007

main = do
    t <- readLn
    replicateM_ t $ do
        [n, m, k] <- liftM (map read . words) getLine
        print $ foldl (\acc t -> (acc + t ! (n, m, 0) + t ! (n, m, 1)) `mod` p) 0 $ take (k+1) tables
