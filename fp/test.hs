import System.IO

count = [[[[[getCount mm nn tt ll ff | ff <- [0..6]] | ll <- [0..6]] | tt <- [0..6]] | nn <- [0..]] | mm <- [0..]]
    where getCount 0 _ _ _ _ = 0
          getCount _ 0 _ _ _ = 0
          getCount 1 1 1 3 2 = 1
          getCount 1 1 _ _ _ = 0
          getCount m n t l f = let
            c1 = count !! (m - 1) !! n !! f !! l !! (7 - t)
            c2 = count !! m !! (n - 1) !! (7 - l) !! t !! f
            p1 = if c1 > 0 then c1 + t else 0
            p2 = if c2 > 0 then c2 + t else 0
            in max p1 p2
                    
calc :: Int -> Int -> Int
calc m n = maximum [if check t l f then count !! m !! n !! t !! l !! f else 0 | t <- [1..6], l <- [1..6], f <- [1..6]]
          where check t l f = let 
                    tt = min (7 - t) t
                    ll = min (7 - l) l
                    ff = min (7 - f) f
                    in if tt + ll + ff == 6 && tt * ll * ff == 6 then True else False
   
solve :: [[Int]] -> [Int]
solve ([t] : arr) = map (\[m, n] -> calc m n) arr

main :: IO ()
main = do 
  withFile "dice-path_input3.txt" ReadMode $ \handle -> do
    hGetContents handle >>= mapM_ putStrLn . map show . solve . map (map read . words) . lines
