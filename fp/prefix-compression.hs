{-
  Prefix Compression
  https://www.hackerrank.com/challenges/prefix-compression

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 6th, 2015
-}

longestPrefix :: String -> String -> String
longestPrefix = f [] where
  f acc [] _ = reverse acc
  f acc _ [] = reverse acc
  f acc (x:xs) (y:ys)
    | x == y    = f (x:acc) xs ys
    | otherwise = reverse acc

subString :: String -> String -> (Int, String)
subString xs prefix = let s = drop (length prefix) xs
                      in  (length s, s)

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  let p = longestPrefix a b
  putStrLn $ show (length p) ++ " " ++ p
  putStrLn $ show (fst (subString a p)) ++ " " ++ snd (subString a p)
  putStrLn $ show (fst (subString b p)) ++ " " ++ snd (subString b p)
