{-
  String Mingling
  https://www.hackerrank.com/challenges/string-mingling
-}

mingle :: String -> String -> String
mingle a b = concat $ zipWith (\x y -> x : [y]) a b

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  putStrLn $ mingle a b
