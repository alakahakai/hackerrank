{-
  String Compression
  https://www.hackerrank.com/challenges/string-compression

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 8th, 2015
-}
import           Data.List (group)

compress :: String -> String
compress = concatMap f . group where
  f [] = []
  f xs = let len = length xs
         in if (len == 1)
              then xs
              else head xs : show len

main :: IO ()
main = do
  s <- getLine
  putStrLn $ compress s
