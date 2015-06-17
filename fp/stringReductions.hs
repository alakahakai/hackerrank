deDup :: Eq a => [a] -> [a]
deDup [] = []
deDup (x:xs) = x : deDup (filter (/= x) xs)

main :: IO ()
main = do
  s <- getLine
  putStr $ deDup s
