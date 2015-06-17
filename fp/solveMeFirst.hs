getV :: (Int -> Bool) -> IO Int
getV p = do
  v <- getLine
  let num = read v :: Int
  if p num
    then return num
    else do
      putStrLn "Invalid input"
      getV p

main :: IO ()
main = do
    val1 <- getV (>=1)
    val2 <- getV (<=1000)
    let sum = val1 + val2
    print sum
