import           Control.Applicative
import           Control.Monad
import           System.IO

data KMP a = KMP { done :: Bool, next :: (a -> KMP a) }

makeTable xs = table
    where table = makeTable' xs (const table)

makeTable' [] failure = KMP True failure
makeTable' (x:xs) failure = KMP False test
    where test c = if c == x then success else failure c
          success = makeTable' xs (next (failure x))


isSubstringOf pat text = match (makeTable pat) text
    where match table [] = done table
          match table (y:ys) = done table || match (next table y) ys


solve [] = []
solve (t:p:xs) = (if isSubstringOf p t then "YES" else "NO") : solve xs

main =
  withFile "kmp-fp_input2.txt" ReadMode $ \handle -> do
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- n <- readLn :: IO Int
    forM_ [1..n] $ \_ -> do
      s <- hGetLine handle
      w <- hGetLine handle
      -- ms <- getLine
      -- ss <- getLine
      mapM putStrLn $ solve [s,w]
