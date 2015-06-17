
{-
  Huge GCD
  https://www.hackerrank.com/challenges/huge-gcd-fp

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 9th, 2015
-}
import           Control.Applicative    (liftA)
import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, evalStateT, get, put)
import           Data.List              (foldl1')
import qualified Data.Vector            as V (Vector, fromList, length, modify,
                                              toList, (!))
import qualified Data.Vector.Mutable    as V (write)
import           System.IO

productMod :: Int -> [Int] -> Int
productMod _ [] = 1
productMod n ns = foldl1' (\x y -> let p = x * y
                                in  if p > n then p `mod` n else p) ns

main :: IO ()
main = do
  -- withFile "huge-gcd-fp_input.txt" ReadMode $ \handle -> do
  --   _ <- liftA (\x -> read x :: Int) (hGetLine handle)
  --   list1 <- liftA words (hGetLine handle)
  --   _ <- liftA (\x -> read x :: Int) (hGetLine handle)
  --   list2 <- liftA words (hGetLine handle)
    _ <- liftA (\x -> read x :: Int) getLine
    list1 <- liftA words getLine
    _ <- liftA (\x -> read x :: Int) getLine
    list2 <- liftA words getLine
    let nv = V.fromList $ map (\x -> read x :: Int) list1
        mv = V.fromList $ map (\x -> read x :: Int) list2
    flip evalStateT (nv, mv, []) $ do
      forM_ [0..V.length nv-1] $ \i ->
        forM_ [0..V.length mv-1] $ \j -> do
          (numerV, denomV, gcds) <- get
          let v1 = numerV V.! i
              v2 = denomV V.! j
          when (gcd v1 v2 /= 1) $ do
            let nv' = V.modify (\v -> V.write v i (v1 `div` gcd v1 v2)) numerV
                mv' = V.modify (\v -> V.write v j (v2 `div` gcd v1 v2)) denomV
            put (nv', mv', gcd v1 v2 : gcds)
      (finalV1, finalV2, gcds) <- get
      liftIO . print $ productMod (10^9 + 7) gcds
