{-
  Mangoes
  https://www.hackerrank.com/challenges/mango

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 14th, 2015
-}
import           Control.Applicative (liftA)
import           Data.List           (sort)
import           Debug.Trace
import           System.IO

data Person = Person {
  appetite  :: Int,
  happiness :: Int
} deriving (Show)

mongoes :: Int -> Person -> Int
mongoes n (Person a h) = a + (n-1) * h

candidates :: Int -> Int -> [Person] -> Int
candidates = go 1 where
  go s e m ps
    | e == s    = e
    | otherwise =
        trace ("s:" ++ show s ++ ",e:" ++ show e) $
        let mi = ceiling $ (fromIntegral s + fromIntegral e) / 2
            r = sum . take mi . sort $ map (mongoes mi) ps
        in  if r > m
              then if e - s <= 1
                then if (sum . take s . sort $ map (mongoes s) ps) <= m
                  then s
                  else 0
                else go s mi m ps
              else go mi e m ps

main :: IO ()
main = do
  -- withFile "mongo_input.txt" ReadMode $ \handle -> do
  --   [n, m] <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
  --   aps <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
  --   hps <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
    [n, m] <- liftA (map (\x -> read x :: Int) . words) getLine
    aps <- liftA (map (\x -> read x :: Int) . words) getLine
    hps <- liftA (map (\x -> read x :: Int) . words) getLine
    print . candidates n m $ zipWith Person aps hps
