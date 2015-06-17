{-
  John and Fences
  https://www.hackerrank.com/challenges/john-and-fences

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 13th, 2015
-}

import           Control.Applicative (liftA)
import qualified Data.Sequence       as S (Seq, drop, fromList, index, length,
                                           null, take)
import           System.IO

largest :: S.Seq Int -> Int
largest ss
  | S.null ss = 0
  | S.length ss == 1 = S.index ss 0
  | otherwise = let i = snd $ findMin ss
                    l = S.take i ss
                    r = S.drop (i+1) ss
                in  maximum [largest l, S.index ss i * S.length ss, largest r]

-- | Find Minimum in a Sequence, return (Value, Index)
findMin :: Ord a => S.Seq a-> (Maybe a, Int)
findMin = go (Nothing, 0) 0 where
  go v pos ss
    | pos >= S.length ss = v
    | otherwise = case v of
                    (Nothing, p) -> go (Just (S.index ss pos), pos) (pos+1) ss
                    (Just v, p) -> go (if v < S.index ss pos
                                         then (Just v,p)
                                         else (Just (S.index ss pos), pos)) (pos+1) ss

main :: IO ()
main = do
  withFile "john-and-fences_input.txt" ReadMode $ \handle -> do
    _ <- liftA (\x -> read x :: Int) (hGetLine handle)
    ns <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
    -- _ <- liftA (\x -> read x :: Int) getLine
    -- ns <- liftA (map (\x -> read x :: Int) . words) getLine
    print . largest . S.fromList $ ns
