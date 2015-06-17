{-
  Prison Transport
  https://www.hackerrank.com/challenges/prison-transport

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 13th, 2015
-}
import           Control.Applicative (liftA)
import           Control.Monad       (forM, forM_)
import qualified Data.Sequence       as S (Seq, foldrWithIndex, fromList, index,
                                           update, (><))
import           System.IO

data UnionFind a = UF {
  ids      :: S.Seq a,
  sizes    :: S.Seq a,
  elemList :: S.Seq (S.Seq a)
} deriving (Show)

busCharges :: UnionFind Int -> Int
busCharges = sum' .
             getSizes
  where
    sum' = foldr (\x z -> z + ceiling (sqrt (fromIntegral x))) 0
    getSizes uf = S.foldrWithIndex (\i v acc ->
                    if i+1 == v
                      then S.index (sizes uf) i:acc
                      else acc) [] (ids uf)

findRoot :: UnionFind Int -> Int -> Int
findRoot u n
  | n == n'   = n
  | otherwise = findRoot u n'
      where n' = S.index (ids u) (n-1)

mergeElems :: UnionFind Int -> Int -> Int -> UnionFind Int
mergeElems u x y
  | x' == y'  = u
  | x' < y'   = join u x' y'
  | otherwise = join u y' x'
   where
    x' = findRoot u x
    y' = findRoot u y
    join :: UnionFind Int -> Int -> Int -> UnionFind Int
    join u' a b =
      UF {ids = S.update (b-1) (S.index (ids u') (a-1)) (ids u'),
          sizes = S.update (a-1) (S.index (sizes u') (a-1) +
                  S.index (sizes u') (b-1)) (sizes u'),
          elemList = S.update (a-1) (S.index (elemList u') (a-1) S.><
                     S.index (elemList u') (b-1)) (elemList u')}

processChains :: UnionFind Int -> [(Int, Int)] -> UnionFind Int
processChains u [] = u
processChains u ((x, y):cs) = processChains (mergeElems u x y) cs

main :: IO ()
main = do
  withFile "prison-transport_input.txt" ReadMode $ \handle -> do
    n <- liftA (\x -> read x :: Int) (hGetLine handle)
    m <- liftA (\x -> read x :: Int) (hGetLine handle)
    -- n <- liftA (\x -> read x :: Int) getLine
    -- m <- liftA (\x -> read x :: Int) getLine
    chains <- forM [1..m] $ \_ -> do
      [x, y] <- liftA (map (\x -> read x :: Int) . words) (hGetLine handle)
      -- [x, y] <- liftA (map (\x -> read x :: Int) . words) getLine
      return (x, y)
    let uf = UF {ids = S.fromList [1..n],
                 sizes = S.fromList $ replicate n 1,
                 elemList = S.fromList . map (\x -> S.fromList [x]) $ [1..n]}
        uf' = processChains uf chains
    print . busCharges $ uf'
