{-
  Crosswords
  https://www.hackerrank.com/challenges/crosswords-101

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 6th, 2015
-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM)
import           Data.Foldable       (Foldable, foldMap)
import           Data.Function       (on)
import           Data.List           (elemIndices, foldl', groupBy, nub,
                                      permutations, sortBy, transpose)
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import           Data.Maybe

type CrossWords = [Row]
type Row = String

data Slot = R Int (Int,Int) | C Int (Int, Int)
  deriving Show

-- | Print out a matrix
printCrossWords :: CrossWords -> IO ()
printCrossWords = putStrLn . unlines

getWords :: String -> [String]
getWords = splitOn ";"

testCrossWords1 :: CrossWords
testCrossWords1 =
  ["+-++++++++",
   "+-++++++++",
   "+-++++++++",
   "+-----++++",
   "+-+++-++++",
   "+-+++-++++",
   "+++++-++++",
   "++------++",
   "+++++-++++",
   "+++++-++++"]

testWords1 :: [String]
testWords1 = getWords "LONDON;DELHI;ICELAND;ANKARA"

testCrossWords2 :: CrossWords
testCrossWords2 =
  ["+-++++++++",
   "+-++++++++",
   "+-------++",
   "+-++++++++",
   "+-++++++++",
   "+------+++",
   "+-+++-++++",
   "+++++-++++",
   "+++++-++++",
   "++++++++++"]

testWords2 :: [String]
testWords2 = getWords "AGRA;NORWAY;ENGLAND;GWALIOR"

-- | Create a map from [[a]] with the length as key
mapLength :: [[a]] -> M.Map Int [[a]]
mapLength xss = foldr1 M.union $
                  zipWith (\x y -> M.insert x y M.empty) lens grps where
  grps = groupBy ((==) `on` length) $ sortBy sf xss
  lens = map (length . head) grps
  sf x y
    | length x > length y = GT
    | length x == length y = EQ
    | otherwise = LT

lengthSlot :: Slot -> Int
lengthSlot (R _ (s, e)) = e - s + 1
lengthSlot (C _ (s, e)) = e - s + 1

-- | Find all candidate slots in a row
findSlot :: Row -> [(Int, Int)]
findSlot = f [] Nothing . elemIndices '-' where
  f rs Nothing [] = rs
  f rs (Just (s, e)) [] =
    if e > s
      then (s, e):rs
      else rs
  f rs Nothing (n:ns) = f rs (Just (n, n)) ns
  f rs (Just (s, e)) (n : ns)
    | n == e + 1 = f rs (Just (s, n)) ns
    | e > s = f ((s, e) : rs) (Just (n, n)) ns
    | otherwise = f rs (Just (n, n)) ns

-- | Find all candidate slots in the CrossWords
findSlots :: CrossWords -> [Slot]
findSlots cw = concat $ zipWith (f R) (map findSlot cw) [0..] ++
                        zipWith (f C) (map findSlot $ transpose cw) [0..] where
  f _ [] _ = []
  f cf (r:rs) n = cf n r : f cf rs n

-- | Fill all the slots with the words, giving all the possible combinations.
--   Number of slots and number of words should be equal.
fillSlots :: [String] -> [Slot] -> [[(Slot, String)]]
fillSlots ws = expand . tryFill [] (mapLength ws) where
  tryFill rs m [] = rs
  tryFill rs m (s:ss) =
    case M.lookup (lengthSlot s) m of
      Nothing -> [[]]
      Just ws -> tryFill (zip (replicate (length ws) s) ws : rs) m ss

-- | Expand from a list of lists of pairs (every pair is a possible choice for a Slot)
--  to a list of lists where each sublist has pairs for all slots and words.
expand :: [[(Slot, String)]] -> [[(Slot, String)]]
expand = foldr expand' [[]] where
  expand' rs ss = vf ((:) <$> rs <*> ss)
  vf = filter (\x -> (length . nub . map snd) x == length x)
  -- ^ Simply filter out solutions with duplicate words

-- | Fill in a slot with a possible choice to see whether success or not
fillIn :: CrossWords -> (Slot, String) -> Maybe CrossWords
fillIn cw (R row (s, e), word) =
  let mergeWords w1 w2 = mergeChars $ zf w1 w2
      zf = zipWith (\x y -> if x == '-' || x == y then Just y else Nothing)
      mergeChars cs
        | any isNothing cs = Nothing
        | otherwise        = foldMap (fmap (:[])) cs
  in case mergeWords (take (e - s + 1) (drop s (cw !! row))) word of
       Nothing -> Nothing
       Just v -> Just (take row cw ++ [row'] ++ drop (row + 1) cw) where
         row' = take s (cw !! row) ++ v ++ drop (e+1) (cw !! row)

fillIn cw (C col (s, e), word) =
  case fillIn (transpose cw) (R col (s, e), word) of
    Nothing -> Nothing
    Just v -> Just (transpose v)

-- | Validate a possible solution by trying to fill them in one by one
validate :: CrossWords -> [(Slot, String)] -> Maybe CrossWords
validate cw []     = Just cw
validate cw (s:ss) = case fillIn cw s of
                       Nothing -> Nothing
                       Just cw' -> validate cw' ss

solve :: CrossWords -> [String] -> Maybe CrossWords
solve cw words =
  case filter isJust . map (validate cw) . fillSlots words $ findSlots cw of
     [] -> Nothing
     v -> head v

printSolution :: Maybe CrossWords -> IO ()
printSolution s =
  case s of
    Nothing -> putStrLn "No solution found."
    Just v -> putStrLn $ unlines v

main :: IO ()
main = do
  printSolution $ solve testCrossWords1 testWords1
  printSolution $ solve testCrossWords2 testWords2
  cw <- forM [1..10] $ \_ -> do
    getLine
  words <- getWords <$> getLine
  printSolution $ solve cw words
