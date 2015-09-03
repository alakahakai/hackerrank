{-
  Logical Hub Controller
  https://www.hackerrank.com/companies/vmware/challenges/logical-hub-controller

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: Sept 2nd, 2015
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (liftA)
import           Data.List           (nub)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Text           (Text)
import qualified Data.Text           as T (concat, intercalate, lines, pack,
                                           splitOn, strip)
import qualified Data.Text.IO        as T (getContents, putStrLn)

data Port = Port {
  host    :: Text,
  portNum :: Text
} deriving (Show)

data BridgeDomain = BD {
  name     :: Text,
  portList :: [Port]
} deriving (Show)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

parseConfig :: [Text] -> [BridgeDomain]
parseConfig cs = M.elems $ foldr parsef M.empty cs where
  parsef conf m =
    let h:ps = T.splitOn " " conf
        pp = zip (map (T.pack . show) [0..]) ps
        m' = foldr f m pp where
          f (p, b) lm =
            case M.lookup b lm of
              Nothing -> M.insert b (BD b [Port h p]) lm
              Just r -> M.alter (\(Just x) -> Just (BD b (Port h p : portList x))) b lm
    in m'

wiring :: BridgeDomain -> [Text]
wiring bd =
  let portPairs = combinations 2 $ portList bd
  in nub $ concatMap processPair portPairs where
    processPair [p1, p2] =
      if host p1 == host p2
        then [T.intercalate " " ["PORT_TO_PORT", host p1, portNum p1, portNum p2],
              T.intercalate " " ["PORT_TO_PORT", host p1, portNum p2, portNum p1]]
        else [T.intercalate " " ["PORT_TO_TUNNEL", host p1, portNum p1, host p2, name bd],
              T.intercalate " " ["TUNNEL_TO_PORT", host p2, name bd, portNum p2],
              T.intercalate " " ["PORT_TO_TUNNEL", host p2, portNum p2, host p1, name bd],
              T.intercalate " " ["TUNNEL_TO_PORT", host p1, name bd, portNum p1]]

main :: IO ()
main = do
  cs <- liftA (map T.strip . T.lines) T.getContents
  mapM_ T.putStrLn . concatMap wiring $ parseConfig cs
