{-
  MAC Learning
  https://www.hackerrank.com/companies/vmware/challenges/mac-learning

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 17th, 2015
-}
import           Control.Applicative       (liftA)
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Data.Char                 (toUpper)
import           Data.List                 (elem)
import qualified Data.Map.Strict           as M (Map, alter, empty, insert,
                                                 lookup)

type Port = String
type Address = String

data Action = P Port
            | Flood
            | Drop
  deriving Show

isMulticast :: Address -> Bool
isMulticast mac = toUpper (mac !! 1) `elem` "13579BDF"

destPort :: Monad m => Port -> Address -> Address -> StateT (M.Map Address Port) m Action
destPort pin da sa
  | isMulticast sa = return Drop
  | otherwise      = do
    m <- get
    put $ M.alter (const $ Just pin) sa m
    if isMulticast da
      then return Flood
      else case M.lookup da m of
        Nothing -> return Flood
        Just p -> if p == pin then return Drop else return (P p)

main :: IO ()
main = do
  n <- readLn
  flip evalStateT M.empty $
    forM_ [1..n] $ \_ -> do
      [p, da, sa] <- liftIO $ liftA words getLine
      action <- destPort p da sa
      case action of
        Drop -> liftIO $ putStrLn "drop"
        Flood -> liftIO $ putStrLn "flood"
        P p -> liftIO $ putStrLn p
