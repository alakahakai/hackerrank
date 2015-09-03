{-
  High Utilization Cluster
  https://www.hackerrank.com/companies/vmware/challenges/high-utilization-cluster

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: July 16th, 2015
-}

import           Control.Applicative    (liftA)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State    (StateT, evalStateT, get, put)
import           Data.List              (delete, elem)
import           Data.Map               (Map)
import qualified Data.Map               as M

data Host = Host {
  name   :: String,
  cpu    :: Int,
  memory :: Int,
  vmList :: [VM]
} deriving (Show)

instance Eq Host where
  (Host h _ _ _) == (Host h' _ _ _) = h == h'

instance Ord Host where
  (Host h c m _) <= (Host h' c' m' _) = c <= c' || (c == c' && m <= m')

data VM = VM String Int Int
  deriving (Eq, Show)

instance Ord VM where
  (VM v c m) <= (VM v' c' m') = c <= c' || (c == c' && m <= m')

data BST a = BST a (BST a) (BST a)
           | Empty
  deriving (Eq, Show)

insert :: (Ord a) => BST a -> a -> BST a
insert Empty h = BST h Empty Empty
insert (BST g l r) h
  | h >= g = BST g l (insert r h)
  | h < g  = BST g (insert l h) r

contains :: (Ord a) => BST a -> a -> Bool
contains Empty _ = False
contains (BST g l r) h
  | h == g    = True
  | h > g     = contains r h
  | otherwise = contains l h

update :: (Ord a) => BST a -> a -> (a -> Maybe a) -> BST a
update Empty _ _ = Empty
update bst@(BST g l r) h f
  | g == h = case f h of
               Nothing -> bst
               Just h' -> BST h' l r
  | g < h  = BST g l (update r h f)
  | g > h  = BST g (update l h f) r

rm :: (Ord a) => BST a -> a -> BST a
rm Empty _ = Empty
rm (BST g l r) h
  | h == g = rmRoot (BST g l r)
  | h > g  = BST g l (rm r h)
  | h < g  = BST g (rm l h) r

biggest :: (Ord a) => BST a -> Maybe a
biggest Empty = Nothing
biggest (BST g _ Empty) = Just g
biggest (BST _ _ r) = biggest r

smallest :: (Ord a) => BST a -> Maybe a
smallest Empty = Nothing
smallest (BST g Empty _) = Just g
smallest (BST _ l _) = smallest l

rmRoot :: (Ord a) => BST a -> BST a
rmRoot Empty = Empty
rmRoot (BST g Empty r) = r
rmRoot (BST g l r) =
  BST g' (rm l g') r where
    Just g' = biggest l

listToBST :: (Ord a) => [a] -> BST a
listToBST = foldr (flip insert) Empty

placeVM :: Host -> VM -> Maybe Host
placeVM (Host hn c m vml) vm@(VM vmn c' m')
  | c < c' || m < m' = Nothing
  | otherwise        = Just $ (Host hn) (c - c') (m - m') (vm : vml)

replaceVM :: Host -> VM -> Maybe Host
replaceVM (Host hn c m vml) vm@(VM vmn c' m') =
  if elem vm vml
    then Just $ Host hn (c + c') (m + m') (delete vm vml)
    else Nothing

placeVMBST :: (Monad m) => BST Host -> VM -> StateT (Map VM Host) m (Maybe (BST Host))
placeVMBST Empty _ = return Nothing
placeVMBST (BST g@(Host gn gc gm _) l r) vm@(VM vmn c' m')
  | gc >= c' && gm >= m' = do
      m <- get
      if l == Empty
        then do
          let Just g' = placeVM g vm
          put (M.insert vm g m)
          return . Just $ BST g' l r
        else do
          ret <- placeVMBST l vm
          case ret of
            Nothing -> do
              let Just g' = placeVM g vm
              put (M.insert vm g m)
              return . Just $ BST g' l r
            Just l' -> do
              return . Just $ BST g l' r
  | otherwise = do
      m <- get
      ret <- placeVMBST r vm
      case ret of
        Nothing -> return Nothing
        Just r' -> do
          return . Just $ BST g l r'

placeVMBSTvMotion :: (MonadIO m) => BST Host -> VM -> StateT (Map VM Host) m (Maybe (BST Host))
placeVMBSTvMotion Empty _ = return Nothing
placeVMBSTvMotion hostBST vm = do
  m <- get
  let vml = filter (< v) (M.keys m)
  liftIO . print . show $ vml
  go hostBST vm vml where
    go b v vl = do
      ret <- placeVMBST b v
      case ret of
        Nothing -> do
          if null vl
            then do
              liftIO . putStrLn $ "No smaller VM to replace"
              return Nothing
            else do
              let v' = head vl
              m' <- get
              liftIO . print $ m'
              case M.lookup v' m' of
                Nothing -> go b v (tail vl)
                Just h -> do

                  liftIO . putStrLn $ "power off " ++ show v'
                  liftIO . putStrLn $ "place " ++ show v' ++ " to " ++ show h
                  go (update b h (\x -> replaceVM x v')) v (tail vl)
        r -> return r

main :: IO ()
main = do
  flip evalStateT M.empty $ do
    bst <- placeVMBSTvMotion (listToBST [Host "h1" 2 100 [], Host "h2" 2 50 []]) (VM "vm1" 1 20)
    liftIO . print $ bst
    case bst of
      Nothing -> error "failed to place VM"
      Just b -> do
        bst' <- placeVMBSTvMotion b (VM "vm2" 1 80)
        liftIO . print $ bst'
        case bst' of
          Nothing -> error "failed to place VM"
          Just b' -> do
            bst'' <- placeVMBSTvMotion b' (VM "vm3" 1 50)
            liftIO . print $ bst''
            m <- get
            liftIO . print $ m
            -- input <- getContents
            -- let hosts = getHosts input
            --     vms = getVMs input
            --     ops = getOps input
            --     vMotion = getVMotion input
