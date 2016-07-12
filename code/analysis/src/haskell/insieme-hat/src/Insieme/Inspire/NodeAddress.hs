module Insieme.Inspire.NodeAddress (
    NodeAddress,
    mkNodeAddress,
    getAddress,
    getIndex,
    getNode,
    getParent,
    isRoot,
    getRoot,
    prettyShow,
    goUp,
    goDown,
    goLeft,
    goRight
) where

import Data.Tree
import qualified Insieme.Inspire as IR

data NodeAddress = NodeAddress [Int] (Tree IR.Inspire) (Maybe NodeAddress)

instance Eq NodeAddress where
    na@(NodeAddress a _ _) == nb@(NodeAddress b _ _) = (a == b) && (getRoot na == getRoot nb)

instance Ord NodeAddress where
    (NodeAddress a _ _) <= (NodeAddress b _ _) = a <= b

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> Tree IR.Inspire -> NodeAddress
mkNodeAddress xs tree = mkNodeAddress' xs tree Nothing

-- | Create a 'NodeAddress' from a list of indizes, a node and a parent
-- 'NodeAddress'.
mkNodeAddress' :: [Int] -> Tree IR.Inspire -> Maybe NodeAddress -> NodeAddress
mkNodeAddress' xs tree parent = mkNodeAddress' xs [] tree parent
  where
    mkNodeAddress' []     ys tree             parent = NodeAddress ys tree parent
    mkNodeAddress' (x:xs) ys tree@(Node _ ns) parent = mkNodeAddress' xs (ys ++ [x]) (ns !! x) na
      where
        na = Just $ NodeAddress ys tree parent

getAddress :: NodeAddress -> [Int]
getAddress (NodeAddress na _ _) = na

getIndex :: NodeAddress -> Int
getIndex a = last $ getAddress a

getNode :: NodeAddress -> Tree IR.Inspire
getNode (NodeAddress _ node _) = node

getParent :: NodeAddress -> Maybe NodeAddress
getParent (NodeAddress _ _ parent) = parent

isRoot :: NodeAddress -> Bool
isRoot (NodeAddress _ _ Nothing ) = True
isRoot (NodeAddress _ _ _       ) = False 

getRoot :: NodeAddress -> Tree IR.Inspire
getRoot (NodeAddress _ n Nothing      ) = n
getRoot (NodeAddress _ _ (Just parent)) = getRoot parent

prettyShow :: NodeAddress -> String
prettyShow (NodeAddress na _ _ ) = '0' : concat ['-' : show x | x <- na]

goUp :: NodeAddress -> NodeAddress
goUp na@(NodeAddress _ _ Nothing  ) = na
goUp    (NodeAddress _ _ (Just na)) = na

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs (Node _ ns) _)= NodeAddress (xs ++ [x]) (ns !! x) (Just parent)

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _            ) | last xs == 0 = na
goLeft na@(NodeAddress xs _ Nothing      ) = na
goLeft    (NodeAddress xs _ (Just parent)) = goDown (last xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ Nothing      ) = na
goRight    (NodeAddress xs _ (Just parent)) = goDown (last xs + 1) parent
