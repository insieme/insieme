module Insieme.Inspire.NodeAddress (
    NodeAddress,
    mkNodeAddress,
    getAddress,
    getIndex,
    getIR,
    getNode,
    getParent,
    isRoot,
    getRoot,
    getRootIR,
    prettyShow,
    goUp,
    goDown,
    goLeft,
    goRight,
    isBuiltin
) where

import Data.Maybe
import Data.List (foldl')
import Data.Tree
import qualified Data.Map as Map
import qualified Insieme.Inspire as IR

data NodeAddress = NodeAddress [Int] IR.Inspire (Maybe NodeAddress)

instance Eq NodeAddress where
    na@(NodeAddress a _ _) == nb@(NodeAddress b _ _) = (a == b) && (getRoot na == getRoot nb)

instance Ord NodeAddress where
    (NodeAddress a _ _) <= (NodeAddress b _ _) = a <= b

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> IR.Inspire -> NodeAddress
mkNodeAddress xs ir = foldl' (flip goDown) (NodeAddress [] ir Nothing) xs

getAddress :: NodeAddress -> [Int]
getAddress (NodeAddress na _ _) = na

getIndex :: NodeAddress -> Int
getIndex a = last $ getAddress a

getIR :: NodeAddress -> IR.Inspire
getIR (NodeAddress _ ir _) = ir

getNode :: NodeAddress -> Tree IR.NodeType
getNode = IR.getTree . getIR

getParent :: NodeAddress -> Maybe NodeAddress
getParent (NodeAddress _ _ parent) = parent

isRoot :: NodeAddress -> Bool
isRoot (NodeAddress _ _ Nothing ) = True
isRoot (NodeAddress _ _ _       ) = False

getRoot :: NodeAddress -> Tree IR.NodeType
getRoot = IR.getTree . getRootIR

getRootIR :: NodeAddress -> IR.Inspire
getRootIR (NodeAddress _ ir Nothing      ) = ir
getRootIR (NodeAddress _ _  (Just parent)) = getRootIR parent

prettyShow :: NodeAddress -> String
prettyShow (NodeAddress na _ _ ) = '0' : concat ['-' : show x | x <- na]

goUp :: NodeAddress -> NodeAddress
goUp na@(NodeAddress _ _ Nothing  ) = na
goUp    (NodeAddress _ _ (Just na)) = na

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs ir _) = NodeAddress (xs ++ [x]) ir' (Just parent)
  where
    ir' = ir{IR.getTree = (subForest $ IR.getTree ir) !! x}

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _            ) | last xs == 0 = na
goLeft na@(NodeAddress xs _ Nothing      ) = na
goLeft    (NodeAddress xs _ (Just parent)) = goDown (last xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ Nothing      ) = na
goRight    (NodeAddress xs _ (Just parent)) = goDown (last xs + 1) parent

lookupBuiltin :: NodeAddress -> String -> Maybe (Tree IR.NodeType)
lookupBuiltin addr needle = Map.lookup needle (IR.getBuiltins $ getIR addr)

isBuiltin :: NodeAddress -> String -> Bool
isBuiltin addr needle = fromMaybe False $ (== getNode addr) <$> lookupBuiltin addr needle
