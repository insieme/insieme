{-# LANGUAGE LambdaCase #-}

module Insieme.Inspire.Utils (
    foldTree,
    foldAddress,
    foldTreePrune,
    foldAddressPrune,
    findDecl
) where

import Control.Applicative
import Data.List
import Data.Tree
import Insieme.Analysis.Callable as Callable
import Insieme.Inspire.NodeAddress
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR



collectCallable :: NodeAddress -> Callable.CallableSet -> Callable.CallableSet
collectCallable addr s =
  let ins e s = Set.insert (e addr) s in
   case rootLabel (getNode addr) of
    IR.Lambda -> ins Callable.Lambda s
    IR.Literal -> ins Callable.Literal s
    IR.BindExpr -> ins Callable.Closure s
    _ -> s

-- | Fold the given 'Tree'. The accumulator function takes the subtree
-- and the address of this subtree in the base tree.
foldTree :: Monoid a => (NodeAddress -> a -> a) -> Tree IR.Inspire -> a
foldTree = flip foldTreePrune noPrune

-- | Fold the given 'Tree'. The accumulator function takes the subtree
-- and the address of this subtree in the base tree.
foldAddress :: Monoid a => (NodeAddress -> a -> a) -> NodeAddress -> a
foldAddress = flip foldAddressPrune noPrune


-- | Disables pruning for 'foldTreePrune'.
noPrune :: NodeAddress -> Bool
noPrune _ = False

-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldTreePrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> Tree IR.Inspire              -- ^ initial tree
                -> a                            -- ^ accumulated result
foldTreePrune collect prune tree = foldAddressPrune collect prune (mkNodeAddress [] tree)


-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldAddressPrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> NodeAddress                  -- ^ the root of the fold operation
                -> a                            -- ^ accumulated result
foldAddressPrune collect prune addr = visit addr mempty
  where
    visit base acc = if prune base
                     then acc
                     else collect base $ visitsub base acc
    visitsub base acc = foldr visit acc (subtrees base)
    subtrees addr = [goDown i addr | i <- [0..(length . subForest . getNode $ addr) - 1]]




--
-- * Get Definition Point Analysis
--

findDecl :: NodeAddress -> Maybe NodeAddress
findDecl start = findDecl start
  where
    org = getNode start

    findDecl :: NodeAddress -> Maybe NodeAddress
    findDecl addr = case getNode addr of
        Node IR.Lambda _ -> lambda addr
        _ -> declstmt addr <|> forstmt addr <|> bindexpr addr <|>
             compstmt addr <|> nextlevel addr

    declstmt :: NodeAddress -> Maybe NodeAddress
    declstmt addr = case getNode addr of
        Node IR.DeclarationStmt [_, v] | v == org -> Just $ goDown 1 addr
        _ -> Nothing

    forstmt :: NodeAddress -> Maybe NodeAddress
    forstmt addr = case getNode addr of
        Node IR.ForStmt _ -> declstmt $ goDown 0 addr
        _ -> Nothing

    lambda :: NodeAddress -> Maybe NodeAddress
    lambda addr = case getNode addr of
        Node IR.Lambda [_, Node IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> findIndex (==org) ps
        _ -> Nothing

    bindexpr :: NodeAddress -> Maybe NodeAddress
    bindexpr addr = case getNode addr of
        Node IR.BindExpr [_, Node IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> findIndex (==org) ps
        _ -> Nothing

    compstmt :: NodeAddress -> Maybe NodeAddress
    compstmt addr = getNode <$> getParent addr >>= \case
        Node IR.CompoundStmt _ | last (getAddress addr) == 0 -> Nothing
        Node IR.CompoundStmt _ -> findDecl $ goLeft addr
        _ -> Nothing

    nextlevel :: NodeAddress -> Maybe NodeAddress
    nextlevel addr = getParent addr >>= findDecl






-- some examples
excoll a (Node n _) = Set.insert (a, n)
extree = unfoldTree
         (\i -> (i, if i>2 then [i `div` 2, i `div` 2 -1] else [])) 8
exprune s t = Seq.length s <= 2
