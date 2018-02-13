{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Insieme.Inspire.Visit (
    Pruning(..),
    collectAll,
    collectAllPrune,
    collectAllPrunePaths,
    collectAllPaths'Naive,
    collectAddr,
    foldTree,
    foldAddress,
    foldTreePrune,
    foldAddressPrune,
    findDecl,
) where

import Control.Applicative
import Data.List
import Insieme.Inspire.Visit.NodeMap (NodeMap)
import qualified Insieme.Inspire.Visit.NodeMap as NodeMap
import Insieme.Inspire.Visit.NodePaths
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.NodePath
import qualified Insieme.Inspire.IR as IR

-- import Debug.Trace

data Pruning = NoPrune       -- ^ Continue descending into child nodes
             | PruneChildren -- ^ Skip children, but cover current node
             | PruneHere     -- ^ Skip this and all child nodes
  deriving (Eq)

-- | Collect all nodes of the given tree, matching the given predicate.
collectAll :: (IR.Tree -> Bool) -> NodeAddress -> [NodeAddress]
collectAll p root = collectAllPrune p (const NoPrune) root

-- | Like 'collectAll' but allows you to prune the search space.
collectAllPrune :: (IR.Tree -> Bool) -> (IR.Tree -> Pruning) -> NodeAddress -> [NodeAddress]
collectAllPrune p pruning addr = map (append addr) $ resolveNodePaths $ collectAllPrunePaths p pruning (getNode addr)
  where
    resolveNodePaths :: [NodePath] -> [NodeAddress]
--    resolveNodePaths = map (error "dohh!!")
--    resolveNodePaths = map (flip mkNodeAddress (getNode addr))
    resolveNodePaths = mkNodeAddresses' (getNode addr)


collectAllPrunePaths :: (IR.Tree -> Bool) -> (IR.Tree -> Pruning) -> IR.Tree -> [NodePath]
collectAllPrunePaths p pruning root = flattenNodePaths $ go root
  where
    {-# NOINLINE resultMap #-}
    resultMap :: NodeMap (NodePaths Int)
    resultMap = fmap (\n -> {- traceShow ("result", (IR.getID n)) -} (go n)) $ NodeMap.mkNodeMap root

    lookup :: IR.Tree -> Maybe (NodePaths Int)
    lookup n = {- traceShow ("lookup", IR.getID n) $ -} NodeMap.lookup n resultMap

    go :: IR.Tree -> NodePaths Int
    go n = mergeNodePath (p n && pruning n /= PruneHere) $
                case pruning n of
                   NoPrune -> map (lookup >>| go) $ IR.getChildren n
                   _       -> []

    (>>|) :: (a -> Maybe b) -> (a -> b) -> a -> b
    f >>| g = \a -> case f a of Just x -> x; Nothing -> g a

collectAllPaths'Naive :: (IR.Tree -> Bool) -> IR.Tree -> [NodePath]
collectAllPaths'Naive pred root = map reverse $ go [] root
  where
    go :: NodePath -> IR.Tree -> [NodePath]
    go p n =
        (if pred n then (p:) else id) $ concat $ zipWith (\i c -> go (i : p) c) [0..] (IR.getChildren n)

-- | Collect all nodes of the given 'IR.NodeType' but prune the tree when
-- encountering one of the other 'IR.NodeType's.
collectAddr :: IR.NodeType -> [IR.NodeType -> Bool] -> NodeAddress -> [NodeAddress]
collectAddr t fs = collectAllPrune p pruning
  where
    p = (==t) . IR.getNodeType
    pruning n = if any (\f -> f $ IR.getNodeType n) fs then PruneHere else NoPrune

-- | Fold the given 'IR.Tree'. The accumulator function takes the subtree and
-- the address of this subtree in the base tree.
foldTree :: Monoid a => (NodeAddress -> a -> a) -> IR.Tree -> a
foldTree = flip foldTreePrune noPrune

-- | Fold the given 'IR.Tree'. The accumulator function takes the subtree and
-- the address of this subtree in the base tree.
foldAddress :: Monoid a => (NodeAddress -> a -> a) -> NodeAddress -> a
foldAddress = flip foldAddressPrune noPrune

-- | Disables pruning for 'foldTreePrune'.
noPrune :: NodeAddress -> Bool
noPrune = const False

-- | Like 'foldTree' but is able to not follow entire subtrees when the pruning
-- function returns 'False'.
foldTreePrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> IR.Tree                      -- ^ current processed node
                -> a                            -- ^ accumulated result
foldTreePrune collect prune ir = foldAddressPrune collect prune (mkNodeAddress [] ir)


-- | Like 'foldTree' but is able to not follow entire subtrees when the pruning
-- function returns 'False'.
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
    -- visitsub base acc = foldr (\i a -> visit (goDown i base) a) acc [0..(numChildren base - 1)]
    visitsub base acc = foldr visit acc (subtrees base)
    subtrees a = [goDown i a | i <- [0..(numChildren a) - 1]]

-- | Find declaration of given variable.
findDecl :: NodeAddress -> Maybe NodeAddress
findDecl start = findDecl' start
  where
    org = getNode start

    findDecl' :: NodeAddress -> Maybe NodeAddress
    findDecl' addr = case getNode addr of
        IR.Node IR.Lambda _ -> lambda addr
        _ -> parameter addr <|> declstmt addr <|> forstmt addr   <|>
             bindexpr addr  <|> compstmt addr <|> nextlevel addr

    parameter :: NodeAddress -> Maybe NodeAddress
    parameter addr = case getNode addr of
        IR.Node IR.Parameters _ -> Just start
        _ -> Nothing

    declstmt :: NodeAddress -> Maybe NodeAddress
    declstmt addr = case getNode addr of
        IR.Node IR.DeclarationStmt [_, v] | v == org -> Just $ goDown 1 addr
        _ -> Nothing

    forstmt :: NodeAddress -> Maybe NodeAddress
    forstmt addr = case getNode addr of
        IR.Node IR.ForStmt _ -> declstmt $ goDown 0 addr
        _ -> Nothing

    lambda :: NodeAddress -> Maybe NodeAddress
    lambda addr = case getNode addr of
        IR.Node IR.Lambda [_, IR.Node IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> elemIndex org ps
        _ -> Nothing

    bindexpr :: NodeAddress -> Maybe NodeAddress
    bindexpr addr = case getNode addr of
        IR.Node IR.BindExpr [_, IR.Node IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> elemIndex org ps
        _ -> Nothing

    compstmt :: NodeAddress -> Maybe NodeAddress
    compstmt addr = getNode <$> getParent addr >>= \case
        IR.Node IR.CompoundStmt _ | getIndex addr == 0 -> Nothing
        IR.Node IR.CompoundStmt _ -> findDecl' $ goLeft addr
        _ -> Nothing

    nextlevel :: NodeAddress -> Maybe NodeAddress
    nextlevel addr = getParent addr >>= findDecl'
