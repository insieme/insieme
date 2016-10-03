{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
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
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

{-# LANGUAGE LambdaCase #-}

module Insieme.Inspire.Visit (
    Pruning(..),
    collectAll,
    collectAllPrune,
    collectAddr,
    foldTree,
    foldAddress,
    foldTreePrune,
    foldAddressPrune,
    findDecl,
) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Insieme.Inspire.NodeAddress
import qualified Data.IntMap.Strict as IntMap
import qualified Insieme.Inspire as IR

data Pruning = NoPrune       -- continue descending into child nodes
             | PruneChildren -- skip children, but cover current node
             | PruneHere     -- skip this and all child nodes
  deriving (Eq)

collectAllPrune :: (IR.Tree -> Bool) -> (IR.Tree -> Pruning) -> NodeAddress -> [NodeAddress]
collectAllPrune pred filter root = evalState (go root) IntMap.empty
  where
    go :: NodeAddress -> State (IntMap.IntMap [NodeAddress]) [NodeAddress]
    go addr = do
        cache <- get
        let hit = IntMap.lookup key cache
        if isJust hit
            then return $ fromJust hit
            else do
                r <- res
                modify $ IntMap.insert key r
                res

      where
        node = getNode addr
        key = IR.getID node
        res = addAddr <$> concat <$> grow <$>
            if filter node == NoPrune then mapM go (crop <$> getChildren addr) else return []

        grow lists = (zipWith go) [ goDown i addr | i <- [0..] ] lists
            where
                go head tails = append head <$> tails

        addAddr xs = if (filter node /= PruneHere) && (pred $ getNode addr) then (addr:xs) else xs

collectAll :: (IR.Tree -> Bool) -> NodeAddress -> [NodeAddress]
collectAll pred root = collectAllPrune pred (\_ -> NoPrune ) root

-- | Collect all nodes of the given 'IR.NodeType' but prune the tree when
-- encountering one of the other 'IR.NodeType's.
collectAddr :: IR.NodeType -> [IR.NodeType -> Bool] -> NodeAddress -> [NodeAddress]
collectAddr t fs = collectAllPrune pred filter
  where
    pred = (==t) . IR.getNodeType
    filter t = if any (\f -> f $ IR.getNodeType t) fs then PruneHere else NoPrune

-- | Fold the given 'Tree'. The accumulator function takes the subtree and the
-- address of this subtree in the base tree.
foldTree :: Monoid a => (NodeAddress -> a -> a) -> IR.Inspire -> a
foldTree = flip foldTreePrune noPrune

-- | Fold the given 'Tree'. The accumulator function takes the subtree and the
-- address of this subtree in the base tree.
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
                -> IR.Inspire                   -- ^ current inspire representation
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
    subtrees addr = [goDown i addr | i <- [0..(numChildren addr) - 1]]

-- | Find declaration of given variable.
findDecl :: NodeAddress -> Maybe NodeAddress
findDecl start = findDecl start
  where
    org = getNode start

    findDecl :: NodeAddress -> Maybe NodeAddress
    findDecl addr = case getNode addr of
        IR.NT IR.Lambda _ -> lambda addr
        _ -> parameter addr <|> declstmt addr <|> forstmt addr   <|>
             bindexpr addr  <|> compstmt addr <|> nextlevel addr

    parameter :: NodeAddress -> Maybe NodeAddress
    parameter addr = case getNode addr of
        IR.NT IR.Parameters _ -> Just start
        _ -> Nothing

    declstmt :: NodeAddress -> Maybe NodeAddress
    declstmt addr = case getNode addr of
        IR.NT IR.DeclarationStmt [_, v] | v == org -> Just $ goDown 1 addr
        _ -> Nothing

    forstmt :: NodeAddress -> Maybe NodeAddress
    forstmt addr = case getNode addr of
        IR.NT IR.ForStmt _ -> declstmt $ goDown 0 addr
        _ -> Nothing

    lambda :: NodeAddress -> Maybe NodeAddress
    lambda addr = case getNode addr of
        IR.NT IR.Lambda [_, IR.NT IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> elemIndex org ps
        _ -> Nothing

    bindexpr :: NodeAddress -> Maybe NodeAddress
    bindexpr addr = case getNode addr of
        IR.NT IR.BindExpr [_, IR.NT IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> elemIndex org ps
        _ -> Nothing

    compstmt :: NodeAddress -> Maybe NodeAddress
    compstmt addr = getNode <$> getParent addr >>= \case
        IR.NT IR.CompoundStmt _ | getIndex addr == 0 -> Nothing
        IR.NT IR.CompoundStmt _ -> findDecl $ goLeft addr
        _ -> Nothing

    nextlevel :: NodeAddress -> Maybe NodeAddress
    nextlevel addr = getParent addr >>= findDecl
