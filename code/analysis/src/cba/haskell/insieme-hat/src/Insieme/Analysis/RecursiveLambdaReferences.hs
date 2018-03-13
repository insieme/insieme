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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Insieme.Analysis.RecursiveLambdaReferences (

    LambdaReferenceSet(..),
    recursiveCalls

) where

import Data.Typeable
import qualified Data.Set as Set

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q

import Insieme.Analysis.Solver

import qualified Data.Map as Map
import Data.Maybe

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import qualified Insieme.Inspire.Visit.NodeMap as NodeMap


--
-- * the lattice of this analysis
--

newtype LambdaReferenceSet = LambdaReferenceSet { unLRS :: Set.Set NodeAddress }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Lattice LambdaReferenceSet where
    bot = LambdaReferenceSet Set.empty
    (LambdaReferenceSet x) `merge` (LambdaReferenceSet y) = LambdaReferenceSet $ Set.union x y


--
-- * RecursiveLambdaReferences Analysis
--

data RecursiveLambdaReferenceAnalysis = RecursiveLambdaReferenceAnalysis
    deriving (Typeable)


--
-- * the constraint generator
--

recursiveCalls :: NodeAddress -> TypedVar LambdaReferenceSet
recursiveCalls addr = case Q.getNodeType addr of

        I.Lambda | I.depth addr >= 3 -> var

        _ -> error "Can only compute recursive calls for lambdas with sufficient context!"

    where

      var = mkVariable varId [con] bot
      con = createConstraint dep val var

      varId = mkIdentifierFromExpression analysis addr
      analysis = mkAnalysisIdentifier RecursiveLambdaReferenceAnalysis "RecLambdaRefs"

      dep _ = [toVar indexVar]

      val a = LambdaReferenceSet $ Set.fromList refs
        where
          refs = getRecursiveReferences (get a indexVar) addr

      indexVar = globalRecursiveLambdaReferenceIndex $ I.node $ I.getRootAddress addr




--------------------------------------------------------------------------------
--           Global Recursive Lambda Reference Index Analysis                 --
--------------------------------------------------------------------------------

--
-- * the lattice of this analysis
--

newtype RecursiveLambdaReferenceIndex = RecursiveLambdaReferenceIndex (Map.Map I.Tree (Map.Map I.Tree [NodeAddress]))
  deriving (Eq, Ord, Show, Generic, NFData)

instance Lattice RecursiveLambdaReferenceIndex where
    bot = RecursiveLambdaReferenceIndex Map.empty
    (RecursiveLambdaReferenceIndex a) `merge` (RecursiveLambdaReferenceIndex b) = case () of
      _ | Map.null a -> RecursiveLambdaReferenceIndex b
        | Map.null b -> RecursiveLambdaReferenceIndex a
        | otherwise  -> undefined


getRecursiveReferences :: RecursiveLambdaReferenceIndex -> NodeAddress -> [NodeAddress]
getRecursiveReferences _ lambda | I.depth lambda <= 2 = []
getRecursiveReferences (RecursiveLambdaReferenceIndex index) lambda = I.append definition <$> list
  where
    Just binding = I.getParent lambda
    Just definition = I.getParent binding
    def = I.node definition
    tag = I.child 0 $ I.node binding

    inner = fromMaybe Map.empty $ Map.lookup def index
    list = fromMaybe [] $ Map.lookup tag inner


--
-- * RecursiveLambdaReferenceIndex Analysis
--

data RecursiveLambdaReferenceIndexAnalysis = RecursiveLambdaReferenceIndexAnalysis
    deriving (Typeable)


--
-- * the constraint generator
--

globalRecursiveLambdaReferenceIndex :: I.Tree -> TypedVar RecursiveLambdaReferenceIndex
globalRecursiveLambdaReferenceIndex root = var
  where
    var = mkVariable id [con] bot
    con = constant index var

    aid = mkAnalysisIdentifier RecursiveLambdaReferenceIndexAnalysis "RecLambdaRefIndex"
    id = mkIdentifierFromExpression aid $ I.mkNodeAddress [] root

    index = indexRecursiveLambdaReferences root




-- a utility to store sets of addresses by sharing indices
-- in the form of a tree
data NodeAddressSet = Level Bool [(Int,NodeAddressSet)]
  deriving Show

empty :: NodeAddressSet
empty = Level False []

isEmpty :: NodeAddressSet -> Bool
isEmpty (Level False []) = True
isEmpty (Level False ls) = all isEmpty (snd <$> ls)
isEmpty _ = False


toAddresses :: NodeAddress -> NodeAddressSet -> [NodeAddress]
toAddresses r s = go r s []
  where
    go a (Level f nested) rs = foldr step nrs nested
      where
        nrs = if f then a:rs else rs
        step (i,l) = go (I.child i a) l


filterSet :: (I.Tree -> Bool) -> I.Tree -> NodeAddressSet -> NodeAddressSet
filterSet p t (Level f cs) = if isEmpty res then empty else res
  where
    res = Level cur subs
    cur = f && p t
    subs = go <$> cs
    go (i,s) = (i,filterSet p (I.child i t) s)



-- the actual lambda reference indexer
indexRecursiveLambdaReferences :: I.Tree -> RecursiveLambdaReferenceIndex
indexRecursiveLambdaReferences root = RecursiveLambdaReferenceIndex index
  where
    -- the resulting index
    index :: Map.Map I.Tree (Map.Map I.Tree [NodeAddress])
    index = foldr go Map.empty $ Map.keys refIndex
      where
        -- only interested in lambda definitions
        go :: I.Tree -> Map.Map I.Tree (Map.Map I.Tree [NodeAddress]) -> Map.Map I.Tree (Map.Map I.Tree [NodeAddress])
        go t res | I.getNodeType t /= I.LambdaDefinition = res
        go t cur = res
          where
            res :: Map.Map I.Tree (Map.Map I.Tree [NodeAddress])
            res = Map.insert t indexed cur

            -- get full list of references
            references :: [NodeAddress]
            references = foldr go [] $ I.children $ I.mkNodeAddress [] t
              where
                go a ls = newAddresses ++ ls
                  where
                    newAddresses = toAddresses a refSet
                    Just refSet = Map.lookup (I.node a) refIndex

            -- an indexed version of the references (ref->addresses)
            indexed :: Map.Map I.Tree [NodeAddress]
            indexed = foldr go Map.empty references
              where
                go a = Map.insertWith (++) (I.node a) [a]

    -- an index of all tag type reference addresses
    refIndex :: Map.Map I.Tree NodeAddressSet
    refIndex = Map.fromList $ foldr (:) [] resultMap
      where
        resultMap = go <$> NodeMap.mkNodeMap root

        go tree = (tree,set)
          where
            -- see whether this node is a lambada reference
            isRef = I.getNodeType tree == I.LambdaReference

            -- lookup value of child nodes (memorized)
            resolve t = let Just x = NodeMap.lookup t resultMap in snd x

            -- skip tags in lamba expressions and bindings
            subs = case I.getNodeType tree of
              I.LambdaExpr       -> [ empty, empty, resolve $ I.child 2 tree ]
              I.LambdaBinding    -> [ empty , resolve $ I.child 1 tree ]
              _                  -> resolve <$> I.children tree

            -- create child list and filter out empty sub-lists
            lst = filter go $ zip [0..] subs
              where
                go (_,b) = not $ isEmpty b

            -- finally, filter out bound references
            set = case I.getNodeType tree of
                I.LambdaDefinition -> filterSet p tree unfiltered
                _ -> unfiltered
              where
                unfiltered = Level isRef lst
                p n = I.getNodeType n == I.LambdaReference && notElem n definedTags
                definedTags = head <$> (I.children <$> I.children tree)
