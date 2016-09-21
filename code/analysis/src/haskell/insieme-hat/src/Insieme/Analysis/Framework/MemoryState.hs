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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Insieme.Analysis.Framework.MemoryState (

    MemoryLocation(..),       -- re-exported for convinience
    MemoryStatePoint(..),
    memoryStateValue

) where

import Debug.Trace
import Data.Typeable
import Insieme.Inspire.NodeAddress as Node

import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.ProgramPoint
import Insieme.Analysis.Framework.Utils.OperatorHandler

import Insieme.Analysis.AccessPath
import Insieme.Analysis.Reference
import Insieme.Analysis.Callable
import qualified Insieme.Analysis.WriteSetSummary as WS

import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.Utils as IR
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Utils.BoundSet as BSet
import qualified Insieme.Utils.UnboundSet as USet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.DataPath hiding (isRoot)
import qualified Insieme.Analysis.Entities.AccessPath as AP
import qualified Insieme.Analysis.Entities.DataPath as DP
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.Memory

-- define the lattice of definitions
data Definition i = Initial
                | Creation
                | Declaration NodeAddress
                | MaterializingCall NodeAddress
                | Assignment NodeAddress (DataPath i)
                | PerfectAssignment NodeAddress
    deriving (Eq,Ord,Show)

type Definitions i = USet.UnboundSet (Definition i)

instance (FieldIndex i) => Solver.Lattice (Definitions i) where
    bot = USet.empty
    merge = USet.union

instance (FieldIndex i) => Solver.ExtLattice (Definitions i) where
    top = USet.Universe

--
-- * Memory State Analysis
--

data MemoryStateAnalysis a = MemoryStateAnalysis a
    deriving (Typeable)

memoryStateAnalysis :: (Typeable a, Typeable v) => DataFlowAnalysis a v -> Solver.AnalysisIdentifier
memoryStateAnalysis a = Solver.mkAnalysisIdentifier (MemoryStateAnalysis a) ('M' : show (analysisIdentifier a))

--
-- * Memory State Variable Generator
--

memoryStateValue :: (ComposedValue.ComposedValue v i a, Typeable d)
  => MemoryStatePoint      -- ^ program point and memory location interested in
  -> DataFlowAnalysis d v   -- ^ underlying data flow analysis this memory state analysis is cooperating with
  -> Solver.TypedVar v      -- ^ analysis variable representing the requested state

memoryStateValue ms@(MemoryStatePoint pp@(ProgramPoint addr p)
                     ml@(MemoryLocation loc)) analysis = var
    where
      -- extend the underlysing analysis's identifier for the memory state identifier
      varId = Solver.mkIdentifierFromMemoryStatePoint (memoryStateAnalysis analysis) ms
      var = Solver.mkVariable varId [con] Solver.bot
      con = Solver.createConstraint dep val var

      dep a = Solver.toVar reachingDefVar : (
        if USet.isUniverse defs || USet.member Creation defs then []
        else map Solver.toVar (definingValueVars a) ++ partialAssingDep a)
        where
          defs = reachingDefVal a

      val a
        | USet.isUniverse defs = Solver.top
        | USet.member Initial defs = Solver.merge init value
        | otherwise = value
        where
          init = initialValueHandler analysis loc
          value = if USet.member Creation defs then ComposedValue.top
                  else Solver.join $ partialAssingVal a : map (Solver.get a) (definingValueVars a)
          defs = reachingDefVal a

      reachingDefVar = reachingDefinitions ms
      reachingDefVal a = Solver.get a reachingDefVar

      definingValueVars a = USet.fromUnboundSet []
        (concatMap go . USet.toList) $ reachingDefVal a
        where
          go (Declaration addr) = [variableGenerator analysis $ goDown 1 addr]
          go (MaterializingCall addr) = [variableGenerator analysis addr]
          go (PerfectAssignment addr) = [getDeclaredValueVar $ goDown 3 addr]
          go _                        = []

          getDeclaredValueVar addr =
            if isMaterializingDeclaration $ getNodePair addr
            then memoryStateValue (MemoryStatePoint (ProgramPoint addr Post) (MemoryLocation addr)) analysis
            else variableGenerator analysis (goDown 1 addr)

      -- partial assignment support --
      partialAssignments a =
        USet.fromUnboundSet [] ( filter pred . USet.toList ) $ reachingDefVal a
        where
          pred (Assignment _ _) = True
          pred _                = False

      hasPartialAssign = not . null . partialAssignments
      partialAssingDep a =
        if ( not . hasPartialAssign ) a then []
        else concatMap go (partialAssignments a)
        where go (Assignment addr _) =
                [ Solver.toVar $ elemValueVar addr
                , Solver.toVar $ predStateVar addr ]

      partialAssingVal a =
        if ( not . hasPartialAssign ) a then Solver.bot
        else Solver.join $ map go $ partialAssignments a
        where
          go (Assignment addr dp) = ComposedValue.setElement dp elemValue predState
          elemValue = Solver.get a $ elemValueVar addr
          predState = Solver.get a (predStateVar addr)

      elemValueVar assign = variableGenerator analysis $ goDown 3 assign
      predStateVar assign = memoryStateValue (MemoryStatePoint (ProgramPoint assign Internal) ml) analysis


--
-- * Reaching Definition Analysis
--

data ReachingDefinitionAnalysis = ReachingDefinitionAnalysis
    deriving (Typeable)

reachingDefinitionAnalysis :: Solver.AnalysisIdentifier
reachingDefinitionAnalysis = Solver.mkAnalysisIdentifier ReachingDefinitionAnalysis "RD"


--
-- * Reaching Definition Variable Generator
--

reachingDefinitions :: (FieldIndex i) => MemoryStatePoint -> Solver.TypedVar (Definitions i)
reachingDefinitions (MemoryStatePoint pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) = case getNodeType addr of

        -- a declaration could be an assignment if it is materializing
        IR.Declaration | addr == loc && p == Post ->
            Solver.mkVariable varId [] (USet.singleton $ Declaration addr)

        -- a call could be an assignment if it is materializing
        IR.CallExpr | addr == loc && p == Post && isMaterializingCall (getNodePair addr) ->
            Solver.mkVariable varId [] (USet.singleton $ MaterializingCall addr)

        -- the call could also be the creation point if it is not materializing
        IR.CallExpr | addr == loc && p == Post ->
            Solver.mkVariable varId [] (USet.singleton Creation)

        -- the entry point is the creation of everything that reaches this point
        _ | p == Pre && IR.isEntryPoint addr ->
            Solver.mkVariable varId [] (USet.singleton Initial)

        -- skip everything that can statically be considered assignment free
        _ | p == Post && isAssignmentFree addr && not isParentOfLocation -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.forward (reachingDefinitions $ MemoryStatePoint (ProgramPoint addr Pre) ml) var

        -- to prune the set of variables, we check whether the invoced callable may update the traced reference
        IR.CallExpr | p == Internal && not isParentOfLocation -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.createEqualityConstraint dep val var

            dep a = Solver.toVar writeSetVar :
                if canSkipCallable a then [Solver.toVar skipPredecessorVar] else nonSkipPredecessorVars a

            val a = if canSkipCallable a then skipPredecessorVal a else nonSkipPredecessorVal a

            writeSetVar = writeSet addr
            writeSetVal a = Solver.get a writeSetVar

            canSkipCallable = not . USet.member loc . writeSetVal

            -- utils for skip case --

            skipPredecessorVar = reachingDefinitions $ MemoryStatePoint (ProgramPoint (goDown 1 addr) Post) ml
            skipPredecessorVal a = Solver.get a skipPredecessorVar

            -- utils for non-skip case --

            nonSkipPredecessorVars a = Solver.getDependencies a defaultVar
            nonSkipPredecessorVal a = Solver.getLimit a defaultVar

        -- for all the others, the magic is covered by the generic program point value constraint generator
        _ -> defaultVar

    where
        isParentOfLocation = loc `isChildOf` addr
        analysis pp = reachingDefinitions (MemoryStatePoint pp ml)
        idGen pp = Solver.mkIdentifierFromMemoryStatePoint reachingDefinitionAnalysis (MemoryStatePoint pp ml)
        varId = idGen pp
        extract = ComposedValue.toValue
        defaultVar = programPointValue pp idGen analysis [assignHandler]

        -- a handler for intercepting the interpretation of the ref_assign operator --
        assignHandler = OperatorHandler cov dep val
          where
            cov a = isBuiltin a $ getBuiltin addr "ref_assign"
            dep a = Solver.toVar targetRefVar : (
              if isEmptyRef a || (isActive a && isSingleRef a)
              then [] else pdep a)
            val a
              -- the target reference is not yet determined - wait
              | isEmptyRef a = Solver.bot
              -- it is referencing this memory location => do something
              | isActive a =
                  if isSingleRef a
                  -- it only references this memory location
                  then collectLocalDefs a
                  -- it references this and other memory locations
                  else USet.union (collectLocalDefs a) $ pval a
              -- it does not reference this memory location => no change to reachable definitions
              | otherwise = pval a

            -- here we have to skip the potentially materializing declaration!
            targetRefVar = referenceValue $ goDown 1 $ goDown 2 addr

            targetRefVal a = extract $ Solver.get a targetRefVar
            isActive a = USet.isUniverse refs || any pred (USet.toSet refs)
              where
                refs = targetRefVal a
                pred (Reference cp _) = cp == loc

            isEmptyRef a = USet.null $ targetRefVal a

            isSingleRef a = (not . USet.isUniverse) refs && all pred (USet.toSet refs)
              where
                refs = targetRefVal a
                pred (Reference l _) = l == loc

            collectLocalDefs a =
              if USet.isUniverse targets
              then USet.Universe
              else USet.fromList $ concatMap go (USet.toList targets)
              where
                targets = targetRefVal a
                go (Reference cp dp) | (cp == loc) && DP.isRoot dp = [PerfectAssignment addr]
                go (Reference cp dp) | cp == loc = [Assignment addr dp]
                go _                             = []

            (pdep,pval) = mkPredecessorConstraintCredentials pp analysis


-- A filter for the reaching definition analysis --

isAssignmentFree :: NodeAddress -> Bool
isAssignmentFree addr = case getNodeType addr of
        IR.Literal     -> True
        IR.Variable    -> True
        IR.CallExpr    ->
          isAssignmentFreeFunction (goDown 1 addr) &&
          all isAssignmentFree [goDown x addr | x <- [1..(numChildren addr-1)]]
        IR.LambdaExpr  -> True
        IR.BindExpr    -> True
        IR.Declaration -> isAssignmentFree $ goDown 1 addr
        _ -> False

isAssignmentFreeFunction :: NodeAddress -> Bool
isAssignmentFreeFunction addr = case getNodeType addr of
    IR.Literal -> not $ isBuiltinByName addr "ref_assign"
    IR.LambdaExpr -> any (isBuiltinByName addr) [
                                "ite",
                                "ref_scalar_to_ref_array",
                                "ref_array_elem",
                                "ptr_from_ref",
                                "ptr_to_ref",
                                "ptr_from_array",
                                "ptr_to_array",
                                "ptr_reinterpret"
                        ]
    _ -> False


--
-- * Write Set Analysis
--

data WriteSetAnalysis = WriteSetAnalysis
    deriving (Typeable)

writeSetAnalysis :: Solver.AnalysisIdentifier
writeSetAnalysis = Solver.mkAnalysisIdentifier WriteSetAnalysis "WriteSet"

instance Solver.Lattice (USet.UnboundSet Location) where
    bot = USet.empty
    merge = USet.union


-- an analysis computing the set of memory locations written by a expression
writeSet :: NodeAddress -> Solver.TypedVar (USet.UnboundSet Location)
writeSet addr = case getNodeType addr of

        IR.CallExpr -> var
            where
                var = Solver.mkVariable (idGen addr) [con] USet.empty
                con = Solver.createConstraint dep val var

                dep a = Solver.toVar targetVar : (Solver.toVar <$> writeSetSummaryVars a) ++ (Solver.toVar <$> refVars a)
                val a = if unknown then USet.Universe else res
                    where
                        wss = writeSetSummaryVal a
                        aps = WS.toAccessPaths wss

                        unknown = WS.isUnknown wss || any infinite (refVars a) || any tooLong aps
                            where
                                infinite r = USet.isUniverse $ ComposedValue.toValue $ Solver.get a r
                                tooLong (AP.AccessPath _ d) = length d > 1

                        res = foldr go USet.empty (WS.parameters wss)
                            where
                                go x s = USet.union s $ USet.map toLoc $ ComposedValue.toValue $ Solver.get a $ refVar x
                                toLoc :: Reference SimpleFieldIndex -> Location
                                toLoc (Reference l _ ) = l

                refVar :: Int -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
                refVar x = referenceValue $ goDown 1 $ goDown (x+2) addr

                targetVar = callableValue $ goDown 1 addr
                targetVal a = ComposedValue.toValue $ Solver.get a targetVar

                writeSetSummaryVars :: Solver.Assignment -> [Solver.TypedVar (WS.WriteSet SimpleFieldIndex)]
                writeSetSummaryVars a = if USet.isUniverse trgs then [] else list
                    where
                        trgs = targetVal a
                        list = go <$> USet.toList trgs
                        go = WS.writeSetSummary . toAddress

                writeSetSummaryVal a = Solver.join $ Solver.get a <$> writeSetSummaryVars a
                refVars a = if WS.isUnknown wss then [] else res
                     where
                        wss = writeSetSummaryVal a
                        res = refVar <$> WS.parameters wss
        _ -> empty

    where
        empty = Solver.mkVariable (idGen addr) [] USet.empty
        idGen = Solver.mkIdentifierFromExpression writeSetAnalysis

-- killed definitions

killedDefinitions :: MemoryStatePoint -> Solver.TypedVar (Definitions i)
killedDefinitions (MemoryStatePoint pp ml) = undefined
