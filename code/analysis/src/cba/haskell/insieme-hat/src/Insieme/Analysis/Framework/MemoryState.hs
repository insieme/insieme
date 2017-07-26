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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Insieme.Analysis.Framework.MemoryState (

    MemoryLocation(..),       -- re-exported for convinience
    MemoryStatePoint(..),
    memoryStateValue

) where

import Control.DeepSeq
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.Memory
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.ProgramPoint
import Insieme.Analysis.Reference
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import qualified Insieme.Analysis.Entities.AccessPath as AP
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Analysis.WriteSetSummary as WS
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow




-- define the lattice of definitions

data Definition = Initial                                    -- it is the definiton obtained at program startup
              | Creation                                     -- it is the creation point definition (e.g. ref_alloc)
              | Declaration NodeAddress                      -- the definition is conducted by an assignment triggered through a materializing declaration
              | MaterializingCall NodeAddress                -- the definition is conducted by an assignment triggered through a materializing call
              | Assignment NodeAddress                       -- an assignment conducting an update to a memory location
              | Initialization NodeAddress                   -- an initialization expression conducting an update to a memory location
    deriving (Eq,Ord,Show,Generic,NFData)

type Definitions = BSet.UnboundSet Definition

instance Solver.Lattice Definitions where
    bot = BSet.empty
    merge = BSet.union


instance Solver.ExtLattice Definitions where
    top = BSet.Universe

--
-- * Memory State Analysis
--

data MemoryStateAnalysis a = MemoryStateAnalysis a
    deriving (Typeable)

memoryStateAnalysis :: (Typeable a, Typeable v, Typeable i) => DataFlowAnalysis a v i -> Solver.AnalysisIdentifier
memoryStateAnalysis a = Solver.mkAnalysisIdentifier (MemoryStateAnalysis a) ('M' : (show $ analysisIdentifier a) )


--
-- * Memory State Variable Generator
--

memoryStateValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => MemoryStatePoint                            -- ^ the program point and memory location interested in
         -> DataFlowAnalysis d v i                      -- ^ the underlying data flow analysis this memory state analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested state

memoryStateValue ms@(MemoryStatePoint (ProgramPoint _ _) ml@(MemoryLocation loc)) analysis = var

    where

        -- extend the underlysing analysis's identifier for the memory state identifier
        varId = Solver.mkIdentifierFromMemoryStatePoint (memoryStateAnalysis analysis) ms

        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var

        dep a = (Solver.toVar reachingDefVar) :
                   (
                     if BSet.isUniverse defs || BSet.member Creation defs then []
                     else (map Solver.toVar $ definingValueVars a)
                   )
            where
                defs = reachingDefVal a

        val a = case () of
                _ | BSet.isUniverse defs      -> Solver.top
                  | BSet.member Initial defs  -> Solver.merge init value
                  | otherwise                 -> value
            where
                init = initialValueHandler analysis loc

                value = if BSet.member Creation $ defs then ComposedValue.top
                        else Solver.join $ map (Solver.get a) (definingValueVars a)

                defs = reachingDefVal a


        reachingDefVar = reachingDefinitions ms
        reachingDefVal a = Solver.get a reachingDefVar

        definingValueVars a =
                BSet.applyOrDefault [] (concat . (map go) . BSet.toList) $ reachingDefVal a
            where
                go (Declaration       addr)         = [variableGenerator analysis $ goDown 1 addr]
                go (MaterializingCall addr)         = [variableGenerator analysis $ addr]
                go (Assignment addr)                = [definedValue addr ml analysis]
                go (Initialization addr)            = [definedValue addr ml analysis]
                go _                                = []




--
-- * Defined Value Analysis
--

data DefinedValueAnalysis a = DefinedValueAnalysis a
    deriving (Typeable)

definedValueAnalysis :: (Typeable a, Typeable v, Typeable i) => DataFlowAnalysis a v i -> Solver.AnalysisIdentifier
definedValueAnalysis a = Solver.mkAnalysisIdentifier (DefinedValueAnalysis a) ( "DV-" ++ (show $ analysisIdentifier a) )


--
-- * Defined Value Variable Generator
--

definedValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => NodeAddress                                 -- ^ the assignment interrested in
         -> MemoryLocation                              -- ^ the memory location interrested in
         -> DataFlowAnalysis d v i                      -- ^ the underlying data flow analysis this defined value analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested value

definedValue addr ml@(MemoryLocation loc) analysis = case getNodeType addr of

        -- handle values defined by assignments
        IR.CallExpr -> var

        -- handle struct values defined by init expressions
        IR.InitExpr | isTagType elemType -> var
           where
             var = Solver.mkVariable varId [con] Solver.bot
             con = Solver.createEqualityConstraint dep val var

             dep a = Solver.toVar targetRefVar : (Solver.toVar <$> valueVars)
             val a = ComposedValue.composeElements $ zip fields (valueVal a)

             fields = fromJust $ map field <$> getFieldNames elemType

             valueVal a = Solver.get a <$> valueVars
             valueVars = valueVar <$> getChildren (goDown 2 addr)

        -- handle struct values defined by init expressions
        IR.InitExpr | isArray elemType -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.createEqualityConstraint dep val var

            dep a = Solver.toVar targetRefVar : (Solver.toVar <$> valueVars)
            val a = ComposedValue.composeElements $ elems a

            -- attach (unknownIndex, default value) if not all array elements are initialized
            elems a | numIndices == getArraySize arrayType = zip indices (valueVal a)
            elems a = (unknownIndex, initValueHandler analysis) : zip indices (valueVal a)

            arrayType = goDown 0 $ goDown 2 $ goDown 0 addr

            indices = (index . Ar.mkConst . fromIntegral) <$> [0..numIndices-1]
            numIndices = numChildren $ goDown 2 addr

            valueVal a = Solver.get a <$> valueVars
            valueVars = valueVar <$> getChildren (goDown 2 addr)

        -- handle scalar values defined by init expressions
        IR.InitExpr -> var

    where

        -- the ID of the produced variable
        varId = Solver.mkIdentifierFromMemoryStatePoint (definedValueAnalysis analysis) ms
            where
                ms = MemoryStatePoint (ProgramPoint addr Post) ml


        -- the variable for assignment and scalar init

        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createEqualityConstraint dep val var

        dep a = Solver.toVar targetRefVar : valueDeps a

        val a = case () of _ | not $ isActive a  -> old
                             | isFullAssign a    -> elemValueVal a
                             | isPerfectAssign a -> new
                             | otherwise         -> Solver.merge old new
            where
                old = Solver.get a oldStateVar
                new = updatedValue a



        -- get access to the referenced locations
        targetRefVar = referenceValue $ case getNodeType addr of
            IR.CallExpr -> goDown 1 $ goDown 2 addr         -- here we have to skip the potentially materializing declaration!
            IR.InitExpr -> goDown 1 addr                    -- the reference to be initialized
        targetRefVal a = ComposedValue.toValue $ Solver.get a targetRefVar


        -- some checks regarding the scope of the update

        isActive a = (BSet.isUniverse trgs) || (any pred $ BSet.toSet $ trgs)
            where
                trgs = targetRefVal a
                pred (Reference cp _) = cp == loc
                pred _ = False

        isPerfectAssign a = (not $ BSet.isUniverse trgs) && (BSet.size trgs == 1)
            where
                trgs = targetRefVal a

        isFullAssign a = isPerfectAssign a && (all pred $ BSet.toSet $ targetRefVal a)
            where
                pred (Reference _ DP.Root) = True
                pred _ = False


        -- value analysis variable generator

        valueVar = variableGenerator analysis


        -- value composing utilities

        valueDeps a = case () of _ | not $ isActive a -> [Solver.toVar oldStateVar]
                                   | isFullAssign a   -> [Solver.toVar elemValueVar]
                                   | otherwise        -> [Solver.toVar oldStateVar, Solver.toVar elemValueVar]

        elemValueVar = case getNodeType addr of

                IR.InitExpr  -> valueVar $ goDown 0 $ goDown 2 addr

                IR.CallExpr  -> if isMaterializingDeclaration $ getNode valueDecl
                                then memoryStateValue (MemoryStatePoint (ProgramPoint valueDecl Post) (MemoryLocation valueDecl)) analysis
                                else valueVar (goDown 1 valueDecl)

            where
                valueDecl = goDown 3 addr


        elemValueVal a = Solver.get a elemValueVar

        updatedValue a =
                if BSet.isUniverse trgs then Solver.top else Solver.join values
            where
                oldVal = oldStateVal a
                elemVal = elemValueVal a

                trgs = targetRefVal a

                values = concat $ update <$> BSet.toList trgs

                update (Reference cp dp) | cp == loc = [
                        ComposedValue.setElement dp elemVal oldVal
                    ]
                update _ = []


        -- access to the old state

        oldStateVar = memoryStateValue (MemoryStatePoint (ProgramPoint (goDown 1 addr) Post) ml) analysis
        oldStateVal a = Solver.get a oldStateVar


        -- type checks

        elemType = fromJust $ getReferencedType =<< getType addr




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

reachingDefinitions :: MemoryStatePoint -> Solver.TypedVar Definitions
reachingDefinitions (MemoryStatePoint pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) = case getNodeType addr of

        -- a declaration could be an assignment if it is materializing
        IR.Declaration | addr == loc && p == Pre ->
            Solver.mkVariable varId [] (BSet.singleton $ Declaration addr)

        -- a call could be an assignment if it is materializing
        IR.CallExpr | addr == loc && p == Post && isMaterializingCall (getNode addr) ->
            Solver.mkVariable varId [] (BSet.singleton $ MaterializingCall addr)

        -- the call could also be the creation point if it is not materializing
        IR.CallExpr | addr == loc && p == Post ->
            Solver.mkVariable varId [] (BSet.singleton Creation)

        -- the entry point is the creation of everything that reaches this point
        _ | p == Pre && isEntryPoint addr ->
            Solver.mkVariable varId [] (BSet.singleton $ Initial)

        -- skip everything that can statically be considered assignment free
        _ | p == Post && isAssignmentFree addr && (not isParentOfLocation) ->
            reachingDefinitions $ MemoryStatePoint (ProgramPoint addr Pre) ml

        -- an assignment might be a definition point
        IR.CallExpr | p == Post && isAssignCandidate -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = Solver.toVar funVar : case () of
                        _ | noFuns a    -> []                -- wait until there are some target functions (optimization)
                          | mayAssign a -> refs : case () of
                                _ | noRefs a  -> []             -- wait until there are some references (optimization)
                                  | hitsLoc a -> []             -- it is a hit, we don't need the predecessor
                                  | otherwise -> predVars       -- it is no full hit, we might need the predecessor
                          | otherwise   -> predVars
                    where
                        predVars = predecessorVars a
                        refs = Solver.toVar refVar


                val a = case () of
                    _ | mayAssign a -> case () of
                            _ | noRefs a  -> Solver.bot
                              | hitsLoc a -> BSet.singleton $ Assignment addr
                              | otherwise -> predVals
                      | otherwise   -> predVals
                    where
                        predVals = predecessorVal a


                funVar = callableValue $ goDown 1 addr
                funVal a = ComposedValue.toValue $ Solver.get a funVar

                noFuns a = BSet.null $ funVal a

                mayAssign a = BSet.isUniverse funs || (any assign $ BSet.toSet funs)
                    where
                        funs = funVal a
                        assign c = isBuiltin (toAddress c) "ref_assign"



        -- to prune the set of variables, we check whether the invoced callable may update the traced reference
        IR.CallExpr | p == Internal && (not isParentOfLocation)-> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = (Solver.toVar writeSetVar) :
                    if canSkipCallable a then [Solver.toVar skipPredecessorVar] else nonSkipPredecessorVars a

                val a = if canSkipCallable a then skipPredecessorVal a else nonSkipPredecessorVal a

                writeSetVar = writeSet addr
                writeSetVal a = Solver.get a writeSetVar

                canSkipCallable = not . BSet.member loc . writeSetVal

                -- utils for skip case --

                skipPredecessorVar = reachingDefinitions $ MemoryStatePoint (ProgramPoint (goDown 1 addr) Post) ml
                skipPredecessorVal a = Solver.get a skipPredecessorVar

                -- utils for non-skip case --

                nonSkipPredecessorVars a = Solver.getDependencies a defaultVar
                nonSkipPredecessorVal a = Solver.getLimit a defaultVar

        -- init expressions may alter memory states as well
        IR.InitExpr | p == Post -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = Solver.toVar refVar : case () of
                        _ | noRefs a  -> []
                          | hitsLoc a -> []
                          | otherwise -> predecessorVars a

                val a = case () of
                     _ | noRefs a  -> Solver.bot
                       | hitsLoc a -> BSet.singleton $ Initialization addr
                       | otherwise -> predecessorVal a

        -- for all the others, the magic is covered by the generic program point value constraint generator
        _ -> defaultVar

    where

        isParentOfLocation = loc `isChildOf` addr

        analysis pp = reachingDefinitions (MemoryStatePoint pp ml)

        idGen pp = Solver.mkIdentifierFromMemoryStatePoint reachingDefinitionAnalysis (MemoryStatePoint pp ml)
        varId = idGen pp

        defaultVar = programPointValue pp idGen analysis []

        isAssignCandidate = res
            where
                res = numChildren addr == 4 && unitRes && refParam
                unitRes  = isUnit $ getNode addr
                refParam = isReference $ getNode $ goDown 1 $ goDown 2 addr

        -- target location utils --

        refVar = referenceValue $ case getNodeType addr of
            IR.CallExpr -> goDown 1 $ goDown 2 addr             -- first argument
            IR.InitExpr -> goDown 1 addr                        -- target memory location

        refVal a = ComposedValue.toValue $ Solver.get a refVar

        noRefs a = BSet.null $ refVal a

        hitsLoc a = BSet.isUniverse refs || (any hit $ BSet.toSet refs)
            where
                refs :: ReferenceSet SimpleFieldIndex
                refs = refVal a
                hit (Reference cp _) = cp == loc
                hit _ = False

        predecessorVars a = Solver.getDependencies a defaultVar
        predecessorVal  a = Solver.getLimit a defaultVar


-- A filter for the reaching definition analysis --

isAssignmentFree :: NodeAddress -> Bool
isAssignmentFree addr = case getNodeType addr of

        IR.Literal     -> True

        IR.Variable    -> True

        IR.CallExpr    -> isAssignmentFreeFunction (goDown 1 addr) &&
                            all isAssignmentFree [ goDown x addr | x <- [1 .. (numChildren addr - 1)] ]

        IR.LambdaExpr  -> True

        IR.BindExpr    -> True

        IR.Declaration -> isAssignmentFree $ goDown 1 addr

        _ -> False



isAssignmentFreeFunction :: NodeAddress -> Bool
isAssignmentFreeFunction addr = case getNodeType addr of

    IR.Literal -> not $ isBuiltin addr "ref_assign"

    IR.LambdaExpr -> any (isBuiltin addr) [
                                "bool_and",
                                "bool_not",
                                "bool_or",
                                "bool_to_int",
                                "enum_from_int",
                                "enum_to_int",
                                "id",
                                "ite",
                                "num_cast",
                                "ptr_cast",
                                "ptr_deref",
                                "ptr_diff",
                                "ptr_eq",
                                "ptr_from_array",
                                "ptr_from_ref",
                                "ptr_ge",
                                "ptr_gt",
                                "ptr_le",
                                "ptr_lt",
                                "ptr_ne",
                                "ptr_null",
                                "ptr_of_function",
                                "ptr_parent_cast",
                                "ptr_reinterpret",
                                "ptr_subscript",
                                "ptr_to_array",
                                "ptr_to_ref",
                                "ref_array_element",
                                "ref_kind_cast",
                                "ref_member_access",
                                "ref_new",
                                "ref_new_init",
                                "ref_of_function",
                                "ref_parent_cast",
                                "ref_scalar_to_ref_array",
                                "ref_temp",
                                "ref_temp_init",
                                "unit_consume"
                        ]

    _ -> False


--
-- * Write Set Analysis
--

data WriteSetAnalysis = WriteSetAnalysis
    deriving (Typeable)

writeSetAnalysis :: Solver.AnalysisIdentifier
writeSetAnalysis = Solver.mkAnalysisIdentifier WriteSetAnalysis "WriteSet"



instance Solver.Lattice (BSet.UnboundSet Location) where
    bot = BSet.empty
    merge = BSet.union


-- an analysis computing the set of memory locations written by a expression
writeSet :: NodeAddress -> Solver.TypedVar (BSet.UnboundSet Location)
writeSet addr = case getNodeType addr of

        IR.CallExpr -> var
            where
                var = Solver.mkVariable (idGen addr) [con] BSet.empty
                con = Solver.createConstraint dep val var

                dep a = (Solver.toVar targetVar) : (Solver.toVar <$> writeSetSummaryVars a) ++ (Solver.toVar <$> refVars a)
                val a = if unknown then BSet.Universe else res
                    where
                        wss = writeSetSummaryVal a
                        aps = WS.toAccessPaths wss

                        unknown = (WS.isUnknown wss) || (any infinite $ refVars a) || (any tooLong aps)
                            where
                                infinite r = BSet.isUniverse $ ComposedValue.toValue $ Solver.get a r
                                tooLong (AP.AccessPath _ d) = length d > 1

                        res = foldr go BSet.empty (WS.parameters wss)
                            where
                                go x s = BSet.union s $ BSet.fromList $ concat $ map toLoc $ BSet.toList $ ComposedValue.toValue $ Solver.get a $ refVar x
                                toLoc :: Reference SimpleFieldIndex -> [Location]
                                toLoc (Reference l _ ) = [l]
                                toLoc _ = []

                refVar :: Int -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
                refVar x = referenceValue $ goDown 1 $ goDown (x+2) addr

                targetVar = callableValue $ goDown 1 addr
                targetVal a = ComposedValue.toValue $ Solver.get a targetVar

                writeSetSummaryVars :: Solver.Assignment -> [Solver.TypedVar (WS.WriteSet SimpleFieldIndex)]
                writeSetSummaryVars a = if BSet.isUniverse trgs then [] else list
                    where
                        trgs = targetVal a
                        list = go <$> BSet.toList trgs
                        go = WS.writeSetSummary . toAddress

                writeSetSummaryVal a = Solver.join $ (Solver.get a) <$> writeSetSummaryVars a

                refVars a = if WS.isUnknown wss then [] else res
                     where
                        wss = writeSetSummaryVal a
                        res = refVar <$> WS.parameters wss

        _ -> empty

    where

        empty = Solver.mkVariable (idGen addr) [] (BSet.empty)

        idGen a = Solver.mkIdentifierFromExpression writeSetAnalysis a



-- killed definitions

killedDefinitions :: MemoryStatePoint -> Solver.TypedVar Definitions
killedDefinitions (MemoryStatePoint pp ml) = undefined
