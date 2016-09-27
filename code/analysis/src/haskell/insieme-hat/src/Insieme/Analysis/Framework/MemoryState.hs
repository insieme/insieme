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
import Insieme.Inspire.NodeAddress

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

data Definition = Initial                                    -- it is the definiton obtained at program startup
              | Creation                                     -- it is the creation point definition (e.g. ref_alloc)
              | Declaration NodeAddress                      -- the definition is conducted by an assignment triggered through a materializing declaration
              | MaterializingCall NodeAddress                -- the definition is conducted by an assignment triggered through a materializing call
              | Assignment NodeAddress                       -- an assignment conducting an update to a memory location
    deriving (Eq,Ord,Show)

type Definitions = USet.UnboundSet Definition

instance Solver.Lattice Definitions where
    bot = USet.empty
    merge = USet.union


instance Solver.ExtLattice Definitions where
    top = USet.Universe

--
-- * Memory State Analysis
--

data MemoryStateAnalysis a = MemoryStateAnalysis a
    deriving (Typeable)

memoryStateAnalysis :: (Typeable a, Typeable v) => DataFlowAnalysis a v -> Solver.AnalysisIdentifier
memoryStateAnalysis a = Solver.mkAnalysisIdentifier (MemoryStateAnalysis a) ('M' : (show $ analysisIdentifier a) )


--
-- * Memory State Variable Generator
--

memoryStateValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => MemoryStatePoint                            -- ^ the program point and memory location interested in
         -> DataFlowAnalysis d v                        -- ^ the underlying data flow analysis this memory state analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested state

memoryStateValue ms@(MemoryStatePoint pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) analysis = var

    where

        -- extend the underlysing analysis's identifier for the memory state identifier
        varId = Solver.mkIdentifierFromMemoryStatePoint (memoryStateAnalysis analysis) ms

        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var

        dep a = (Solver.toVar reachingDefVar) :
                   (
                     if USet.isUniverse defs || USet.member Creation defs then [] 
                     else (map Solver.toVar $ definingValueVars a)
                   )
            where
                defs = reachingDefVal a

        val a = case () of 
                _ | USet.isUniverse defs      -> Solver.top
                  | USet.member Initial defs  -> Solver.merge init value
                  | otherwise                 -> value
            where
                init = initialValueHandler analysis loc
            
                value = if USet.member Creation $ defs then ComposedValue.top
                        else Solver.join $ map (Solver.get a) (definingValueVars a)
                        
                defs = reachingDefVal a


        reachingDefVar = reachingDefinitions ms
        reachingDefVal a = Solver.get a reachingDefVar

        definingValueVars a =
                USet.fromUnboundSet [] ( concat . (map go) . USet.toList ) $ reachingDefVal a
            where
                go (Declaration       addr)         = [variableGenerator analysis $ goDown 1 addr]
                go (MaterializingCall addr)         = [variableGenerator analysis $ addr]
                go (Assignment addr)                = [definedValue addr ml analysis]
                go _                                = []




--
-- * Defined Value Analysis
--

data DefinedValueAnalysis a = DefinedValueAnalysis a
    deriving (Typeable)
    
definedValueAnalysis :: (Typeable a, Typeable v) => DataFlowAnalysis a v -> Solver.AnalysisIdentifier
definedValueAnalysis a = Solver.mkAnalysisIdentifier (DefinedValueAnalysis a) ( "DV-" ++ (show $ analysisIdentifier a) )


--
-- * Defined Value Variable Generator
--

definedValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => NodeAddress                                 -- ^ the assignment interrested in
         -> MemoryLocation                              -- ^ the memory location interrested in
         -> DataFlowAnalysis d v                        -- ^ the underlying data flow analysis this defined value analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested value

definedValue addr ml@(MemoryLocation loc) analysis = var
    where
    
        -- the ID of the produced variable
        varId = Solver.mkIdentifierFromMemoryStatePoint (definedValueAnalysis analysis) ms
            where
                ms = MemoryStatePoint (ProgramPoint addr Post) ml
    
    
        -- the variable
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
        targetRefVar = referenceValue $ goDown 1 $ goDown 2 addr            -- here we have to skip the potentially materializing declaration!
        targetRefVal a = ComposedValue.toValue $ Solver.get a targetRefVar

        -- some checks
        
        isActive a = (USet.isUniverse trgs) || (any pred $ USet.toSet $ trgs)
            where
                trgs = targetRefVal a
                pred (Reference cp _) = cp == loc
                pred _ = False
                
        isPerfectAssign a = (not $ USet.isUniverse trgs) && (USet.size trgs == 1)
            where
                trgs = targetRefVal a
        
        isFullAssign a = isPerfectAssign a && (all pred $ USet.toSet $ targetRefVal a)
            where
                pred (Reference _ DP.Root) = True
                pred _ = False 


        valueDeps a = case () of _ | not $ isActive a -> [Solver.toVar oldStateVar]
                                   | isFullAssign a   -> [Solver.toVar elemValueVar]
                                   | otherwise        -> [Solver.toVar oldStateVar, Solver.toVar elemValueVar]
                          
        oldStateVar = memoryStateValue (MemoryStatePoint (ProgramPoint addr Internal) ml) analysis
        oldStateVal a = Solver.get a oldStateVar         
        
        -- elemValueVar = variableGenerator analysis $ goDown 3 addr
        elemValueVar = 
                if isMaterializingDeclaration $ getNodePair valueDecl
                then memoryStateValue (MemoryStatePoint (ProgramPoint valueDecl Post) (MemoryLocation valueDecl)) analysis
                else variableGenerator analysis (goDown 1 valueDecl)
            where
                valueDecl = goDown 3 addr
        
        
        elemValueVal a = Solver.get a elemValueVar

        updatedValue a = 
                if USet.isUniverse trgs then Solver.top else Solver.join values
            where
                oldVal = oldStateVal a
                elemVal = elemValueVal a
                
                trgs = targetRefVal a
                
                values = concat $ update <$> USet.toList trgs
                
                update (Reference cp dp) | cp == loc = [
                        ComposedValue.setElement dp elemVal oldVal 
                    ] 
                update _ = []
                

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
            Solver.mkVariable varId [] (USet.singleton $ Initial)

        -- skip everything that can statically be considered assignment free
        _ | p == Post && isAssignmentFree addr && (not isParentOfLocation) -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.forward (reachingDefinitions $ MemoryStatePoint (ProgramPoint addr Pre) ml) var

        -- an assignment might be a definition point
        IR.CallExpr | p == Post -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var
                
                dep a = Solver.toVar targetVar :
                    if hasUnknownTargets a || isAssign a then [] else predecessorVars a
                    
                val a = if hasUnknownTargets a then USet.Universe
                        else if isAssign a then USet.singleton $ Assignment addr
                        else predecessorVal a
                        
                targetVar = callableValue $ goDown 1 addr
                targetVal a = ComposedValue.toValue $ Solver.get a targetVar
                
                hasUnknownTargets a = USet.isUniverse $ targetVal a
                        
                isAssign a = any assign $ USet.toSet $ targetVal a 
                    where
                        assign c = isBuiltin (toAddress c) ref_assign
                        ref_assign = getBuiltin addr "ref_assign" 
                        
                predecessorVars a = Solver.getDependencies a defaultVar
                predecessorVal  a = Solver.getLimit a defaultVar 
                         

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

        defaultVar = programPointValue pp idGen analysis []


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
    
    IR.Literal -> not $ isBuiltinByName addr "ref_assign"
    
    IR.LambdaExpr -> any (isBuiltinByName addr) [
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
                
                dep a = (Solver.toVar targetVar) : (Solver.toVar <$> writeSetSummaryVars a) ++ (Solver.toVar <$> refVars a)
                val a = if unknown then USet.Universe else res
                    where
                        wss = writeSetSummaryVal a
                        aps = WS.toAccessPaths wss 
                        
                        unknown = (WS.isUnknown wss) || (any infinite $ refVars a) || (any tooLong aps)
                            where
                                infinite r = USet.isUniverse $ ComposedValue.toValue $ Solver.get a r
                                tooLong (AP.AccessPath _ d) = length d > 1 
                        
                        res = foldr go USet.empty (WS.parameters wss)
                            where
                                go x s = USet.union s $ USet.fromList $ concat $ map toLoc $ USet.toList $ ComposedValue.toValue $ Solver.get a $ refVar x
                                toLoc :: Reference SimpleFieldIndex -> [Location]
                                toLoc (Reference l _ ) = [l]
                                toLoc _ = []

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
                
                writeSetSummaryVal a = Solver.join $ (Solver.get a) <$> writeSetSummaryVars a
                
                refVars a = if WS.isUnknown wss then [] else res 
                     where
                        wss = writeSetSummaryVal a
                        res = refVar <$> WS.parameters wss
        
        _ -> empty
        
    where
        
        empty = Solver.mkVariable (idGen addr) [] (USet.empty) 

        idGen a = Solver.mkIdentifierFromExpression writeSetAnalysis a



-- killed definitions

killedDefinitions :: MemoryStatePoint -> Solver.TypedVar Definitions
killedDefinitions (MemoryStatePoint pp ml) = undefined
