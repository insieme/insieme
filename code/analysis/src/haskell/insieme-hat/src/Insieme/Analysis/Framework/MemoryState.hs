{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Insieme.Analysis.Framework.MemoryState where

import Debug.Trace
import Data.Tree
import Data.Typeable
import Data.Foldable
import Insieme.Inspire.NodeAddress

import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.ProgramPoint
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Predecessor
import Insieme.Analysis.Reference

import qualified Data.Set as Set
import qualified Insieme.Inspire as IR
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Utils.UnboundSet as USet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.DataPath hiding (isRoot)
import qualified Insieme.Analysis.Entities.DataPath as DP
import Insieme.Analysis.Entities.FieldIndex


data MemoryLocation = MemoryLocation NodeAddress
    deriving (Eq,Ord,Show)

data MemoryState = MemoryState ProgramPoint MemoryLocation
    deriving (Eq,Ord,Show)



-- define the lattice of definitions

data Definition i = Creation
                | Declaration NodeAddress 
                | MaterializingCall NodeAddress
                | Assignment NodeAddress (DataPath i)
                | PerfectAssignment NodeAddress
    deriving (Eq,Ord,Show)
    
type Definitions i = USet.UnboundSet (Definition i)

instance (FieldIndex i) => Solver.Lattice (Definitions i) where
    bot = USet.empty
    merge = USet.union
    


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
         => MemoryState                                 -- ^ the program point and memory location interested in
         -> DataFlowAnalysis d v                        -- ^ the underlying data flow analysis this memory state analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested state

memoryStateValue ms@(MemoryState pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) analysis = var

    where

        -- extend the underlysing analysis's identifier for the memory state identifier          
        varId = Solver.mkIdentifier (memoryStateAnalysis analysis) addr ("/" ++ (show p) ++ " " ++ show ml)
        
        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var
        
        dep a = (Solver.toVar reachingDefVar) : 
                   (
                     if USet.member Creation $ reachingDefVal a then [] 
                     else (map Solver.toVar $ definingValueVars a) ++ (partialAssingDep a)
                   )
                   
        val a = if USet.member Creation $ reachingDefVal a then ComposedValue.top 
                else Solver.join $ (partialAssingVal a) : (map (Solver.get a) (definingValueVars a)) 
        

        reachingDefVar = reachingDefinitions ms
        reachingDefVal a = Solver.get a reachingDefVar
        
        definingValueVars a = 
                USet.fromUnboundSet [] ( concat . (map go) . USet.toList ) $ reachingDefVal a
            where
                go (Declaration       addr) = [variableGenerator analysis $ goDown 1 addr]
                go (MaterializingCall addr) = [variableGenerator analysis $ addr]         
                go (PerfectAssignment addr) = [variableGenerator analysis $ goDown 3 addr]
                go _                        = []
        
        
        -- partial assignment support --
        
        partialAssignments a =
                USet.fromUnboundSet [] ( (filter pred) . USet.toList ) $ reachingDefVal a
            where
                pred (Assignment _ _) = True
                pred _                = False 
        
        hasPartialAssign = not . null . partialAssignments
                 
        partialAssingDep a = 
                if ( not . hasPartialAssign ) a then []
                else concat $ map go $ partialAssignments a
            where 
                go (Assignment addr _ ) = [
                        Solver.toVar $ elemValueVar addr,
                        Solver.toVar $ predStateVar addr
                    ]


        partialAssingVal a = 
                if ( not . hasPartialAssign ) a then Solver.bot
                else Solver.join $ map go $ partialAssignments a
            where
                go (Assignment addr dp) = ComposedValue.setElement dp elemValue predState
                    where
                        elemValue = Solver.get a $ elemValueVar addr 
                
                        predState = Solver.get a (predStateVar addr)

        elemValueVar assign = variableGenerator analysis $ goDown 3 assign

        predStateVar assign = memoryStateValue (MemoryState (ProgramPoint assign Internal) ml) analysis
        




--
-- * Reaching Definition Analysis
--

data ReachingDefinitionAnalysis = ReachingDefinitionAnalysis
    deriving (Typeable)

reachingDefinitionAnalysis = Solver.mkAnalysisIdentifier ReachingDefinitionAnalysis "RD"


--
-- * Reaching Definition Variable Generator
--

reachingDefinitions :: (FieldIndex i) => MemoryState -> Solver.TypedVar (Definitions i) 
reachingDefinitions (MemoryState pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) = case getNode addr of 

        -- a declaration could be an assignment if it is materializing
        d@(Node IR.Declaration _) | addr == loc && p == Post && isMaterializingDeclaration d -> 
            Solver.mkVariable (idGen addr) [] (USet.singleton $ Declaration addr)
        
        -- a call could be an assignment if it is materializing
        c@(Node IR.CallExpr _) | addr == loc && p == Post && isMaterializingCall c -> 
            Solver.mkVariable (idGen addr) [] (USet.singleton $ MaterializingCall addr)
        
        -- the call could also be the creation point if it is not materializing
        c@(Node IR.CallExpr _) | addr == loc && p == Post -> 
            Solver.mkVariable (idGen addr) [] (USet.singleton Creation)
        
        -- for all the others, the magic is covered by the generic program point value constraint generator    
        _ -> programPointValue pp idGen analysis [assignHandler]
        
    where
    
        analysis pp = reachingDefinitions (MemoryState pp ml)
    
        idGen pp = Solver.mkIdentifier reachingDefinitionAnalysis addr $ ("/" ++ (show pp) ++ " for " ++ (show ml)) 
        
        extract = ComposedValue.toValue
        
        -- a handler for intercepting the interpretation of the ref_assign operator --
        
        assignHandler = OperatorHandler cov dep val
            where 
                cov a = isBuiltin a "ref_assign"
                
                dep a = (Solver.toVar targetRefVar) : (
                        if isEmptyRef a || (isActive a && isSingleRef a) then [] else pdep a
                    ) 

                val a = if isEmptyRef a then Solver.bot                             -- the target reference is not yet determined - wait
                        else if isActive a then                                     -- it is referencing this memory location => do something
                            (if isSingleRef a 
                                then collectLocalDefs a                             -- it only references this memory location 
                                else USet.union (collectLocalDefs a) $ pval a       -- it references this and other memory locations
                            ) 
                        else pval a                                                 -- it does not reference this memory location => no change to reachable definitions
                
                targetRefVar = referenceValue $ goDown 1 $ goDown 2 addr            -- here we have to skip the potentially materializing declaration!
                targetRefVal a = extract $ Solver.get a targetRefVar
                
                isActive a = (USet.isUniverse refs) || (any pred $ USet.toSet refs)
                    where
                        refs = targetRefVal a
                        pred (Reference cp _) = cp == loc  
                
                isEmptyRef a = USet.null $ targetRefVal a        
                
                isSingleRef a = ((not . USet.isUniverse) refs) && (all pred $ USet.toSet refs)
                    where
                        refs = targetRefVal a
                        pred (Reference l _) = l == loc
                
                collectLocalDefs a = if USet.isUniverse targets then USet.Universe
                        else USet.fromList $ concat $ map go $ USet.toList $ targets
                    where
                        targets = targetRefVal a
                        go (Reference cp dp) | (cp == loc) && (DP.isRoot dp) = [PerfectAssignment addr]
                        go (Reference cp dp) | cp == loc = [Assignment addr dp]
                        go _                             = []
                        
                (pdep,pval) = mkPredecessorConstraintCredentials pp analysis



-- killed definitions

killedDefinitions :: MemoryState -> Solver.TypedVar (Definitions i) 
killedDefinitions (MemoryState pp ml) = undefined
