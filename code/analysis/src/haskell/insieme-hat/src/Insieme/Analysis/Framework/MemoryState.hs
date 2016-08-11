{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Framework.MemoryState where

import Debug.Trace
import Data.Tree
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

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue


data MemoryLocation = MemoryLocation NodeAddress
    deriving (Eq,Ord,Show)

data MemoryState = MemoryState ProgramPoint MemoryLocation
    deriving (Eq,Ord,Show)



memoryStateValue :: (Solver.Lattice v)
         => MemoryState                                 -- ^ the program point and memory location interested in
         -> DataFlowAnalysis v                          -- ^ the underlying data flow analysis this memory state analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested state

memoryStateValue ms@(MemoryState pp@(ProgramPoint addr _) ml@(MemoryLocation loc)) analysis = var

    where

          -- extend the underlysing analysis's identifier for the memory state identifier          
          varId = Solver.mkIdentifier $ ('M' : analysisID analysis) ++ (show ms) 
        
          var = Solver.mkVariable varId [con] Solver.bot
          con = Solver.createConstraint dep val var

          dep a = (Solver.toVar reachingDefVar) : (map Solver.toVar $ definingValueVars a)
          val a = Solver.join $ map (Solver.get a) (definingValueVars a)

          reachingDefVar = reachingDefinitions ms
          
          definingValueVars a = map go $ Set.toList $ Solver.get a reachingDefVar
            where
                go (Assignment        addr) = (variableGenerator analysis $ goDown 3 addr)  
                go (Declaration       addr) = (variableGenerator analysis $ goDown 1 addr)
                go (MaterializingCall addr) = (variableGenerator analysis $ addr)


-- define the lattice of definitions

data Definition = Assignment NodeAddress
                | Declaration NodeAddress 
                | MaterializingCall NodeAddress
    deriving (Eq,Ord,Show)
    
type Definitions = Set.Set Definition

instance Solver.Lattice Definitions where
    bot = Set.empty
    merge = Set.union
    
    
toAddress :: Definition -> NodeAddress
toAddress (Assignment addr)        = addr
toAddress (Declaration addr)       = addr
toAddress (MaterializingCall addr) = addr


-- reaching definitions

reachingDefinitions :: MemoryState -> Solver.TypedVar Definitions 
reachingDefinitions (MemoryState pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) = case getNode addr of 

        -- a declaration could be an assignment if it is materializing
        d@(Node IR.Declaration _) | addr == loc && p == Post && isMaterializingDeclaration d -> 
            Solver.mkVariable (idGen addr) [] (Set.singleton $ Declaration addr)
        
        -- a call could be an assignment if it is materializing
        c@(Node IR.CallExpr _) | addr == loc && p == Post && isMaterializingCall c -> 
            Solver.mkVariable (idGen addr) [] (Set.singleton $ MaterializingCall addr)
        
        -- for all the others, the magic is covered by the generic program point value constraint generator    
        _ -> programPointValue pp idGen analysis [handler]
        
    where
    
        analysis pp = reachingDefinitions (MemoryState pp ml)
    
        idGen pp = Solver.mkIdentifier $ ("RD-" ++ (show ml) ++ "@" ++ (show pp)) 
        
        extract = ComposedValue.toValue
        
        -- a handler for intercepting the interpretation of the ref_assign operator --
        
        handler = OperatorHandler cov dep val

        cov a = isBuiltin a "ref_assign"
        
        dep a = (Solver.toVar targetRefVar) : (
                if isEmptyRef a || (isActive a && isSingleRef a) then [] else pdep a
            ) 
        
        val a = if isEmptyRef a then Solver.bot                             -- the target reference is not yet determined - wait
                else if isActive a then                                     -- it is referencing this memory location => do something
                    (if isSingleRef a 
                        then Set.singleton $ Assignment addr                -- it only references this memory location 
                        else Set.insert (Assignment addr) $ pval a          -- it references this and other memory locations
                    ) 
                else pval a                                                 -- it does not reference this memory location => no change to reachable definitions
        
        targetRefVar = referenceValue $ goDown 1 $ goDown 2 addr            -- here we have to skip the potentially materializing declaration!
        
        isActive a = any pred $ extract $ Solver.get a targetRefVar
            where
                pred (Reference cp _) = cp == loc  
        
        isEmptyRef a = Set.null $ extract $ Solver.get a targetRefVar        
        
        isSingleRef a = (==1) . Set.size $ extract $ Solver.get a targetRefVar
                
        (pdep,pval) = mkPredecessorConstraintCredentials pp analysis



-- killed definitions

killedDefinitions :: MemoryState -> Solver.TypedVar Definitions 
killedDefinitions (MemoryState pp ml) =
        
        -- the magic is covered by the generic program point value constraint generator  
        programPointValue pp idGen (\pp -> killedDefinitions (MemoryState pp ml)) [undefined]
        
    where
    
        idGen pp = Solver.mkIdentifier $ ("KD-" ++ (show ml) ++ "@" ++ (show pp))
        
        handler = undefined 
