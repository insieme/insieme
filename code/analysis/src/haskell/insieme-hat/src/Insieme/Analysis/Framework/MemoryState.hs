{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Framework.MemoryState where

import Data.Foldable
import Insieme.Inspire.NodeAddress

import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.ProgramPoint
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Predecessor
import Insieme.Analysis.Reference

import qualified Data.Set as Set
import qualified Insieme.Analysis.Solver as Solver



data MemoryLocation = MemoryLocation NodeAddress
    deriving (Eq,Ord,Show)

data MemoryState = MemoryState ProgramPoint MemoryLocation
    deriving (Eq,Ord,Show)



memoryStateValue :: (Solver.Lattice a)
         => MemoryState                                     -- ^ the program point and memory location interested in
         -> a                                               -- ^ the top value of the processed lattice
         -> (NodeAddress -> Solver.Identifier)              -- ^ the variable ID generator function of the associated value analysis
         -> NodeAddress -> Solver.TypedVar a                -- ^ the associated value analysis
         -> Solver.TypedVar a                               -- ^ the analysis variable representing the requested state

memoryStateValue (MemoryState pp@(ProgramPoint addr _) ml@(MemoryLocation loc)) top analysisIdGen analysis = var
    where
          var = undefined  
--        var = Solver.mkVariable (idGen pp) [con] Solver.bot
--        con = Solver.createConstraint dep val var
--        
--        idGen pp = Solver.mkIdentifier  





-- define the lattice of definitions

newtype Definition = Definition NodeAddress
    deriving (Eq,Ord,Show)
    
type Definitions = Set.Set Definition

instance Solver.Lattice Definitions where
    bot = Set.empty
    merge = Set.union


-- reaching definitions

reachingDefinitions :: MemoryState -> Solver.TypedVar Definitions 
reachingDefinitions (MemoryState pp@(ProgramPoint addr _) ml@(MemoryLocation loc)) = 
        
        -- the magic is covered by the generic program point value constraint generator    
        programPointValue pp idGen analysis [handler]
        
    where
    
        analysis pp = reachingDefinitions (MemoryState pp ml)
    
        idGen pp = Solver.mkIdentifier $ ("RD-" ++ (show ml) ++ "@" ++ (show pp)) 
        
        -- a handler for intercepting the interpretation of the ref_assign operator --
        
        handler = OperatorHandler cov dep val

        cov a = isBuiltin a "ref_assign"
        
        dep a = (Solver.toVar targetRefVar) : (
                if isEmptyRef a || (isActive a && isSingleRef a) then [] else pdep a
            ) 
        
        val a = if isEmptyRef a then Solver.bot                             -- the target reference is not yet determined - wait
                else if isActive a then                                     -- it is referencing this memory location => do something
                    (if isSingleRef a 
                        then Set.singleton $ Definition addr                -- it only references this memory location 
                        else Set.insert (Definition addr) $ pval a          -- it references this and other memory locations
                    ) 
                else pval a                                                 -- it does not reference this memory location => no change to reachable definitions
        
        targetRefVar = referenceValue $ goDown 2 addr
        
        isActive a = any pred $ Solver.get a targetRefVar
            where
                pred (Reference cp _) = cp == loc  
        
        isEmptyRef a = Set.null $ Solver.get a targetRefVar        
        
        isSingleRef a = (==1) . Set.size $ Solver.get a targetRefVar
                
        (pdep,pval) = mkPredecessorConstraintCredentials pp analysis



-- killed definitions

killedDefinitions :: MemoryState -> Solver.TypedVar Definitions 
killedDefinitions (MemoryState pp ml) =
        
        -- the magic is covered by the generic program point value constraint generator  
        programPointValue pp idGen (\pp -> killedDefinitions (MemoryState pp ml)) [undefined]
        
    where
    
        idGen pp = Solver.mkIdentifier $ ("KD-" ++ (show ml) ++ "@" ++ (show pp))
        
        handler = undefined 
