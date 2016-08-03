
module Insieme.Analysis.Framework.Dataflow (
    dataflowValue
) where


import Data.List
import Data.Tree
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set

import qualified Insieme.Inspire as IR
import Insieme.Inspire.Utils
import Insieme.Inspire.NodeAddress

import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.CallSite as CallSite
import qualified Insieme.Analysis.ExitPoint as ExitPoint

import Insieme.Analysis.Reachable


--
-- * Generic Data Flow Value Analysis
--

data OperatorHandler a = OpHandler {
    operator  :: Tree IR.NodeType, 
    dependsOn :: Solver.Assignment -> [Solver.Var],
    getValue  :: Solver.Assignment -> a
}



dataflowValue :: (Solver.Lattice a)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> a                                               -- ^ the top value of the lattice
         -> (NodeAddress -> Solver.Identifier)              -- ^ a variable ID generator function
         -> (NodeAddress -> Solver.TypedVar a)              -- ^ a variable generator function for referenced variables
         -> [OperatorHandler a]                             -- ^ TODO: add support for those operator handlers
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
dataflowValue addr top idGen analysis ops = case getNode addr of

    Node IR.Declaration _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        con = Solver.forward (analysis (goDown 1 addr)) var

    Node IR.Variable _ -> case findDecl addr of
        Just declrAddr -> handleDeclr declrAddr
        _              -> Solver.mkVariable (idGen addr) [] top

    Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con,con2] Solver.bot
        con = Solver.createConstraint dep val var
        
        trg = Callable.callableValue (goDown 1 addr)
        dep a = (Solver.toVar trg) : ( 
                    (map Solver.toVar (getExitPointVars a)) ++ 
                    (map Solver.toVar (getReturnValueVars a)) ++ 
                    (getOperatorDependencies a)
                )
        val a = Solver.join $ (map (Solver.get a) (getReturnValueVars a)) ++ (getOperatorValue a) 
        
        
        -- support for calls to lambda and closures --
        
        getExitPointVars a = Set.fold go [] (Solver.get a trg)
            where
                go = (\c l -> (ExitPoint.exitPoints $ Callable.toAddress c) : l)
                
        getReturnValueVars a = foldr go [] $ map (Solver.get a) (getExitPointVars a)
            where 
                go = \e l -> foldr (\(ExitPoint.ExitPoint r) l -> analysis r : l) l e
        
        -- operator support --
        
        getActiveOperators a = filter f ops
            where 
                f = \o -> any (\l -> getNode (Callable.toAddress l) == operator o) literals
                literals = Set.toList $ Solver.get a trg
        
        getOperatorDependencies a = concat $ map go $ getActiveOperators a
            where
                go = \o -> dependsOn o a
        
        getOperatorValue a = map go $ getActiveOperators a
            where
                go = \o -> getValue o a

        -- temporary fix to support ref_deref before supporting referencences
        con2 = Solver.createConstraint dep2 val2 var
        isDeref = isBuiltin (goDown 1 addr) "ref_deref" 
        dep2 a = if isDeref then [Solver.toVar trgRef] else [] 
        val2 a = if isDeref then (Solver.get a trgRef) else Solver.bot  
        trgRef = analysis $ goDown 2 addr
        -- end temporary fix

    _ -> Solver.mkVariable (idGen addr) [] top
    
  where

    handleDeclr declrAddr = case getNode (goUp declrAddr) of
    
        Node IR.DeclarationStmt _ -> var
          where
            var = Solver.mkVariable (idGen addr) [constraint] Solver.bot
            constraint = Solver.forward (analysis (goDown 0 . goUp $ declrAddr)) var
            
        Node IR.Parameters _ -> var
          where 
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var
            
            n = getIndex declrAddr
            
            callSiteVar = CallSite.callSites (goUp $ goUp declrAddr)
            
            dep a = (Solver.toVar callSiteVar) : (map Solver.toVar (getArgumentVars a)) 
            val a = Solver.join $ map (Solver.get a) (getArgumentVars a)
            
            getArgumentVars a = foldr go [] $ Solver.get a callSiteVar
                where 
                    go = \(CallSite.CallSite call) list -> (analysis $ goDown (n+2) call) : list
                
            
        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"


