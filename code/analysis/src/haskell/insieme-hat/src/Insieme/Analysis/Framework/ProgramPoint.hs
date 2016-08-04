
module Insieme.Analysis.Framework.ProgramPoint where

import Data.Foldable
import Data.Tree
import qualified Data.Set as Set
import Insieme.Inspire.NodeAddress

import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Predecessor
import Insieme.Analysis.Callable
import qualified Insieme.Analysis.Solver as Solver

import Insieme.Analysis.Framework.Utils.OperatorHandler

import qualified Insieme.Inspire as IR


--
-- A generic variable and constraint generator for variables describing
-- properties of program points (e.g. the state of a heap object after
-- the execution of a given expression)
--
programPointValue :: (Solver.Lattice a)
         => ProgramPoint                                    -- ^ the program point for which to compute a variable representing a state value
         -> (ProgramPoint -> Solver.Identifier)             -- ^ a variable ID generator function
         -> (ProgramPoint -> Solver.TypedVar a)             -- ^ a variable generator function for referenced variables
         -> [OperatorHandler a]                             -- ^ a list of operator handlers to intercept the interpretation of certain operators
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information

programPointValue pp@(ProgramPoint a p) idGen analysis ops = case getNode a of
    
        -- allow operator handlers to intercept the interpretation of calls
        Node IR.CallExpr _ | p == Post -> ivar
            where
                
                -- create a variable and an intertangled constraint
                ivar = Solver.mkVariable (idGen pp) [icon] Solver.bot
                icon = Solver.createConstraint idep ival ivar
                
                -- if an handler is active, use the handlers value, else the default
                idep a = (Solver.toVar targetVar) : (
                        if isHandlerActive a then operatorDeps a else dep a
                    )
                ival a = if isHandlerActive a then operatorVal a else val a 
                
                -- the variable storing the target of the call
                targetVar = callableValue (goDown 1 a)
                
                -- test whether any operator handlers are active
                getActiveHandlers a = filter fit ops
                    where
                        targets = Solver.get a targetVar
                        fit o = any ( \t -> covers o $ toAddress t ) targets 
                    
                
                isHandlerActive a = not . null $ getActiveHandlers a 
                
                -- compute the dependencies of the active handlers
                operatorDeps a = concat $ map go $ getActiveHandlers a
                    where
                        go o = dependsOn o a
                
                -- compute the value computed by the active handlers
                operatorVal a = Solver.join $ map go $ getActiveHandlers a
                    where
                        go o = getValue o a
                
            
        -- everything else is just forwarded
        _ -> var 
    
    where
    
        -- by default, the state at each program point is the joined state of all its predecessors 
        var = Solver.mkVariable (idGen pp) [con] Solver.bot
        con = Solver.createConstraint dep val var
        
        (dep,val) = mkPredecessorConstraintCredentials pp analysis
        
        
mkPredecessorConstraintCredentials :: (Solver.Lattice a)
        => ProgramPoint
        -> (ProgramPoint -> Solver.TypedVar a)
        -> (Solver.Assignment -> [Solver.Var],Solver.Assignment -> a)
        
mkPredecessorConstraintCredentials pp analysis = (dep,val)
    where
        predecessorVar = predecessor pp
        predecessorStateVars a = map (\p -> analysis p) (Solver.get a predecessorVar) 
        
        dep a = (Solver.toVar predecessorVar) : map Solver.toVar (predecessorStateVars a)
        val a = Solver.join $ map (Solver.get a) (predecessorStateVars a) 
        