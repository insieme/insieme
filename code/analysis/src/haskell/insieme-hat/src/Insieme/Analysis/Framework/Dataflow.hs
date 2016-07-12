
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

import qualified Insieme.Callable as Callable
import Insieme.Analysis.Callable



--
-- * Generic Data Flow Value Analysis
--


dataflowValue :: (Solver.Lattice a)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> a                                               -- ^ the top value of the lattice
         -> (NodeAddress -> Solver.Identifier)              -- ^ a variable ID generator function
         -> (NodeAddress -> Solver.TypedVar a)              -- ^ a variable generator function for referenced variables
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
dataflowValue addr top idGen analysis = case getNode addr of

    Node IR.Declaration _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        con = Solver.forward (analysis (goDown 1 addr)) var

    Node IR.Variable _ -> case findDeclr addr of
        Just declrAddr -> handleDeclr declrAddr
        _              -> Solver.mkVariable (idGen addr) [] top

    Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con,con2] Solver.bot
        con = Solver.createConstraint dep val var
        
        trg = callableValue (goDown 1 addr)
        dep a = (Solver.toVar trg) : (map Solver.toVar (getReturnVars a))
        val a = Solver.join $ map (Solver.get a) (getReturnVars a) 
        getReturnVars a = Set.fold (\c l -> (map (\a -> analysis a) (getReturnPoints c)) ++ l ) [] (Solver.get a trg)

        -- temporary fix to support ref_deref before supporting referencences
        con2 = Solver.createConstraint dep2 val2 var
        isDeref = case getNode (goDown 1 addr) of 
            Node IR.Literal [_, Node (IR.StringValue "ref_deref") _  ] -> True
            _ -> False
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
            
            allCalls = foldTree collector (getRoot declrAddr) 
            collector a calls = case getNode a of
                Node IR.CallExpr _  -> a : calls
                _                   -> calls 
            
            allTrgVars = map (\c -> (c , callableValue $ goDown 1 c ) ) allCalls           
            
            callableAddr = goUp $ goUp declrAddr
            callable = case getNode callableAddr of
                Node IR.Lambda _   -> Callable.Lambda callableAddr
                Node IR.BindExpr _ ->  Callable.Closure callableAddr
            
            dep a = (map Solver.toVar (map snd allTrgVars)) ++ (map Solver.toVar (getArgumentVars a)) 
            val a = Solver.join $ map (Solver.get a) (getArgumentVars a)
            getArgumentVars a = foldr go [] allTrgVars
                where
                    go = \(call,var) list ->  
                            if (Set.member callable (Solver.get a var)) 
                            then (analysis $ goDown (n+2) call) : list 
                            else list
                
            
        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"


getReturnPoints :: Callable.Callable -> [NodeAddress]
getReturnPoints (Callable.Closure x ) = [goDown 2 x] 
getReturnPoints (Callable.Literal x ) = [] --trace "Inspecting a literal" $ error "No return point in literals implemented!"
getReturnPoints (Callable.Lambda x ) = collectReturnPoints x


collectReturnPoints :: NodeAddress -> [NodeAddress]
collectReturnPoints = foldAddressPrune collector filter
    where
        filter cur = 
            case getNode cur of
                Node IR.LambdaExpr _ -> True
                _ -> False
        collector cur returns =
            case getNode cur of 
                Node IR.ReturnStmt _ -> (goDown 0 cur : returns)
                _ -> returns
        