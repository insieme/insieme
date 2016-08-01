{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Predecessor (
    PredecessorList,
    predecessor
) where


import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver

import Debug.Trace
import Data.Tree
import Data.Maybe
import Insieme.Inspire.NodeAddress
import Insieme.Analysis.Callable
import Insieme.Analysis.CallSite
import Insieme.Analysis.ExitPoint
import qualified Insieme.Inspire as IR



--
-- * Predecessor Lattice
--

type PredecessorList = [ProgramPoint]

instance Solver.Lattice PredecessorList where
    join [] = []
    join xs = foldr1 (++) xs

--
-- * Predecessor Analysis
--


predecessor :: ProgramPoint -> Solver.TypedVar PredecessorList

-- Predecessor rules for pre program points:
predecessor p@(ProgramPoint a Pre) | isRoot a = var
    where 
        var = Solver.mkVariable (idGen p) [] []


predecessor p@(ProgramPoint a Pre) = case getNode parent of
    
    Node IR.CallExpr children -> single $
            if i == (length children) - 1 
            then ProgramPoint parent Pre                   -- start with last argument
            else ProgramPoint (goDown (i+1) parent) Post   -- eval arguments in reverse order
    
    Node IR.CompoundStmt stmts -> single $
            if i == 0                                        -- if it is the first statement
            then ProgramPoint parent Pre                   -- then go to the pre-state of the compound statement
            else ProgramPoint (goDown (i-1) parent) Post   -- else to the post state of the previous statement
    
    Node IR.Lambda   _ -> call_sites
    
    Node IR.BindExpr _ -> call_sites
    
    _ -> trace ( " Unhandled Pre Program Point: " ++ (show p) ) $ error "unhandled case"
    
  where
  
    parent = fromJust $ getParent a
    i = getIndex a
  
    single :: ProgramPoint -> Solver.TypedVar PredecessorList
    single x = Solver.mkVariable (idGen p) [] [x] 
    
    call_sites = Solver.mkVariable (idGen p) [con] []
    con = Solver.createConstraint dep val call_sites
    
    dep a = [Solver.toVar callSitesVar]
    val a = foldr go [] $ Solver.get a callSitesVar
        where
            go = \(CallSite call) list -> (ProgramPoint (goDown 1 call) Post) : list
    
    callSitesVar = callSites parent
    
    
-- Predecessor rules for internal program points:    
predecessor  p@(ProgramPoint a Internal) = case getNode a of

    -- link to exit points of potential target functions
    Node IR.CallExpr _ -> var 
        where
         
            var = Solver.mkVariable (idGen p) [con] []
            con = Solver.createConstraint dep val var
         
            dep a = Solver.toVar callableVar : map Solver.toVar (exitPointVars a)
            val a = foldr go [] (exitPointVars a)
                where
                    go = \e l -> foldr (\(ExitPoint r) l -> (ProgramPoint r Post) : l) l (Solver.get a e)
         
            callableVar = callableValue (goDown 1 a)
            
            exitPointVars a = foldr (\t l -> exitPoints (toAddress t) : l) [] (Solver.get a callableVar)
                
    _ -> trace ( " Unhandled Internal Program Point: " ++ (show p) ) $ error "unhandled case"



-- Predecessor rules for post program points:
predecessor p@(ProgramPoint a Post) = case getNode a of

    -- basic expressions are directly switching from Pre to Post
    Node IR.Variable         _ -> pre
    Node IR.Literal          _ -> pre
    Node IR.LambdaExpr       _ -> pre
    Node IR.LambdaReference  _ -> pre
    Node IR.BindExpr         _ -> pre
    
    -- call expressions are switching from Internal to Post
    Node IR.CallExpr _ -> single $ ProgramPoint a Internal
    
    -- compound statements
    Node IR.CompoundStmt []    -> pre
    Node IR.CompoundStmt stmts -> single $ ProgramPoint (goDown ((length stmts) - 1) a) Post
    
    _ -> trace ( " Unhandled Post Program Point: " ++ (show p) ) $ error "unhandled case"
    
  where
  
    single p = Solver.mkVariable (idGen p) [] [p] 
    
    pre = single $ ProgramPoint a Pre


-- variable ID generator
idGen :: ProgramPoint -> Solver.Identifier
idGen p = Solver.mkIdentifier $ "pred_of:" ++ (show p)
