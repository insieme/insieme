{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.CallSite where

import Data.Tree
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
import qualified Data.Set as Set
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR


--
-- * CallSite Results
--

newtype CallSite = CallSite NodeAddress
 deriving (Eq, Ord)

instance Show CallSite where
    show (CallSite na) = "Call@" ++ (prettyShow na)

--
-- * CallSite Lattice
--

type CallSiteSet = Set.Set CallSite

instance Solver.Lattice CallSiteSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs

--
-- * CallSite Analysis
--

callSites :: NodeAddress -> Solver.TypedVar CallSiteSet
callSites addr = case getNode addr of
    
    -- for lambdas and bind exressions: collect all call sites
    Node IR.Lambda   _ -> var
    Node IR.BindExpr _ -> var
    
    -- everything else has no call sites
    _ -> Solver.mkVariable id [] Solver.bot
        
  where
  
    id = Solver.mkIdentifier . ("CS"++) . prettyShow $ addr
        
    var = Solver.mkVariable id [con] Solver.bot
    con = Solver.createConstraint dep val var
    
    allCalls = foldTree collector (getRootIR addr) 
    collector a calls = case getNode a of
        Node IR.CallExpr _  -> a : calls
        _                   -> calls 
    
    allTrgVars = map (\c -> (c , Callable.callableValue $ goDown 1 c ) ) allCalls           
    
    callable = case getNode addr of
        Node IR.Lambda _   -> Callable.Lambda addr
        Node IR.BindExpr _ ->  Callable.Closure addr
    
    dep a = map Solver.toVar (map snd allTrgVars)
    val a = foldr go Set.empty allTrgVars
        where
            go = \(call,var) set ->
                if (Set.member callable (Solver.get a var))
                then Set.insert (CallSite call) set 
                else set
