{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.ExitPoint where

import Data.List
import Data.Maybe
import Data.Tree
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
import qualified Data.Set as Set
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.Reachable as Reachable
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR


--
-- * ExitPoint Results
--

newtype ExitPoint = ExitPoint NodeAddress
 deriving (Eq, Ord)

instance Show ExitPoint where
    show (ExitPoint na) = "Exit@" ++ (prettyShow na)

--
-- * ExitPoint Lattice
--

type ExitPointSet = Set.Set ExitPoint

instance Solver.Lattice ExitPointSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs

--
-- * ExitPoint Analysis
--

exitPoints :: NodeAddress -> Solver.TypedVar ExitPointSet
exitPoints addr = case getNode addr of
    
    -- for lambdas: collect all reachable return statements and end of body
    Node IR.Lambda   _ -> var
        where
            var = Solver.mkVariable id [con] Solver.bot
            con = Solver.createConstraint dep val var
            
            dep a = (map Solver.toVar (getReturnReachVars a))
            val a = foldr go Set.empty returns
                where
                    go = \r s -> 
                        if Reachable.toBool $ Solver.get a (Reachable.reachableIn r) 
                        then Set.insert (ExitPoint r) s
                        else s 

            returns = collectReturns addr

            getReturnReachVars a = map (\a -> Reachable.reachableIn a) returns
    
    -- for bind expressions: use nested call expression
    Node IR.BindExpr _ -> Solver.mkVariable id [] $ Set.singleton $ ExitPoint $ goDown 2 addr 
    
    -- everything else has no call sites
    _ -> Solver.mkVariable id [] Solver.bot
        
  where
  
    id = Solver.mkIdentifier . ("EP"++) . prettyShow $ addr
    
    

collectReturns :: NodeAddress -> [NodeAddress]
collectReturns = foldAddressPrune collector filter
    where
        filter cur = 
            case getNode cur of
                Node IR.LambdaExpr _ -> True
                _ -> False
        collector cur returns =
            case getNode cur of 
                Node IR.ReturnStmt _ -> (goDown 0 cur : returns)
                _ -> returns
    