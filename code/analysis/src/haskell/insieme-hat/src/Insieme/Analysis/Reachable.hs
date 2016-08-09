
module Insieme.Analysis.Reachable (
    Reachable,
    toBool,
    reachableIn,
    reachableOut
) where


import Data.Tree
import Data.Maybe

import qualified Insieme.Analysis.Solver as Solver

import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

import qualified Insieme.Analysis.Boolean as Boolean
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.Tree as PSTree

--
-- * Reachable Lattice
--

newtype Reachable = Reachable Bool
    deriving (Eq,Show)

instance Solver.Lattice Reachable where
    join = foldr or (Reachable False)
        where
            or (Reachable a) (Reachable c) = Reachable $ a || c 

toBool :: Reachable -> Bool
toBool (Reachable b) = b

--
-- * Reachable-In Analysis
--
reachableIn :: NodeAddress -> Solver.TypedVar Reachable

reachableIn a | isRoot a = Solver.mkVariable (reachableInIdGen a) [] (Reachable True)

reachableIn a = case getNode parent of

    Node IR.Lambda _ -> Solver.mkVariable (idGen a) [] (Reachable True)

    Node IR.CompoundStmt _ -> var
        where 
            n = getIndex a
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = if n == 0 
                then Solver.forward (reachableIn parent) var
                else Solver.forward (reachableOut $ goDown (n-1) parent) var

    Node IR.IfStmt _ -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = case getIndex a of
                0 -> Solver.forward (reachableIn parent) var
                1 -> Solver.forwardIf (compose Boolean.AlwaysTrue)  (Boolean.booleanValue $ goDown 0 parent) (reachableOut $ goDown 0 parent) var
                2 -> Solver.forwardIf (compose Boolean.AlwaysFalse) (Boolean.booleanValue $ goDown 0 parent) (reachableOut $ goDown 0 parent) var 
        
    Node IR.WhileStmt _ -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = case getIndex a of
                0 -> Solver.forward (reachableIn parent) var
                1 -> Solver.forwardIf (compose Boolean.AlwaysTrue)  (Boolean.booleanValue $ goDown 0 parent) (reachableOut $ goDown 0 parent) var

    -- for all others: if the parent is reachable, so is this node
    _ -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = Solver.forward (reachableIn parent) var

    where 
        
        parent = fromJust $ getParent a
        
        idGen = reachableInIdGen
        
        compose = ComposedValue.toComposed



reachableInIdGen = Solver.mkIdentifier . ("R[in]"++) . prettyShow


--
-- * Reachable-Out Analysis
--
reachableOut :: NodeAddress -> Solver.TypedVar Reachable
reachableOut a = case getNode a of

    -- the end of a return is not reachable
    Node IR.ReturnStmt _ -> Solver.mkVariable (idGen a) [] Solver.bot

    -- the end of continue is not reachable
    Node IR.ContinueStmt _ -> Solver.mkVariable (idGen a) [] Solver.bot

    -- the end of break is not reachable
    Node IR.BreakStmt _ -> Solver.mkVariable (idGen a) [] Solver.bot

    -- a commound statement ends if the last statement ends
    Node IR.CompoundStmt ns -> var 
        where
            size = length ns
            var = Solver.mkVariable (idGen a) [cnt] Solver.bot
            cnt = if size == 0 
                then Solver.forward (reachableIn a) var 
                else Solver.forward (reachableOut $ goDown ( (length ns) - 1 ) a) var 

    -- the end of a if is reached if any of the bodies is finished
    Node IR.IfStmt _ -> var
        where
            var = Solver.mkVariable (idGen a) [t,e] Solver.bot
            t = Solver.forward (reachableOut $ goDown 1 a) var
            e = Solver.forward (reachableOut $ goDown 2 a) var

    -- the end of a while is reached after a false condition
    Node IR.WhileStmt _ -> var
        where
            var = Solver.mkVariable (idGen a) [cnt] Solver.bot
            cnt = Solver.forwardIf (ComposedValue.toComposed Boolean.AlwaysFalse) (Boolean.booleanValue $ goDown 0 a) (reachableOut $ goDown 0 a) var

            
    -- everything else: if the begin is reachable, so is the end
    _ -> var
        where 
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = Solver.forward (reachableIn a) var

    where
        
        idGen = Solver.mkIdentifier . ("R[out]"++) . prettyShow
        
