{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Arithmetic where

import Data.Tree
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Arithmetic
import Insieme.Utils.ParseInt
import Insieme.Inspire.Utils (isFreeVariable)
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Framework.Dataflow

--
-- * Arithemtic Symbol
--

data Symbol = Constant {getNode :: (Tree IR.NodeType), getAddr :: Addr.NodeAddress }
            | Variable {getNode :: (Tree IR.NodeType), getAddr :: Addr.NodeAddress }

instance Eq Symbol where
    x == y = (getNode x) == (getNode y)

instance Ord Symbol where
    compare x y = compare (getNode x) (getNode y)

instance Show Symbol where
    show (Constant (Node IR.Literal  [_, Node (IR.StringValue v) _]) _) = v
    show (Variable (Node IR.Variable [_, Node (IR.UIntValue   v) _]) _) = "v" ++ show v
    show _ = "???"

--
-- * Arithemtic Lattice
--

type SymbolicFormula = Formula CInt Symbol
type SymbolicFormulaSet b = BSet.BoundSet b SymbolicFormula

instance BSet.IsBound b => Solver.Lattice (SymbolicFormulaSet b)  where
    join [] = BSet.empty
    join xs = foldr1 BSet.union xs

--
-- * Arithemtic Value Analysis
--

arithmeticValue :: Addr.NodeAddress -> Solver.TypedVar (SymbolicFormulaSet BSet.Bound10)
arithmeticValue addr = case Addr.getNode addr of
    Node IR.Literal [t, Node (IR.StringValue v) _] | isIntType t -> case parseInt v of
        Just cint -> Solver.mkVariable (idGen addr) [] (BSet.singleton $ mkConst cint)
        Nothing   -> Solver.mkVariable (idGen addr) [] (BSet.singleton $ mkVar $ Constant (Addr.getNode addr) addr)

    Node IR.Variable (t:_) | isIntType t && isFreeVariable addr ->
        Solver.mkVariable (idGen addr) [] (BSet.singleton $ mkVar $ Variable (Addr.getNode addr) addr)

    _ -> dataflowValue addr analysis ops
  where
    analysis = DataFlowAnalysis "A" arithmeticValue BSet.Universe
    idGen = mkVarIdentifier analysis

    ops = [ add, mul, sub ]

    add = OperatorHandler cov dep (val addFormula)
      where
        cov a = any (Addr.isBuiltin a) ["int_add", "uint_add"]

    mul = OperatorHandler cov dep (val mulFormula)
      where
        cov a = any (Addr.isBuiltin a) ["int_mul", "uint_mul"]

    sub = OperatorHandler cov dep (val subFormula)
      where
        cov a = any (Addr.isBuiltin a) ["int_sub", "uint_sub"]

    lhs = arithmeticValue $ Addr.goDown 2 addr
    rhs = arithmeticValue $ Addr.goDown 3 addr

    dep a = Solver.toVar <$> [lhs, rhs]

    val op a = BSet.map (uncurry op) $ BSet.cartProduct (Solver.get a lhs) (Solver.get a rhs)


    isIntType :: Tree IR.NodeType -> Bool
    isIntType (Node IR.GenericType (Node (IR.StringValue "int" ) _:_)) = True
    isIntType (Node IR.GenericType (Node (IR.StringValue "uint") _:_)) = True
    isIntType _ = False
