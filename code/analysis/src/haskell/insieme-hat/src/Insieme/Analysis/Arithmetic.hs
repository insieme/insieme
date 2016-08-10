{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Arithmetic where

import Data.Tree
import Insieme.Analysis.Entities.SymbolicFormula
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.Utils (isFreeVariable)
import Insieme.Utils.ParseInt
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex


--
-- * Arithemtic Lattice
--

type SymbolicFormulaSet b = BSet.BoundSet b SymbolicFormula

instance BSet.IsBound b => Solver.Lattice (SymbolicFormulaSet b)  where
    join [] = BSet.empty
    join xs = foldr1 BSet.union xs

--
-- * Arithemtic Value Analysis
--

arithmeticValue :: Addr.NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (SymbolicFormulaSet BSet.Bound10))
arithmeticValue addr = case Addr.getNode addr of
    Node IR.Literal [t, Node (IR.StringValue v) _] | isIntType t -> case parseInt v of
        Just cint -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkConst cint)
        Nothing   -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Constant (Addr.getNode addr) addr)

    Node IR.Variable (t:_) | isIntType t && isFreeVariable addr ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Variable (Addr.getNode addr) addr)

    _ -> dataflowValue addr analysis ops
  where
    analysis = DataFlowAnalysis "A" arithmeticValue (compose $ BSet.Universe)
    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed
    extract = ComposedValue.toValue

    ops = [ add, mul, sub ]

    add = OperatorHandler cov dep (val Ar.addFormula)
      where
        cov a = any (Addr.isBuiltin a) ["int_add", "uint_add"]

    mul = OperatorHandler cov dep (val Ar.mulFormula)
      where
        cov a = any (Addr.isBuiltin a) ["int_mul", "uint_mul"]

    sub = OperatorHandler cov dep (val Ar.subFormula)
      where
        cov a = any (Addr.isBuiltin a) ["int_sub", "uint_sub"]

    lhs = arithmeticValue $ Addr.goDown 2 addr
    rhs = arithmeticValue $ Addr.goDown 3 addr

    dep a = Solver.toVar <$> [lhs, rhs]

    val op a = compose $ (BSet.lift2 op) (extract $ Solver.get a lhs) (extract $ Solver.get a rhs)


    isIntType :: Tree IR.NodeType -> Bool
    isIntType (Node IR.GenericType (Node (IR.StringValue "int" ) _:_)) = True
    isIntType (Node IR.GenericType (Node (IR.StringValue "uint") _:_)) = True
    isIntType _ = False
