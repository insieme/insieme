{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Arithmetic where

import Data.Tree
import Data.Typeable
import Insieme.Analysis.Entities.SymbolicFormula
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.Utils (isFreeVariable,getType)
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
    bot   = BSet.empty
    merge = BSet.union

instance BSet.IsBound b => Solver.ExtLattice (SymbolicFormulaSet b)  where
    top   = BSet.Universe
  
  
  
--
-- * Arithemtic Value Analysis
--

data ArithmeticAnalysis = ArithmeticAnalysis
    deriving (Typeable)

arithmeticAnalysis = Solver.mkAnalysisIdentifier ArithmeticAnalysis "A"


--
-- * Arithemtic Value Variable Generator
--

arithmeticValue :: Addr.NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (SymbolicFormulaSet BSet.Bound10))
arithmeticValue addr = case Addr.getNode addr of
    Node IR.Literal [t, Node (IR.StringValue v) _] | isIntType t -> case parseInt v of
        Just cint -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkConst cint)
        Nothing   -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Constant (Addr.getNode addr) addr)

    Node IR.Variable (t:_) | isIntType t && isFreeVariable addr ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Variable (Addr.getNode addr) addr)

    Node IR.CastExpr (t:_) | isIntType t -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        con = Solver.forward (arithmeticValue $ Addr.goDown 1 addr) var

    n@_ | isIntExpr && isSideEffectFree addr -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Constant (Addr.getNode addr) addr)
        where
            isIntExpr = maybe False isIntType (getType $ n)
     
    _ -> dataflowValue addr analysis ops
    
  where
    analysis = DataFlowAnalysis ArithmeticAnalysis arithmeticAnalysis arithmeticValue (compose $ BSet.Universe)
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

    
-- TODO: provide a improved implementation of this filter
    
isSideEffectFree :: Addr.NodeAddress -> Bool
isSideEffectFree a = case Addr.getNode a of
    Node IR.Literal _  -> True
    Node IR.Variable _ -> isFreeVariable a
    Node IR.CallExpr _ -> (Addr.isBuiltin (Addr.goDown 1 a) "ref_deref") && (isSideEffectFree $ Addr.goDown 1 $ Addr.goDown 2 a)
    _                  -> False
    
