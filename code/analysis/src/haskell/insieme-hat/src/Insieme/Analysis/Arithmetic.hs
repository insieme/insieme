{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Arithmetic where

import Data.Maybe
import Data.Typeable
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.Query
import Insieme.Inspire.Visit (findDecl)
import Insieme.Utils.ParseInt
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow


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


--
-- * Arithemtic Value Variable Generator
--

arithmeticValue :: Addr.NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (SymbolicFormulaSet BSet.Bound10))
arithmeticValue addr = case Addr.getNode addr of

    IR.NT IR.Literal [t, IR.NT (IR.StringValue v) _] | isIntType t -> case parseInt v of
        Just cint -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkConst cint)
        Nothing   -> Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Constant (Addr.getNode addr) addr)

    IR.NT IR.Variable _ | isLoopIterator addr -> Solver.mkVariable (idGen addr) [] (compose $ BSet.Universe)

    IR.NT IR.Variable (t:_) | isIntType t && isFreeVariable addr ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton $ Ar.mkVar $ Variable (Addr.getNode addr) addr)

    IR.NT IR.CastExpr (t:_) | isIntType t -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        con = Solver.forward (arithmeticValue $ Addr.goDown 1 addr) var

    _ -> dataflowValue addr analysis ops

  where

    analysis = (mkDataFlowAnalysis ArithmeticAnalysis "A" arithmeticValue) {
        initialValueHandler = \a -> compose $ BSet.singleton $ Ar.mkVar $ Constant (Addr.getNode a) a
    }

    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed
    extract = ComposedValue.toValue

    ops = [ add, mul, sub, div, mod, ptrFromRef]

    add = OperatorHandler cov dep (val Ar.addFormula)
      where
        cov a = any (isBuiltin a) $ getBuiltin addr <$> [ "int_add", "uint_add", "gen_add" ]

    sub = OperatorHandler cov dep (val Ar.subFormula)
      where
        cov a = any (isBuiltin a) $ getBuiltin addr <$> [ "int_sub", "uint_sub", "gen_sub" ]

    mul = OperatorHandler cov dep (val Ar.mulFormula)
      where
        cov a = any (isBuiltin a) $ getBuiltin addr <$> [ "int_mul", "uint_mul", "gen_mul" ]

    div = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) $ getBuiltin addr <$> [ "int_div", "uint_div", "gen_div" ]
        val a = compose $ tryDiv (extract $ Solver.get a lhs) (extract $ Solver.get a rhs)

        tryDiv x y = if not (BSet.isUniverse prod) && all (uncurry Ar.canDivide) (BSet.toList prod)
                        then BSet.map (uncurry Ar.divFormula) prod
                        else BSet.Universe
          where
            prod = BSet.cartProduct x y

    mod = OperatorHandler cov dep (val Ar.modFormula)
      where
        cov a = any (isBuiltin a) $ getBuiltin addr <$> [ "int_mod", "uint_mod", "gen_mod" ]

    ptrFromRef = OperatorHandler cov dep val
      where
        cov a = isBuiltin a $ getBuiltin addr "ptr_from_ref"
        dep _ = []
        val a = ComposedValue.composeElements [(component 1,compose res)]
          where
            res = BSet.singleton $ Ar.mkConst 0



    lhs = arithmeticValue $ Addr.goDown 2 addr
    rhs = arithmeticValue $ Addr.goDown 3 addr

    dep a = Solver.toVar <$> [lhs, rhs]

    val op a = compose $ (BSet.lift2 op) (extract $ Solver.get a lhs) (extract $ Solver.get a rhs)



isIntType :: IR.Tree -> Bool
isIntType (IR.NT IR.GenericType (IR.NT (IR.StringValue "int" ) _:_)) = True
isIntType (IR.NT IR.GenericType (IR.NT (IR.StringValue "uint") _:_)) = True
isIntType (IR.NT IR.TypeVariable _ )                                 = True
isIntType _ = False

isFreeVariable :: Addr.NodeAddress -> Bool
isFreeVariable v | (not . isVariable) v = False
isFreeVariable v = isNothing (findDecl v)
