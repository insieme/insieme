{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
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
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.SymbolicValue (

    SymbolicValue,
    SymbolicValueSet(..),
    symbolicValue,

    SymbolicValueLattice,
    genericSymbolicValue,

) where

-- import Debug.Trace

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Adapter.Utils (pprintTree)
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Reference
import Insieme.Analysis.Utils.CppSemantic
import Insieme.Query

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire as Addr
import qualified Insieme.Inspire.Builder as Builder
import qualified Insieme.Utils.BoundSet as BSet

--
-- * Symbolic Value Lattice
--

type SymbolicValue = IR.Tree

newtype SymbolicValueSet = SymbolicValueSet { unSVS :: BSet.BoundSet BSet.Bound10 SymbolicValue }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Solver.Lattice SymbolicValueSet where
    bot   = SymbolicValueSet BSet.empty
    (SymbolicValueSet x ) `merge` (SymbolicValueSet y) = SymbolicValueSet $ BSet.union x y

    print (SymbolicValueSet BSet.Universe) = "{-all-}"
    print (SymbolicValueSet s            ) = "{" ++ (intercalate "," $ pprintTree <$> BSet.toList s) ++ "}"

instance Solver.ExtLattice SymbolicValueSet where
    top   = SymbolicValueSet BSet.Universe



--
-- * Symbolic Value Analysis
--

data SymbolicValueAnalysis = SymbolicValueAnalysis
    deriving (Typeable)


-- the actual lattice used by the analysis
type SymbolicValueLattice = (ValueTree.Tree SimpleFieldIndex SymbolicValueSet)

--
-- * Symbolic Value Variable and Constraint Generator
--

symbolicValue :: Addr.NodeAddress -> Solver.TypedVar SymbolicValueLattice
symbolicValue = genericSymbolicValue analysis
  where
    -- we just re-use the default version of the generic symbolic value analysis
    analysis = (mkDataFlowAnalysis SymbolicValueAnalysis "S" symbolicValue)


--
-- * A generic symbolic value analysis which can be customized
--

genericSymbolicValue :: (Typeable d) => DataFlowAnalysis d SymbolicValueLattice SimpleFieldIndex -> Addr.NodeAddress -> Solver.TypedVar SymbolicValueLattice
genericSymbolicValue userDefinedAnalysis addr = case getNodeType addr of

    IR.Literal  -> Solver.mkVariable varId [] (compose $ BSet.singleton $ Addr.getNode addr)

    -- handle implicit constructor calls
    IR.Declaration | callsImplicitConstructor addr -> var
      where

        -- retrieve the implicit constructor
        Just ctor = getImplicitConstructor addr

        -- decide based on constructor what to do
        var = case getNodeType ctor of
            IR.LambdaExpr   -> makeExplicit
            t -> error $ "Unexpected ctor type: " ++ (show t)

        makeExplicit = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val makeExplicit

        dep _ = [Solver.toVar initValueVar]
        val a = compose $ BSet.map go $ initValueVal a
          where
            go arg = Builder.mkCallWithArgs resType IR.hsImplicitConstructor [mem,arg]
            Just resType = getType $ Addr.getNode $ Addr.goDown 1 addr
            Just objType = getReferencedType $ resType
            mem = Builder.refTemporary $ objType

        initValueVar = variableGenerator userDefinedAnalysis $ (Addr.goDown 1 addr)
        initValueVal a = extract $ Solver.get a initValueVar

    -- introduce implicit materialization into symbolic value
    IR.Declaration | isMaterializingDeclaration $ Addr.getNode addr -> var
      where
        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var

        dep _ = [Solver.toVar valueVar]
        val a = compose $ BSet.map go $ valueVal a
          where
            go value = Builder.refTemporaryInit value

        valueVar = variableGenerator userDefinedAnalysis $ (Addr.goDown 1 addr)
        valueVal a = extract $ Solver.get a valueVar

    -- handle materializing return statements to produce constructor calls
    IR.ReturnStmt -> var
      where
        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var
        dep _ = [Solver.toVar declVar]
        val a = compose $ BSet.map go $ extract $ declVal a
          where
            go x = case () of
                _ | isCallOfRefTempInit x -> let Just val = getArgument 0 x in val
                _ -> x

        declVar = genericSymbolicValue userDefinedAnalysis $ IR.goDown 0 addr
        declVal a = Solver.get a declVar

    _ -> dataflowValue addr analysis ops

  where

    analysis = userDefinedAnalysis {
        freeVariableHandler = freeVariableHandler,
        initialValueHandler = initialMemoryValue,
        excessiveFileAccessHandler = excessiveFileAccessHandler,
        forwardCtorDtorResultValue = False
    }

    varId = mkVarIdentifier analysis addr

    ops = [ refDeclHandler, operatorHandler ]

    -- a list of symbolic values of the arguments
    argVars = (variableGenerator analysis) <$> ( Addr.goDown 1 ) <$> ( tail . tail $ IR.children addr )

    -- the handler for ref_decl calls
    refDeclHandler = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "ref_decl"
        dep _ _ = []
        val _ _ = compose $ BSet.singleton $ Builder.refTemporary objType
          where
            Just resType = getType $ Addr.getNode $ Addr.goDown 0 addr
            Just objType = getReferencedType $ resType

    -- the one operator handler that covers all other operators
    operatorHandler = OperatorHandler cov dep val
      where

        -- TODO: apply on all builtins, also deriveds
        cov a = (getNodeType a == IR.Literal || toCover) && (not toIgnore)
          where
            -- derived builtins to cover
            toCover  = any (isBuiltin a) [
                        "ref_member_access",
                        "ref_temp", "ref_temp_init",
                        "ref_new", "ref_new_init",
                        "ref_kind_cast", "ref_const_cast", "ref_volatile_cast", "ref_parent_cast",
                         "ref_reinterpret",
                        "num_cast"
                     ] || any (isOperator a) [
                        "IMP_std_colon__colon_array::IMP__operator_subscript_",
                        "IMP_std_colon__colon_array::IMP_at"
                     ] || isConstructor a 
            -- literal builtins to ignore
            toIgnore = any (isBuiltin a) [ "ref_deref", "ref_assign", "ref_decl" ]

        -- if triggered, we will need the symbolic values of all arguments
        dep _ _ = Solver.toVar <$> argVars


        -- to get the new value, we have to take the cross product of all arguments
        val o a = compose $ BSet.map toCall argCombinations

          where

            argVals = extract . (Solver.get a) <$> argVars

            argCombinations = BSet.cartProductL argVals

            toCall args = IR.mkNode IR.CallExpr (resType : trg : decls) []
              where
                decls = toDecl <$> zip (tail $ tail $ IR.children $ IR.node addr ) args

                toDecl (decl,arg) = IR.mkNode IR.Declaration [IR.child 0 decl, arg] []

            callTrg = case getNodeType o of
                IR.Literal -> o
                IR.Lambda  -> Addr.goUpX 3 o
                nt@_ -> error $ "Unexpected node type: " ++ (show nt)

            trg = Addr.getNode $ if isInstantiated then instantiateCall else callTrg
              where

                isInstantiated = Addr.depth callTrg > 2 
                    && getNodeType instantiateCall == IR.CallExpr
                    && isBuiltin (Addr.goDown 1 instantiateCall) "instantiate"

                instantiateCall = Addr.goUp $ Addr.goUp callTrg

            resType = IR.child 0 $ IR.node addr


    -- the handler determinign the value of a free variable

    freeVariableHandler a = var
      where
        var = Solver.mkVariable varId [] val
        val = compose $ BSet.singleton $ Addr.getNode a


    -- the handler assigning values to pre-existing memory locations (e.g. globals)

    initialMemoryValue a =

        if isReference lit then compose $ BSet.singleton value else Solver.top

      where

        lit = Addr.getNode a

        value = Builder.deref lit


    -- the handler processing excessive field accesses

    excessiveFileAccessHandler composedValue f = res
      where
        res = compose $ BSet.map append $ extract composedValue
          where
            append x = Builder.deref ext
              where
                base = IR.child 1 $ IR.child 2 x
                
                ext = case f of
                    (StructField field)   -> Builder.refMember base field
                    (UnionField field)    -> Builder.refMember base field
                    (TupleElementIndex i) -> Builder.refComponent base i
                    (ArrayIndex i)        -> Builder.refArrayElement base i
                    (StdArrayIndex i)     -> Builder.refStdArrayElement base i
                    _                     -> error $ "Unsupported field access: " ++ (show f)


    -- utilities

    extract = unSVS . ComposedValue.toValue
    compose = ComposedValue.toComposed . SymbolicValueSet
