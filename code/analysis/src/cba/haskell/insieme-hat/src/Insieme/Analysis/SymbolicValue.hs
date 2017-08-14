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

--import Debug.Trace

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Typeable
--import Foreign
--import Foreign.C.Types
import GHC.Generics (Generic)
--import Insieme.Adapter (AnalysisResultPtr,CRepPtr,CRepArr,CSetPtr,allocAnalysisResult,dumpIrTree,getTimelimit,passBoundSet,pprintTree)
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Reference
import Insieme.Inspire.Query
--import System.Timeout (timeout)

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
--import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.Builder as Builder
import qualified Insieme.Inspire.NodeAddress as Addr
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
    --print (SymbolicValueSet s            ) = "{" ++ (intercalate "," $ pprintTree <$> BSet.toList s) ++ "}"
    print (SymbolicValueSet s            ) = "{" ++ (intercalate "," $ (const "-omitted for profiling-") <$> BSet.toList s) ++ "}"

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

    _ -> dataflowValue addr analysis ops

  where

    analysis = userDefinedAnalysis {
        freeVariableHandler = freeVariableHandler,
        initialValueHandler = initialMemoryValue,
        excessiveFileAccessHandler = excessiveFileAccessHandler
    }

    varId = mkVarIdentifier analysis addr

    ops = [ operatorHandler ]

    -- a list of symbolic values of the arguments
    argVars = (variableGenerator analysis) <$> ( Addr.goDown 1 ) <$> ( tail . tail $ Addr.getChildren addr )

    -- the one operator handler that covers all operators
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
                        "ref_kind_cast", "ref_const_cast", "ref_volatile_cast", "ref_parent_cast"
                     ]
            -- literal builtins to ignore
            toIgnore = any (isBuiltin a) [ "ref_deref", "ref_assign" ]

        -- if triggered, we will need the symbolic values of all arguments
        dep _ _ = Solver.toVar <$> argVars


        -- to get the new value, we have to take the cross product of all arguments
        val o a = compose $ BSet.map toCall argCombinations

          where

            argVals = extract . (Solver.get a) <$> argVars

            argCombinations = BSet.cartProductL argVals

            toCall args = IR.mkNode IR.CallExpr (resType : trg : decls) []
              where
                decls = toDecl <$> zip (tail $ tail $ IR.getChildren $ Addr.getNode addr ) args

                toDecl (decl,arg) = IR.mkNode IR.Declaration [IR.goDown 0 decl, arg] []

            trg = case getNodeType o of
                IR.Literal -> Addr.getNode o
                IR.Lambda  -> Addr.getNode $ Addr.goUpX 3 o
                nt@_ -> error $ "Unexpected node type: " ++ (show nt)

            resType = IR.goDown 0 $ Addr.getNode addr


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
                base = child 1 $ child 2 x
                
                ext = case f of
                    (Field field) -> Builder.refMember base field
                    (Element i) -> Builder.refComponent base i


    -- utilities

    extract = unSVS . ComposedValue.toValue
    compose = ComposedValue.toComposed . SymbolicValueSet








--
-- * FFI
--

{-

foreign import ccall "hat_c_mk_symbolic_value_set"
  mkCSymbolicValueSet :: CRepArr SymbolicValue -> CLLong -> IO (CSetPtr SymbolicValue)

hsSymbolicValues :: StablePtr Ctx.Context
                 -> StablePtr Addr.NodeAddress
                 -> IO (AnalysisResultPtr (CSetPtr SymbolicValue))
hsSymbolicValues ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let ctx_c =  Ctx.getCContext ctx
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (symbolicValue stmt)
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ serialize ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True =<< serialize ctx_c Solver.top
  where
    serialize :: Ctx.CContext -> SymbolicValueLattice -> IO (CSetPtr SymbolicValue)
    serialize ctx_c = passSymbolicValueSet ctx_c . unSVS . ComposedValue.toValue

passSymbolicValueSet :: Ctx.CContext
                     -> BSet.BoundSet bb SymbolicValue
                     -> IO (CSetPtr SymbolicValue)
passSymbolicValueSet ctx s = do
    passBoundSet passSymbolicValue mkCSymbolicValueSet s
  where

    passSymbolicValue :: SymbolicValue -> IO (CRepPtr SymbolicValue)
    passSymbolicValue s = do
        dumpIrTree ctx s
-}
