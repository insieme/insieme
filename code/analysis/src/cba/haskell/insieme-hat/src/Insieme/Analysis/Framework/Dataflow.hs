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

{-# LANGUAGE FlexibleContexts #-}

module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(
        DataFlowAnalysis,
        analysisIdentifier,
        variableGenerator,
        topValue,
        iteratorVariableHandler,
        freeVariableHandler,
        entryPointParameterHandler,
        initialValueHandler,
        initialValue,
        uninitializedValue,
        excessiveFileAccessHandler,
        unknownOperatorHandler,
        forwardCtorDtorResultValue,
        implicitCtorHandler
    ),
    mkDataFlowAnalysis,
    mkVarIdentifier,
    mkConstant,
    dataflowValue
) where


import GHC.Stack

import Data.Foldable
import Data.Maybe
import Data.Typeable
import Debug.Trace
import qualified Data.Set as Set


import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import Insieme.Query

import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.MemoryState
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Utils.CppSemantic
import qualified Insieme.Analysis.Arithmetic as Arithmetic
import qualified Insieme.Analysis.CallSite as CallSite
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.ExitPoint as ExitPoint
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Reference as Reference
import qualified Insieme.Solver as Solver
import qualified Insieme.Utils.BoundSet as BSet

--
-- * Data Flow Analysis summary
--

data DataFlowAnalysis a v i = DataFlowAnalysis {
    analysis                   :: a,                                         -- ^ the analysis type token
    analysisIdentifier         :: Solver.AnalysisIdentifier,                 -- ^ the analysis identifier
    variableGenerator          :: NodeAddress -> Solver.TypedVar v,          -- ^ the variable generator of the represented analysis
    topValue                   :: v,                                         -- ^ the top value of this analysis
    iteratorVariableHandler    :: NodeAddress -> Solver.TypedVar v,          -- ^ a function creating the constraints for the given iterator variable
    freeVariableHandler        :: NodeAddress -> Solver.TypedVar v,          -- ^ a function computing the value of a free variable
    entryPointParameterHandler :: NodeAddress -> Solver.TypedVar v,          -- ^ a function computing the value of a entry point parameter
    initialValueHandler        :: NodeAddress -> v,                          -- ^ a function computing the initial value of a memory location
    initialValue               :: v,                                         -- ^ default value of a memory location
    uninitializedValue         :: v,                                         -- ^ value of an uninitialized memory location
    excessiveFileAccessHandler :: v -> i -> v,                               -- ^ a handler processing excessive field accesses (if ref_narrow calls navigate too deep)
    unknownOperatorHandler     :: NodeAddress -> v,                          -- ^ a handler invoked for unknown operators
    forwardCtorDtorResultValue :: Bool,                                      -- ^ a flag to enable / disable the implicit return of constructors and destructurs
    implicitCtorHandler        :: Maybe (NodeAddress -> Solver.TypedVar v)   -- ^ an optional override for the handling of constructors in the defined value analysis
}

-- a function creating a simple data flow analysis
mkDataFlowAnalysis :: (Typeable a, Solver.ExtLattice v) => a -> String -> (NodeAddress -> Solver.TypedVar v) -> DataFlowAnalysis a v i
mkDataFlowAnalysis a s g = res
    where
        res = DataFlowAnalysis a aid g top justTop justTop justTop (\_ -> top) top top failOnAccess unknownTarget True Nothing
        aid = (Solver.mkAnalysisIdentifier a s)
        justTop a = mkConstant res a top
        top = Solver.top
        failOnAccess _ _ = Solver.top
        unknownTarget _ = Solver.top


-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a v i -> NodeAddress -> Solver.Identifier
mkVarIdentifier a n = Solver.mkIdentifierFromExpression (analysisIdentifier a) n


-- a function creating a data flow analysis variable representing a constant value
mkConstant :: (Typeable a, Solver.ExtLattice v) => DataFlowAnalysis a v i -> NodeAddress -> v -> Solver.TypedVar v
mkConstant a n v = var
    where
        var = Solver.mkVariable (idGen n) [] v
        idGen = mkVarIdentifier a


--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (HasCallStack, ComposedValue.ComposedValue a i v, Typeable d)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis d a i                          -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
dataflowValue addr analysis ops = case I.getNode addr of

    I.Node I.Variable _ -> case I.findDecl addr of
            Just declAddr | addr == declAddr -> handleDeclr declAddr
            Just declAddr  -> var
              where
                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.forward (varGen declAddr) var
--            Just declrAddr -> handleDeclr declrAddr              -- this variable is declared, use declared value
            _              -> freeVariableHandler analysis addr  -- it is a free variable, ask the analysis what to do with it


    I.Node I.CallExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [knownTargets,unknownTarget] Solver.bot
        knownTargets = Solver.createConstraint dep val var

        dep a = (Solver.toVar callTargetVar) : (
                    (map Solver.toVar (getExitPointVars a)) ++
                    (map Solver.toVar (getReturnValueVars a)) ++
                    (map Solver.toVar (getCtorDtorVars a)) ++
                    (getOperatorDependencies a)
                )
        val a = Solver.join $ (getReturnValues a) ++ (getCtorDtorValues a) ++ (getOperatorValue a)


        -- utilities --

        callTargetVar = Callable.callableValue (I.goDown 1 addr)

        callTargetVal a = ComposedValue.toValue $ Solver.get a callTargetVar

        filterInterceptedLambdas ts = Set.filter (not . covered) (BSet.toSet ts)
            where
                covered (Callable.Lambda addr) = any (\o -> covers o addr) extOps
                covered _ = False


        -- support for calls to lambda and closures --

        getExitPointVars a = if BSet.isUniverse targets then [] else go <$> Set.toList uninterceptedTargets
            where
                targets = callTargetVal a
                uninterceptedTargets = filterInterceptedLambdas targets
                go c = (ExitPoint.exitPoints $ Callable.toAddress c)



        getReturnValueVars a = concat $ map go $ map (toList . Solver.get a) (getExitPointVars a)
            where
                go e = concat $ map resolve e

                resolve (ExitPoint.ExitPoint r) = case getNodeType r of
                    I.CompoundStmt | isConstructorOrDestructor $ I.getNode $ fromJust $ I.getParent r -> []
                    _               -> [varGen r]

        getReturnValues a = map (Solver.get a) (getReturnValueVars a)


        -- support for calls to constructors and destructors --

        enableCtorDtorForward = forwardCtorDtorResultValue analysis

        isCtorOrDtor c = isConstructorOrDestructor $ I.getNode $ Callable.toAddress c

        getCtorDtorVars a = if not enableCtorDtorForward || BSet.isUniverse targets then [] else concat $ go <$> Set.toList uninterceptedTargets
            where
                targets = callTargetVal a
                uninterceptedTargets = filterInterceptedLambdas targets

                go c = if isCtorOrDtor c then [varGen $ I.goDown 1 $ I.goDown 2 addr] else []

        getCtorDtorValues a = (Solver.get a) <$> getCtorDtorVars a


        -- operator support --

        getActiveOperators a = if BSet.isUniverse targets then [] else concatMap f extOps
            where
                targets = callTargetVal a
                f o = mapMaybe go $ BSet.toList targets
                    where
                        go l = if covers o trg then Just (o,trg) else Nothing
                            where
                                trg = Callable.toAddress l

        getOperatorDependencies a = concat $ map go $ getActiveOperators a
            where
                go (o,t) = dependsOn o t a

        getOperatorValue a = map go $ getActiveOperators a
            where
                go (o,t) = getValue o t a


        -- support calls to unknown literal --

        unknownTarget = Solver.createConstraint dep val var
            where
                dep _ = [Solver.toVar callTargetVar]
                val a = if BSet.isUniverse targets then top else uncoveredOperatorValue
                    where

                        targets = callTargetVal a

                        uncoveredLiterals = filter f $ BSet.toList $ targets
                            where
                                f c | isCtorOrDtor c   = False
                                f (Callable.Literal a) = not $ any (\h -> covers h a) extOps
                                f _                    = False

                        uncoveredOperatorValue = Solver.join $ (unknownOperatorHandler analysis) . Callable.toAddress <$> uncoveredLiterals



    I.Node I.TupleExpr [_,I.Node I.Expressions args] -> var
        where
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var

            dep _ = Solver.toVar <$> componentValueVars
            val a = ComposedValue.composeElements $ zip (component <$> [0 ..]) (map (Solver.get a) componentValueVars)

            componentValueVars = go <$> [0 .. ((length args) - 1) ]
                where
                    go i = varGen (I.goDown i $ I.goDown 1 $ addr)


    decl@(I.Node I.Declaration _) -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot

        con = Solver.forward
            (
                if Reference.isMaterializingDeclaration decl
                then memoryStateValue (MemoryStatePoint (ProgramPoint addr Post) (MemoryLocation addr)) analysis
                else varGen (I.goDown 1 addr)
            )
            var

    I.Node I.InitExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot

        con = Solver.forward (varGen (I.goDown 1 addr)) var

    I.Node I.ReturnStmt _ -> var
      where
        -- return statements return the value of the temporary memory location they create
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        con = Solver.forward memVar var
        memVar = memoryStateValue (MemoryStatePoint (ProgramPoint r Post) (MemoryLocation r)) analysis
        r = I.goDown 0 addr

    _ -> unknown

  where

    top = topValue analysis

    idGen = mkVarIdentifier analysis

    varGen = variableGenerator analysis

    unknown = Solver.mkVariable (idGen addr) [] top


    -- variable declaration handler

    handleDeclr declrAddr = case getNodeType (I.goUp declrAddr) of

        -- handle iterator variables
        I.DeclarationStmt | ((I.depth declrAddr > 2) && ((getNodeType $ I.goUp $ I.goUp declrAddr) == I.ForStmt))  -> var
          where
            var =  iteratorVariableHandler analysis addr

        -- handle local variables
        I.DeclarationStmt -> var
          where
            var = Solver.mkVariable (idGen addr) [constraint] Solver.bot
            constraint = Solver.forward (varGen (I.goDown 0 . I.goUp $ declrAddr)) var

        -- handle parameters
        I.Parameters -> case () of

            _ | isImplicitCtorOrDtorParameter declrAddr -> iVar
              | isEntryPointParameter declrAddr         -> entryPointParameterHandler analysis declrAddr
              | otherwise                               -> var

          where

            -- implicit ctor/dtor call --

            iVar = Solver.mkVariable (idGen addr) [iCon] Solver.bot
            iCon = Solver.forward arg iVar

            objDecl = getEnclosingDeclaration declrAddr

            arg = varGen $ I.goDown (I.getIndex declrAddr) objDecl   -- we use the type of the declaration as a fake-address for the this pointer

            -- standard parameter --
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var

            n = I.getIndex declrAddr

            callSiteVar = CallSite.callSites (I.goUp $ I.goUp declrAddr)

            dep a = (Solver.toVar callSiteVar) : (map Solver.toVar (getArgumentVars a))
            val a = Solver.join $ map (Solver.get a) (getArgumentVars a)

            getArgumentVars a = foldr go [] $ Solver.get a callSiteVar
                where
                    go = \(CallSite.CallSite call) list -> (varGen $ I.goDown (n+2) call) : list


        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"

    -- add support for predefined operator handlers --

    extOps = readHandler : tupleMemberAccessHandler : instantiateHandler : ops

    -- support the ref_deref operation (read)

    readHandler = OperatorHandler cov dep val
        where
            cov a = isBuiltin a "ref_deref"

            dep _ _ = [Solver.toVar valueVar]

            val _ a = Solver.get a valueVar

            valueVar = referencedValue (I.goDown 1 $ I.goDown 2 addr) analysis


    -- support the tuple_member_access operation (read from tuple component)

    tupleMemberAccessHandler  = OperatorHandler cov dep val
        where
            cov a = isBuiltin a "tuple_member_access"

            dep _ _ = Solver.toVar indexValueVar : Solver.toVar tupleValueVar : []

            val _ a = if BSet.isUniverse indices
                    then top
                    else Solver.join $ map go dataPaths
                where

                    indices = Arithmetic.unSFS $ ComposedValue.toValue (Solver.get a indexValueVar)

                    fieldIndices = tupleElementIndex <$> (BSet.toList indices)

                    dataPaths = step <$> fieldIndices

                    tupleValue = Solver.get a tupleValueVar

                    go i = ComposedValue.getElement i tupleValue


            indexValueVar = Arithmetic.arithmeticValue $ I.goDown 3 addr

            tupleValueVar = varGen $ I.goDown 1 $ I.goDown 2 addr


    -- support the instantiate operator (specialization of a generic values and functions)

    instantiateHandler = OperatorHandler cov dep val
        where
            cov a = any (isBuiltin a) ["instantiate_fun","instantiate_ctor","instantiate_dtor","instantiate_member"]

            dep _ _ = [Solver.toVar valueVar]

            val _ a = Solver.get a valueVar

            arg1 = I.goDown 1 $ I.goDown 2 addr
            arg2 = I.goDown 1 $ I.goDown 3 addr

            valueVar = varGen $ if isLiteral arg1 then arg2 else arg1
