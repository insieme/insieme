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
        initValueHandler,
        excessiveFileAccessHandler
    ),
    mkDataFlowAnalysis,
    mkVarIdentifier,
    mkConstant,
    dataflowValue
) where


import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Typeable
import Debug.Trace
import Insieme.Analysis.Entities.DataPath hiding (isRoot)
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.MemoryState
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Utils.CppSemantic
import Insieme.Inspire.NodeAddress hiding (getPath)
import Insieme.Inspire.Query
import Insieme.Inspire.Visit
import qualified Data.Set as Set
import qualified Insieme.Analysis.Arithmetic as Arithmetic
import qualified Insieme.Analysis.CallSite as CallSite
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.ExitPoint as ExitPoint
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Reference as Reference
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

--
-- * Data Flow Analysis summary
--

data DataFlowAnalysis a v i = DataFlowAnalysis {
    analysis                   :: a,                                    -- ^ the analysis type token
    analysisIdentifier         :: Solver.AnalysisIdentifier,            -- ^ the analysis identifier
    variableGenerator          :: NodeAddress -> Solver.TypedVar v,     -- ^ the variable generator of the represented analysis
    topValue                   :: v,                                    -- ^ the top value of this analysis
    iteratorVariableHandler    :: NodeAddress -> Solver.TypedVar v,     -- ^ a function creating the constraints for the given iterator variable
    freeVariableHandler        :: NodeAddress -> Solver.TypedVar v,     -- ^ a function computing the value of a free variable
    entryPointParameterHandler :: NodeAddress -> Solver.TypedVar v,     -- ^ a function computing the value of a entry point parameter
    initialValueHandler        :: NodeAddress -> v,                     -- ^ a function computing the initial value of a memory location
    initValueHandler           :: v,                                    -- ^ default value of a memory location
    excessiveFileAccessHandler :: v -> i -> v                           -- ^ a handler processing excessive field accesses (if ref_narrow calls navigate too deep)
}

-- a function creating a simple data flow analysis
mkDataFlowAnalysis :: (Typeable a, Solver.ExtLattice v) => a -> String -> (NodeAddress -> Solver.TypedVar v) -> DataFlowAnalysis a v i
mkDataFlowAnalysis a s g = res
    where
        res = DataFlowAnalysis a aid g top justTop justTop justTop (\_ -> top) top failOnAccess
        aid = (Solver.mkAnalysisIdentifier a s)
        justTop a = mkConstant res a top
        top = Solver.top
        failOnAccess _ _ = Solver.top


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

dataflowValue :: (ComposedValue.ComposedValue a i v, Typeable d)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis d a i                          -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
dataflowValue addr analysis ops = case getNode addr of

    IR.Node IR.Variable _ -> case findDecl addr of
            Just declrAddr -> handleDeclr declrAddr              -- this variable is declared, use declared value
            _              -> freeVariableHandler analysis addr  -- it is a free variable, ask the analysis what to do with it


    IR.Node IR.CallExpr _ -> var
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

        callTargetVar = Callable.callableValue (goDown 1 addr)

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
                    IR.Declaration  -> [memoryStateValue (MemoryStatePoint (ProgramPoint r Post) (MemoryLocation r)) analysis]
                    IR.CompoundStmt | isConstructorOrDestructor $ getNode $ fromJust $ getParent r -> []
                    _               -> [varGen r]

        getReturnValues a = map (Solver.get a) (getReturnValueVars a)


        -- support for calls to constructors and destructors --

        isCtorOrDtor c = isConstructorOrDestructor $ getNode $ Callable.toAddress c

        getCtorDtorVars a = if BSet.isUniverse targets then [] else concat $ go <$> Set.toList uninterceptedTargets
            where
                targets = callTargetVal a
                uninterceptedTargets = filterInterceptedLambdas targets

                go c = if isCtorOrDtor c then [varGen $ goDown 1 $ goDown 2 addr] else []

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
                dep a = [] -- covered by known targets: [Solver.toVar trg]
                val a = if hasUnknownTarget then top else Solver.bot
                    where

                        targets = callTargetVal a

                        uncoveredLiterals = filter f $ BSet.toList $ targets
                            where
                                f (Callable.Literal a) = not $ any (\h -> covers h a) extOps
                                f c | isCtorOrDtor c     = False
                                f _                    = False

                        hasUnknownTarget = (BSet.isUniverse targets) || ((not . null) uncoveredLiterals)



    IR.Node IR.TupleExpr [_,IR.Node IR.Expressions args] -> var
        where
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var

            dep _ = Solver.toVar <$> componentValueVars
            val a = ComposedValue.composeElements $ zip (component <$> [0 :: Int32 ..]) (map (Solver.get a) componentValueVars)

            componentValueVars = go <$> [0 .. ((length args) - 1) ]
                where
                    go i = varGen (goDown i $ goDown 1 $ addr)


    decl@(IR.Node IR.Declaration _) -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot

        con = Solver.forward
            (
                if Reference.isMaterializingDeclaration decl
                then memoryStateValue (MemoryStatePoint (ProgramPoint addr Post) (MemoryLocation addr)) analysis
                else varGen (goDown 1 addr)
            )
            var

    IR.Node IR.InitExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot

        con = Solver.forward (varGen (goDown 1 addr)) var


    _ -> unknown

  where

    top = topValue analysis

    idGen = mkVarIdentifier analysis

    varGen = variableGenerator analysis

    unknown = Solver.mkVariable (idGen addr) [] top

    compose = ComposedValue.toComposed
    extract = ComposedValue.toValue


    -- variable declaration handler

    handleDeclr declrAddr = case getNodeType (goUp declrAddr) of

        -- handle iterator variables
        IR.DeclarationStmt | ((depth declrAddr > 2) && ((getNodeType $ goUp $ goUp declrAddr) == IR.ForStmt))  -> var
          where
            var =  iteratorVariableHandler analysis addr

        -- handle local variables
        IR.DeclarationStmt -> var
          where
            var = Solver.mkVariable (idGen addr) [constraint] Solver.bot
            constraint = Solver.forward (varGen (goDown 0 . goUp $ declrAddr)) var

        -- handle parameters
        IR.Parameters -> case () of 

            _ | isImplicitCtorOrDtorParameter declrAddr -> iVar
              | isEntryPointParameter declrAddr         -> entryPointParameterHandler analysis declrAddr 
              | otherwise                               -> var

          where
          
            -- implicit ctor/dtor call --
            
            iVar = Solver.mkVariable (idGen addr) [iCon] Solver.bot
            iCon = Solver.forward arg iVar
            
            objDecl = getEnclosingDeclaration declrAddr 
            
            arg = case getIndex declrAddr of
                0 -> varGen $ goDown 2 $ goDown 1 objDecl  -- the this parameter of the ctor (also for the dtor) TODO: move this to e.g. the tag type address
                1 -> varGen $ goDown 1 objDecl             -- the argument of the implicit constructor call
            
          
            -- standard parameter --
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var

            n = getIndex declrAddr

            callSiteVar = CallSite.callSites (goUp $ goUp declrAddr)

            dep a = (Solver.toVar callSiteVar) : (map Solver.toVar (getArgumentVars a))
            val a = compose $ Solver.join $ map (extract . Solver.get a) (getArgumentVars a)

            getArgumentVars a = foldr go [] $ Solver.get a callSiteVar
                where
                    go = \(CallSite.CallSite call) list -> (varGen $ goDown (n+2) call) : list


        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"

    -- add support for predefined operator handlers --

    extOps = readHandler : tupleMemberAccessHandler : ops

    -- support the ref_deref operation (read)

    readHandler = OperatorHandler cov dep val
        where
            cov a = isBuiltin a "ref_deref"

            dep _ a = (Solver.toVar targetRefVar) : (map Solver.toVar $ readValueVars a)

            val _ a = if includesUnknownSources targets then top else Solver.join $ map go $ BSet.toList targets
                where
                    targets = targetRefVal a
                    
                    go r = descent dp val
                        where
                            dp  = (Reference.dataPath r)
                            val = Solver.get a (memStateVarOf r)

                    descent dp val = case () of 

                        _ | dp == root -> val

                        _ | (isNarrow dp) && (ComposedValue.isValue val) -> res
                          where
                            (i:is) = reverse $ getPath dp
                            res    = descent (narrow $ reverse is) $ excessiveFileAccessHandler analysis val i

                        _ | otherwise -> ComposedValue.getElement dp val

            targetRefVar = Reference.referenceValue $ goDown 1 $ goDown 2 addr          -- here we have to skip the potentially materializing declaration!
            targetRefVal a = ComposedValue.toValue $ Solver.get a targetRefVar

            includesUnknownSources t = BSet.member Reference.NullReference t || BSet.member Reference.UninitializedReference t

            readValueVars a = if includesUnknownSources targets then [] else map memStateVarOf $ BSet.toList targets
                where
                    targets = targetRefVal a

            memStateVarOf r = memoryStateValue (MemoryStatePoint (ProgramPoint addr Internal) (MemoryLocation $ Reference.creationPoint r)) analysis


    -- support the tuple_member_access operation (read from tuple component)

    tupleMemberAccessHandler  = OperatorHandler cov dep val
        where
            cov a = isBuiltin a "tuple_member_access"

            dep _ a = Solver.toVar indexValueVar : Solver.toVar tupleValueVar : []

            val _ a = if BSet.isUniverse indices
                    then top
                    else Solver.join $ map go dataPaths
                where

                    indices = ComposedValue.toValue (Solver.get a indexValueVar)

                    fieldIndices = index <$> (BSet.toList indices)

                    dataPaths = step <$> fieldIndices

                    tupleValue = Solver.get a tupleValueVar

                    go i = ComposedValue.getElement i tupleValue


            indexValueVar = Arithmetic.arithmeticValue $ goDown 3 addr

            tupleValueVar = varGen $ goDown 1 $ goDown 2 addr
