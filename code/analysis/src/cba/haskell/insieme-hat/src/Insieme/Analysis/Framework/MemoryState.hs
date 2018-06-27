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
{-# LANGUAGE ScopedTypeVariables #-}

module Insieme.Analysis.Framework.MemoryState (

    MemoryLocation(..),       -- re-exported for convinience
    MemoryStatePoint(..),
    memoryStateValue,

    referencedValue

) where

import Control.DeepSeq (NFData)
import Data.Maybe
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.Memory
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.ProgramPoint
import Insieme.Analysis.Utils.CppSemantic
import Insieme.Analysis.Reference

import qualified Data.Set as Set

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

import qualified Insieme.Analysis.Entities.AccessPath as AP
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Solver as Solver
import qualified Insieme.Analysis.WriteSetSummary as WS

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow




-- define the lattice of definitions

data Definition = Initial                                    -- it is the definiton obtained at program startup
              | Creation                                     -- it is the creation point definition (e.g. ref_alloc)
              | Declaration NodeAddress                      -- the definition is conducted by an assignment triggered through a materializing declaration
              | Constructor NodeAddress                      -- the definition is conducted by a constructor call triggered at the given position
              | MaterializingCall NodeAddress                -- the definition is conducted by an assignment triggered through a materializing call
              | Assignment NodeAddress                       -- an assignment conducting an update to a memory location
              | Initialization NodeAddress                   -- an initialization expression conducting an update to a memory location
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

newtype Definitions = Definitions { unD :: BSet.UnboundSet Definition }
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Solver.Lattice Definitions where
    bot = Definitions BSet.empty
    (Definitions x) `merge` (Definitions y) = Definitions $ BSet.union x y


instance Solver.ExtLattice Definitions where
    top = Definitions BSet.Universe

--
-- * Memory State Analysis
--

data MemoryStateAnalysis a = MemoryStateAnalysis a
    deriving (Typeable)

memoryStateAnalysis :: (Typeable a, Typeable v, Typeable i) => DataFlowAnalysis a v i -> Solver.AnalysisIdentifier
memoryStateAnalysis a = Solver.mkAnalysisIdentifier (MemoryStateAnalysis a) ('M' : (show $ analysisIdentifier a) )


--
-- * Memory State Variable Generator
--

{-# INLINE memoryStateValue #-}
memoryStateValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => MemoryStatePoint                            -- ^ the program point and memory location interested in
         -> DataFlowAnalysis d v i                      -- ^ the underlying data flow analysis this memory state analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested state

memoryStateValue ms@(MemoryStatePoint (ProgramPoint _ _) ml@(MemoryLocation loc)) analysis = case Q.getNodeType loc of

        -- the value of the implicit this pointer of implicit ctor calls is modeled here
        _ | (not $ I.isRoot loc) && Q.getNodeType parent == I.Declaration && I.getIndex loc == 0 -> var
            where
              var = Solver.mkVariable varId [con] Solver.bot
              con = Solver.forward (variableGenerator analysis parent) var

              parent = I.goUp loc

        -- all other memory locations are handled here
        _ -> var

    where

        -- extend the underlysing analysis's identifier for the memory state identifier
        varId = Solver.mkIdentifierFromMemoryStatePoint (memoryStateAnalysis analysis) ms

        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var

        dep a = (Solver.toVar reachingDefVar) :
                   (
                     if BSet.isUniverse defs then []
                     else (map Solver.toVar $ definingValueVars a)
                   )
            where
                defs = reachingDefVal a

        val a = case () of
                _ | BSet.isUniverse defs      -> Solver.top
                  | otherwise                 -> value
            where
                value = Solver.join $ (mapMaybe go $ BSet.toList defs) ++ valuesFromDefs
                  where
                    go def = case def of
                        Initial  -> Just $ initialValueHandler analysis loc
                        Creation -> Just $ uninitializedValue analysis
                        _        -> Nothing

                valuesFromDefs = map (Solver.get a) (definingValueVars a)

                defs = reachingDefVal a


        reachingDefVar = reachingDefinitions ms
        reachingDefVal a = unD $ Solver.get a reachingDefVar

        definingValueVars a =
                BSet.applyOrDefault [] (concat . (map go) . BSet.toList) $ reachingDefVal a
            where
                go (Declaration       addr)         = [definedValue addr Pre  ml analysis]
                go (Constructor       addr)         = [definedValue addr Post ml analysis]
                go (MaterializingCall addr)         = [variableGenerator analysis $ addr]
                go (Assignment        addr)         = [definedValue addr Post ml analysis]
                go (Initialization    addr)         = [definedValue addr Post ml analysis]
                go _                                = []




--
-- * Defined Value Analysis
--

data DefinedValueAnalysis a = DefinedValueAnalysis a
    deriving (Typeable)

definedValueAnalysis :: (Typeable a, Typeable v, Typeable i) => DataFlowAnalysis a v i -> Solver.AnalysisIdentifier
definedValueAnalysis a = Solver.mkAnalysisIdentifier (DefinedValueAnalysis a) ( "DV-" ++ (show $ analysisIdentifier a) )


--
-- * Defined Value Variable Generator
--

{-# INLINE definedValue #-}
definedValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => NodeAddress                                 -- ^ the definition interrested in
         -> Phase                                       -- ^ the execution phase of this definition
         -> MemoryLocation                              -- ^ the memory location interrested in
         -> DataFlowAnalysis d v i                      -- ^ the underlying data flow analysis this defined value analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested value

definedValue addr phase ml@(MemoryLocation loc) analysis = case Q.getNodeType addr of

        -- handle declarations with implicit constructors (Pre-constructor execution)
        I.Declaration | callsImplicitConstructor addr && phase == Pre -> var
          where

            -- retrieve the implicit constructor
            Just ctor = getImplicitConstructor addr

            -- decide based on constructor what to do
            var = case Q.getNodeType ctor of
                I.LambdaExpr   -> uninitialized -- the constructor is doing the rest
                I.GenericType  -> valueSemantic  -- here we assume value semantic for all generic types
                I.TypeVariable -> valueSemantic  -- and all type variables
                t -> error $ "Unexpected ctor type: " ++ (show t)

            -- the default handling
            uninitialized = Solver.mkVariable varId [] $ uninitializedValue analysis

            -- assume that the implicit copy or move constructor is copying the stored value (analysis data is preserved)
            valueSemantic = var
              where
                -- for generic types we copy the memory state of the input parameter (assume value semantic)
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = (Solver.toVar initRefVar) : (Solver.toVar . snd <$> initMemStateVars a)
                val = initMemStateVal

                -- the variable representing a reference to the value passed as argument
                initRefVar = referenceValue $ I.goDown 1 addr
                initRefVal a = unRS $ ComposedValue.toValue $ Solver.get a initRefVar

                -- get memory state variables of passed argument
                initMemStateVars a = if BSet.isUniverse refs then [] else vars
                  where
                    refs = initRefVal a
                    refList = BSet.toList refs
                    vars = zip refList $ toStateVar <$> refList

                    toStateVar (Reference loc _) = memoryStateValue (MemoryStatePoint pp (MemoryLocation loc)) analysis
                    toStateVar r = error ("Sorry, unexpected reference value: " ++ (show r))

                    pp = ProgramPoint (I.goDown 1 addr) Post

                -- compute aggregated memory state value
                initMemStateVal a =
                    if BSet.isUniverse refs
                    then Solver.top
                    else Solver.join values
                  where
                    refs = initRefVal a

                    values = (go <$> initMemStateVars a)
                    go ((Reference _ dp),memValVar) = ComposedValue.getElement dp $ Solver.get a memValVar
                    go (r,_) = error ("Unexpected reference value: " ++ (show r))

        -- handle declarations with implicit constructors (Post-constructor execution)
        I.Declaration | callsImplicitConstructor addr && phase == Post -> case implicitCtorHandler analysis of
            Just handler -> handler addr
            Nothing -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.forward constructedStateVar var

            constructedStateVar = memoryStateValue (MemoryStatePoint (ProgramPoint addr Internal) ml) analysis

        -- handle declarations with explict constructor calls
        I.Declaration | callsExplicitConstructor addr && phase == Post-> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.forward memVar var
            memVar = memoryStateValue (MemoryStatePoint (ProgramPoint (I.goDown 1 addr) Post) ml) analysis

        -- handle declarations without implicit constructors (simple assignments)
        I.Declaration -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.forward initVar var
            initVar = variableGenerator analysis $ I.goDown 1 addr

        -- handle values defined by assignments or explicit constructor calls
        I.CallExpr -> var

        -- handle struct values defined by init expressions
        I.InitExpr | Q.isTagType elemType -> var
           where
             var = Solver.mkVariable varId [con] Solver.bot
             con = Solver.createEqualityConstraint dep val var

             dep a = valueDepsWithElemVars (Solver.toVar <$> valueVars) a
             val a = updatedValueWithValue (ComposedValue.composeElements $ zip fields (valueVal a)) a

             fields = fromJust $ map structField <$> Q.getFieldNames elemType

             valueVal a = Solver.get a <$> valueVars
             valueVars = valueVar . (I.goDown 1) <$> I.children (I.goDown 2 addr)

        -- handle array values defined by init expressions
        I.InitExpr | Q.isArray elemType -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.createEqualityConstraint dep val var

            dep a = valueDepsWithElemVars (Solver.toVar <$> valueVars) a
            val a = updatedValueWithValue (ComposedValue.composeElements $ elems a) a

            -- attach (unknownIndex, default value) if not all array elements are initialized
            elems a | numIndices == Q.getArraySize arrayType = zip indices (valueVal a)
            elems a = (unknownIndex, initialValue analysis) : zip indices (valueVal a)

            arrayType = I.goDown 0 $ I.goDown 2 $ I.goDown 0 addr

            indices = (arrayIndex . Ar.mkConst . fromIntegral) <$> [0..numIndices-1]
            numIndices = I.numChildren $ I.goDown 2 addr

            valueVars = valueVar <$> I.children (I.goDown 2 addr)
            valueVal a = Solver.get a <$> valueVars

        -- handle std::array values defined by init expressions
        I.InitExpr | Q.isStdArray elemType -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.createEqualityConstraint dep val var

            dep a = valueDepsWithElemVars (initValueVars a) a
            val a = updatedValueWithValue (convertToStdArray $ nestedArrayMemStateVal a) a

            -- auxiliary dependency collection variables
            initValueVars a = (Solver.toVar nestedArrayRefVar) : (Solver.toVar . snd <$> nestedArrayMemStateVars a)

            -- get value of nested array initialization

            nestedArrayInitAddr = I.goDown 0 $ I.goDown 2 addr

            -- the variable representing a reference to the initialized array
            nestedArrayRefVar = referenceValue nestedArrayInitAddr
            nestedArrayRefVal a = unRS $ ComposedValue.toValue $ Solver.get a nestedArrayRefVar

            -- get memory state of initialized nested array
            nestedArrayMemStateVars a = if BSet.isUniverse refs then [] else vars
              where
                refs = nestedArrayRefVal a
                refList = BSet.toList refs
                vars = zip refList $ toStateVar <$> refList

                toStateVar (Reference loc _) = memoryStateValue (MemoryStatePoint pp (MemoryLocation loc)) analysis
                toStateVar r = error ("Sorry, unexpected reference value: " ++ (show r))

                pp = ProgramPoint (I.goDown 2 addr) Post

            nestedArrayMemStateVal a = Solver.join values
              where
                values = (go <$> nestedArrayMemStateVars a)
                go ((Reference _ dp),memValVar) = ComposedValue.getElement dp $ Solver.get a memValVar
                go (r,_) = error ("Unexpected reference value: " ++ (show r))

            -- a utility to convert an array to a std::array
            convertToStdArray t = ComposedValue.mapElements toArrayIndex t
              where
                toArrayIndex e@(i,_) | i == unknownIndex = e
                toArrayIndex (i,t) = case getNumericIndex i of
                    Just n -> (stdArrayIndex n, t)
                    Nothing -> error $ "Array initialized with incompatible Index: " ++ (show i)

        -- handle scalar values defined by init expressions
        I.InitExpr -> var

        _ -> error "definedValue: unhandled case"

    where

        -- the ID of the produced variable
        varId = Solver.mkIdentifierFromMemoryStatePoint (definedValueAnalysis analysis) ms
            where
                ms = MemoryStatePoint (ProgramPoint addr phase) ml


        -- the variable for assignment and scalar init

        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createEqualityConstraint dep val var

        dep a = Solver.toVar targetRefVar : valueDeps a

        val a = case () of _ | not $ isActive a  -> old
                             | isFullAssign a    -> elemValueVal a
                             | isPerfectAssign a -> new
                             | otherwise         -> Solver.merge old new
            where
                old = Solver.get a oldStateVar
                new = updatedValue a



        -- get access to the referenced locations
        targetRefVar = referenceValue $ case Q.getNodeType addr of
            I.CallExpr -> I.goDown 1 $ I.goDown 2 addr         -- here we have to skip the potentially materializing declaration!
            I.InitExpr -> I.goDown 1 addr                    -- the reference to be initialized
            _ -> error "targetRefVar: unhandled case"
        targetRefVal a = ComposedValue.toValue $ Solver.get a targetRefVar


        -- some checks regarding the scope of the update

        isActive a = (BSet.isUniverse trgs) || (any pred trgs)
            where
                trgs = unRS $ targetRefVal a
                pred (Reference cp _) = cp == loc
                pred _ = False

        isPerfectAssign a = (not $ BSet.isUniverse trgs) && (BSet.size trgs == 1)
            where
                trgs = unRS $ targetRefVal a

        isFullAssign a = isPerfectAssign a && (all pred $ unRS $ targetRefVal a)
            where
                pred (Reference _ DP.Root) = True
                pred _ = False


        -- value analysis variable generator

        valueVar = variableGenerator analysis


        -- value composing utilities

        valueDepsWithElemVars valueVars a = (Solver.toVar targetRefVar) :
              case () of _ | not $ isActive a -> [Solver.toVar oldStateVar]
                           | isFullAssign a   -> valueVars
                           | otherwise        -> (Solver.toVar oldStateVar) :  valueVars

        valueDeps = valueDepsWithElemVars [Solver.toVar elemValueVar]

        elemValueVar = case Q.getNodeType addr of

                I.InitExpr  -> valueVar $ I.goDown 1 $ I.goDown 0 $ I.goDown 2 addr

                I.CallExpr -> case () of
                  _ | isExplicitConstructorCall addr -> referencedValue (I.goDown 1 valueDecl) analysis
                    | isMaterializingDeclaration $ I.getNode valueDecl -> memoryStateValue (MemoryStatePoint (ProgramPoint valueDecl Post) (MemoryLocation valueDecl)) analysis
                    | otherwise -> valueVar (I.goDown 1 valueDecl)

                _ -> error "elemValueVar: unhandled case"

            where
                valueDecl = I.goDown 3 addr


        elemValueVal a = Solver.get a elemValueVar

        updatedValueWithValue elemVal a =
                if BSet.isUniverse trgs then Solver.top else Solver.join values
            where
                oldVal = oldStateVal a

                trgs = unRS $ targetRefVal a

                values = concat $ update <$> BSet.toList trgs

                update (Reference cp dp) | cp == loc = [
                        ComposedValue.setElement dp elemVal oldVal
                    ]
                update _ = []

        updatedValue a = updatedValueWithValue (elemValueVal a) a

        -- access to the old state

        oldStateVar = memoryStateValue (MemoryStatePoint (ProgramPoint (I.goDown 1 addr) Post) ml) analysis
        oldStateVal a = Solver.get a oldStateVar


        -- type checks

        elemType = fromJust $ Q.getReferencedType =<< Q.getType addr





--
-- * Referenced Value Analysis
--

data ReferencedValueAnalysis a = ReferencedValueAnalysis a
    deriving (Typeable)

referencedValueAnalysis :: (Typeable a, Typeable v, Typeable i) => DataFlowAnalysis a v i -> Solver.AnalysisIdentifier
referencedValueAnalysis a = Solver.mkAnalysisIdentifier (ReferencedValueAnalysis a) ( "RV-" ++ (show $ analysisIdentifier a) )


--
-- * Referenced Value Variable Generator
--

{-# INLINE referencedValue #-}
referencedValue :: (ComposedValue.ComposedValue v i a, Typeable d)
         => NodeAddress                                 -- ^ the expression providing a reference to the value of interest
         -> DataFlowAnalysis d v i                      -- ^ the underlying data flow analysis this defined value analysis is cooperating with
         -> Solver.TypedVar v                           -- ^ the analysis variable representing the requested value

referencedValue addr analysis = case () of

    -- filter out references (fail if it is not a reference)
    _ | not (Q.isReference addr) -> error "Not a reference!"
      | otherwise -> var

  where

    -- the ID of the produced variable
    varId = Solver.mkIdentifierFromExpression (referencedValueAnalysis analysis) addr

    -- the variable obtaining the value of a referenced memory location

    var = Solver.mkVariable varId [con] Solver.bot
    con = Solver.createConstraint dep val var

    dep a = (Solver.toVar targetRefVar) : (map Solver.toVar $ readValueVars a)

    val a =
            if BSet.isUniverse targets
            then Solver.top
            else Solver.join $ map go $ BSet.toList targets
        where
            targets = unRS $ targetRefVal a

            go r = case r of
                NullReference          -> Solver.top
                UninitializedReference -> uninitializedValue analysis
                _                      -> descent dp val
              where
                dp  = dataPath r
                val = Solver.get a (fromJust $ memStateVarOf r)

            descent dp val = case dp of

                DP.Root -> val

                DP.DataPath p DP.Down i | ComposedValue.isValue val -> res
                  where
                    res = excessiveFileAccessHandler analysis (descent p val) i

                _ | otherwise -> ComposedValue.getElement dp val


    targetRefVar = referenceValue addr
    targetRefVal a = ComposedValue.toValue $ Solver.get a targetRefVar

    readValueVars a =
            if BSet.isUniverse targets
            then []
            else mapMaybe memStateVarOf $ BSet.toList targets
        where
            targets = unRS $ targetRefVal a

    memStateVarOf NullReference = Nothing
    memStateVarOf UninitializedReference = Nothing
    memStateVarOf r = Just $ memoryStateValue (MemoryStatePoint (ProgramPoint addr Post) (MemoryLocation $ creationPoint r)) analysis




--
-- * Reaching Definition Analysis
--

data ReachingDefinitionAnalysis = ReachingDefinitionAnalysis
    deriving (Typeable)

reachingDefinitionAnalysis :: Solver.AnalysisIdentifier
reachingDefinitionAnalysis = Solver.mkAnalysisIdentifier ReachingDefinitionAnalysis "RD"


--
-- * Reaching Definition Variable Generator
--

{-# INLINE reachingDefinitions #-}
reachingDefinitions :: MemoryStatePoint -> Solver.TypedVar Definitions
reachingDefinitions (MemoryStatePoint pp@(ProgramPoint addr p) ml@(MemoryLocation loc)) = case Q.getNodeType addr of

        -- a declaration could be an assignment if it is materializing
        I.Declaration | p == Pre && addr == loc ->
            Solver.mkVariable varId [] $ Definitions $ BSet.singleton $ Declaration addr

        -- at the declaration, potential initial definitions can be filtered out (artifact of recursive functions)
        I.Declaration | p == Internal && addr == loc -> var
          where
            var = Solver.mkVariable varId [con] Solver.bot
            con = Solver.createConstraint dep val var

            dep a = Solver.getDependencies a defaultVar
            val a = Definitions $ BSet.delete Initial $ unD $ Solver.getLimit a defaultVar

        -- an implicit constructor call can be a definition
        I.Declaration | p == Post && addr == loc && callsImplicitConstructor addr ->
            Solver.mkVariable varId [] $ Definitions $ BSet.singleton $ Constructor addr

        -- a call could be an assignment if it is materializing
        I.CallExpr | p == Post && addr == loc && isMaterializingCall (I.getNode addr) ->
            Solver.mkVariable varId [] $ Definitions $ BSet.singleton $ MaterializingCall addr

        -- the call could also be the creation point if it is not materializing
        I.CallExpr | p == Post && addr == loc ->
            Solver.mkVariable varId [] $ Definitions $ BSet.singleton Creation

        -- the entry point is the creation of everything that reaches this point
        _ | p == Pre && Q.isEntryPoint addr ->
            Solver.mkVariable varId [] $ Definitions $ BSet.singleton Initial

        -- skip everything that can statically be considered assignment free
        _ | p == Post && isAssignmentFree (I.getNode addr) && (not isParentOfLocation) -> var
            where
              var = Solver.mkVariable varId [con] Solver.bot
              con = Solver.forward src var
              src = reachingDefinitions $ MemoryStatePoint (ProgramPoint addr Pre) ml

        -- an assignment might be a definition point
        I.CallExpr | p == Post && isAssignCandidate -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = (if isAssign then [] else [Solver.toVar funVar]) ++ case () of
                        _ | not isAssign && noFuns a -> []                -- wait until there are some target functions (optimization)
                          | mayAssign a -> refs : case () of
                                _ | noRefs a  -> []             -- wait until there are some references (optimization)
                                  | hitsLoc a -> []             -- it is a hit, we don't need the predecessor
                                  | otherwise -> predVars       -- it is no full hit, we might need the predecessor
                          | otherwise   -> predVars
                    where
                        predVars = predecessorVars a
                        refs = Solver.toVar refVar


                val a = case () of
                    _ | mayAssign a -> case () of
                            _ | noRefs a  -> Solver.bot
                              | hitsLoc a -> Definitions $ BSet.singleton $ Assignment addr
                              | otherwise -> predVals
                      | otherwise   -> predVals
                    where
                        predVals = predecessorVal a

                -- optimize for the frequent case of static bound ref-assing operations
                isAssign = Q.isBuiltin (I.goDown 1 addr) "ref_assign"

                funVar = callableValue $ I.goDown 1 addr
                funVal a = ComposedValue.toValue $ Solver.get a funVar

                noFuns a = BSet.null $ funVal a

                mayAssign a = isAssign || BSet.isUniverse funs || (any assign funs)
                    where
                        funs = funVal a
                        assign c = isRefAssign || isCtor
                          where
                            isRefAssign = Q.isBuiltin addr "ref_assign"
                            isCtor = Q.isLiteral addr && Q.isCopyOrMoveConstructor addr
                            addr = toAddress c


        -- skip the interpretation of known effect-free builtins
        I.CallExpr | p == Internal && (isAssignmentFreeFunction $ I.node $ I.goDown 1 addr) && (not isParentOfLocation) -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.forward pre var
                pre = reachingDefinitions $ MemoryStatePoint (ProgramPoint (I.goDown 1 addr) Post) ml


        -- TODO: this optimization has been disabled due to invalid write set summaries
        --       re-enable once fixed

        -- to prune the set of variables, we check whether the invoced callable may update the traced reference
        I.CallExpr | False && p == Internal && (not isParentOfLocation)-> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = (Solver.toVar writeSetVar) :
                    if canSkipCallable a then [Solver.toVar skipPredecessorVar] else nonSkipPredecessorVars a

                val a = if canSkipCallable a then skipPredecessorVal a else nonSkipPredecessorVal a

                writeSetVar = writeSet addr
                writeSetVal a = unWS $ Solver.get a writeSetVar

                canSkipCallable = not . BSet.member loc . writeSetVal

                -- utils for skip case --

                skipPredecessorVar = reachingDefinitions $ MemoryStatePoint (ProgramPoint (I.goDown 1 addr) Post) ml
                skipPredecessorVal a = Solver.get a skipPredecessorVar

                -- utils for non-skip case --

                nonSkipPredecessorVars a = Solver.getDependencies a defaultVar
                nonSkipPredecessorVal a = Solver.getLimit a defaultVar


        -- init expressions may alter memory states as well
        I.InitExpr | p == Post -> var
            where
                var = Solver.mkVariable varId [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var

                dep a = Solver.toVar refVar : case () of
                        _ | noRefs a  -> []
                          | hitsLoc a -> []
                          | otherwise -> predecessorVars a

                val a = case () of
                     _ | noRefs a  -> Solver.bot
                       | hitsLoc a -> Definitions $ BSet.singleton $ Initialization addr
                       | otherwise -> predecessorVal a

        -- for all the others, the magic is covered by the generic program point value constraint generator
        _ -> defaultVar

    where

        isParentOfLocation = loc `I.isChildOf` addr

        analysis pp = reachingDefinitions (MemoryStatePoint pp ml)

        idGen pp = Solver.mkIdentifierFromMemoryStatePoint reachingDefinitionAnalysis (MemoryStatePoint pp ml)
        varId = idGen pp

        defaultVar = programPointValue pp idGen analysis []

        isAssignCandidate = mayBeAssign || isExplicitCopyOrMoveConstructorCall addr
            where
                fun = I.goDown 1 addr
                mayBeAssign = Q.isBuiltin fun "ref_assign" || (I.numChildren addr == 4 && unitRes && refParam && isDynamicBoundCall addr)
                unitRes  = Q.isUnit $ I.getNode addr
                refParam = Q.isReference $ I.getNode $ I.goDown 1 $ I.goDown 2 addr

        -- target location utils --

        refVar = referenceValue $ case Q.getNodeType addr of
            I.CallExpr -> I.goDown 1 $ I.goDown 2 addr             -- first argument
            I.InitExpr -> I.goDown 1 addr                        -- target memory location
            _ -> error "refVar: unhandled case"

        refVal :: Solver.AssignmentView -> BSet.UnboundSet (Reference SimpleFieldIndex)
        refVal a = unRS $ ComposedValue.toValue $ Solver.get a refVar

        noRefs a = BSet.null $ refVal a

        hitsLoc a = BSet.isUniverse refs || (any hit refs)
            where
                refs = refVal a
                hit (Reference cp _) = cp == loc
                hit _ = False

        predecessorVars a = Solver.getDependencies a defaultVar
        predecessorVal  a = Solver.getLimit a defaultVar


-- A filter for the reaching definition analysis --

isAssignmentFree :: I.Tree -> Bool
isAssignmentFree tree = case Q.getNodeType tree of

        I.Literal     -> True

        I.Variable    -> True

        I.CallExpr    -> isAssignmentFreeFunction (I.child 1 tree) &&
                            all isAssignmentFree (tail $ I.getChildren tree)

        I.LambdaExpr  -> True

        I.BindExpr    -> True

        I.Declaration -> not (callsImplicitConstructor tree) && (isAssignmentFree $ I.child 1 tree)

        _ -> False



isAssignmentFreeFunction :: I.Tree -> Bool
isAssignmentFreeFunction tree = case Q.getNodeType tree of

    I.Literal -> Q.isaBuiltin tree && not (Q.isBuiltin tree "ref_assign")

    I.LambdaExpr -> Q.isAnyOfBuiltin s tree
      where
        s= Set.fromList [
                                "bool_and",
                                "bool_not",
                                "bool_or",
                                "bool_to_int",
                                "enum_from_int",
                                "enum_to_int",
                                "id",
                                "ite",
                                "num_cast",
                                "ptr_cast",
                                "ptr_deref",
                                "ptr_diff",
                                "ptr_eq",
                                "ptr_from_array",
                                "ptr_from_ref",
                                "ptr_ge",
                                "ptr_gt",
                                "ptr_le",
                                "ptr_lt",
                                "ptr_ne",
                                "ptr_null",
                                "ptr_of_function",
                                "ptr_parent_cast",
                                "ptr_reinterpret",
                                "ptr_subscript",
                                "ptr_to_array",
                                "ptr_to_ref",
                                "ptr_const_cast",
                                "ptr_add",
                                "ptr_sub",
                                "ref_array_element",
                                "ref_kind_cast",
                                "ref_const_cast",
                                "ref_component_access",
                                "ref_member_access",
                                "ref_new",
                                "ref_new_init",
                                "ref_of_function",
                                "ref_parent_cast",
                                "ref_scalar_to_ref_array",
                                "ref_temp",
                                "ref_temp_init",
                                "unit_consume"
                        ]

    _ -> False



--
-- * Write Set Analysis
--

data WriteSetAnalysis = WriteSetAnalysis
    deriving (Typeable)

writeSetAnalysis :: Solver.AnalysisIdentifier
writeSetAnalysis = Solver.mkAnalysisIdentifier WriteSetAnalysis "WriteSet"

newtype WriteSet = WriteSet { unWS :: BSet.UnboundSet Location }
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Solver.Lattice WriteSet where
    bot = WriteSet BSet.empty
    (WriteSet x) `merge` (WriteSet y) = WriteSet $ BSet.union x y


-- an analysis computing the set of memory locations written by a expression
writeSet :: NodeAddress -> Solver.TypedVar WriteSet
writeSet addr = case Q.getNodeType addr of

        I.CallExpr -> var
            where
                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.createConstraint dep val var

                dep a = (Solver.toVar targetVar) : (Solver.toVar <$> writeSetSummaryVars a) ++ (Solver.toVar <$> refVars a)
                val a = WriteSet $ if unknown then BSet.Universe else res
                    where
                        wss = writeSetSummaryVal a
                        aps = WS.toAccessPaths wss

                        unknown = (WS.isUnknown wss) || (any infinite $ refVars a) || (any tooLong aps)
                            where
                                infinite r = BSet.isUniverse $ unRS $ ComposedValue.toValue $ Solver.get a r
                                tooLong (AP.AccessPath _ d) = length d > 1
                                tooLong _ = error "tooLong: unhandled case"

                        res = foldr go BSet.empty (WS.parameters wss)
                            where
                                go x s = BSet.union s $ BSet.fromList $ concat $ map toLoc $ BSet.toList $ unRS $ ComposedValue.toValue $ Solver.get a $ refVar x
                                toLoc :: Reference SimpleFieldIndex -> [Location]
                                toLoc (Reference l _ ) = [l]
                                toLoc _ = []

                refVar :: Int -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
                refVar x = referenceValue $ I.goDown 1 $ I.goDown (x+2) addr

                targetVar = callableValue $ I.goDown 1 addr
                targetVal a = ComposedValue.toValue $ Solver.get a targetVar

                writeSetSummaryVars :: Solver.AssignmentView -> [Solver.TypedVar (WS.WriteSet SimpleFieldIndex)]
                writeSetSummaryVars a = if BSet.isUniverse trgs then [] else list
                    where
                        trgs = targetVal a
                        list = go <$> BSet.toList trgs
                        go = WS.writeSetSummary . toAddress

                writeSetSummaryVal a = Solver.join $ (Solver.get a) <$> writeSetSummaryVars a

                refVars a = if WS.isUnknown wss then [] else res
                     where
                        wss = writeSetSummaryVal a
                        res = refVar <$> WS.parameters wss

        _ -> Solver.mkVariable (idGen addr) [] Solver.bot
    where

        idGen a = Solver.mkIdentifierFromExpression writeSetAnalysis a
