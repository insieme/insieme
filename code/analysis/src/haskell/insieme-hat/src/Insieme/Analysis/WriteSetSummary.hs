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
{-# LANGUAGE FlexibleContexts #-}

module Insieme.Analysis.WriteSetSummary (

    WriteSet,
    null,
    isUnknown,

    toAccessPaths,
    parameters,

    writeSetSummary

) where

import Prelude hiding (null)

import Debug.Trace
import Data.Typeable
import Insieme.Inspire.NodeAddress
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.AccessPath
import Insieme.Analysis.FreeLambdaReferences

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Insieme.Analysis.Entities.AccessPath as AP
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.Utils as IRUtils
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

import qualified Insieme.Utils.BoundSet as BSet
import qualified Insieme.Utils.UnboundSet as USet

import Insieme.Analysis.Callable


import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree


--
-- * WriteSet Lattice
--

data WriteSet i =
          Known (Map.Map AP.BaseVar (AccessPathSet i))
        | Unknown
    deriving(Eq,Ord,Show)


-- an empty write set value
empty :: WriteSet i
empty = Known Map.empty

null :: WriteSet i -> Bool
null (Known m) = Map.null m
null _ = False

isUnknown :: WriteSet i -> Bool
isUnknown Unknown = True
isUnknown _       = False

-- a union operation of write sets
union :: (FieldIndex i) => WriteSet i -> WriteSet i -> WriteSet i
union Unknown _ = Unknown
union _ Unknown = Unknown
union (Known a) (Known b) = Known (Map.unionWith BSet.union a b)

fromAccessPath :: AP.AccessPath i -> WriteSet i
fromAccessPath AP.Local              = empty
fromAccessPath AP.Unknown            = Unknown
fromAccessPath p@(AP.AccessPath v _) = Known (Map.singleton v (BSet.singleton p))

fromAccessPaths :: (FieldIndex i) => [AP.AccessPath i] -> WriteSet i
fromAccessPaths xs = foldr union empty (fromAccessPath <$> xs)

toAccessPaths :: WriteSet i -> [AP.AccessPath i]
toAccessPaths (Known s) = foldr (++) [] (BSet.toList . snd <$> Map.toList s)

parameters :: WriteSet i -> [Int]
parameters Unknown = []
parameters (Known b) = toInt <$> filter params (Map.keys b)
    where
        params (AP.Parameter _) = True
        params _ = False

        toInt (AP.Parameter x) = x


bindAccessPaths :: (FieldIndex i) => (Map.Map Int (AccessPathSet i)) -> WriteSet i -> WriteSet i
bindAccessPaths _ Unknown = Unknown
bindAccessPaths a b = fromAccessPaths $ concat (bind <$> toAccessPaths b)
    where
        bind p@(AP.AccessPath (AP.Global _) _)    = [p]
        bind p@(AP.AccessPath (AP.Parameter i) _) = case Map.lookup i a of
            Just s -> if BSet.isUniverse s then [AP.Unknown] else (\x -> AP.extend x p) <$> BSet.toList s
            Nothing -> error "Needed parameter not provided!"


-- make write sets valid lattices
instance (FieldIndex i) => Solver.Lattice (WriteSet i) where
    bot = empty
    merge = union


--
-- * WriteSet Analysis
--

data WriteSetAnalysis = WriteSetAnalysis
    deriving (Typeable)

writeSetAnalysis :: Solver.AnalysisIdentifier
writeSetAnalysis = Solver.mkAnalysisIdentifier WriteSetAnalysis "WS"


--
-- * WriteSet Variable Generator
--

writeSetSummary :: (FieldIndex i) => NodeAddress -> Solver.TypedVar (WriteSet i)
writeSetSummary addr = case getNodeType addr of

        -- ref_assign writes to location addressed by first argument
        IR.Literal | isRoot addr && (isBuiltin addr $ getBuiltin addr "ref_assign") -> var
            where
                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.createConstraint dep val var

                dep _ = []
                val a = fromAccessPath $ AP.parameter 0


        -- all other literals => no write sets
        IR.Literal | isRoot addr -> nothing


        -- for lambdas, all call sites of the body have to be aggregated
        IR.Lambda | isContextFree addr -> var
            where
                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.createConstraint dep val var

                dep a = Solver.toVar <$> writeSetVars
                val a = Solver.join $ (Solver.get a <$> writeSetVars)
                

        -- for non context-free lambdas, it has to be tested wether they are closed
        IR.Lambda -> var
            where
                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.createEqualityConstraint dep val var
                
                dep a = Solver.toVar hasFreeLambdaRefsVar : if isClosed a then closedDep a else openDep a 
                val a = if isClosed a then closedVal a else openVal a 
                
                hasFreeLambdaRefsVar = freeLambdaReferences $ goUp $ goUp $ goUp addr 
                
                isClosed a = depth addr >= 3 && (Set.null $ Solver.get a hasFreeLambdaRefsVar)
                
                -- the closed case --
                
                closedWriteSetVar = writeSetSummary contextFreeAddr
                closedDep _ = [Solver.toVar closedWriteSetVar]
                closedVal a = Solver.get a closedWriteSetVar 
                
                -- the non closed case --
                
                openDep _ = Solver.toVar <$> writeSetVars
                openVal a = Solver.join $ (Solver.get a <$> writeSetVars)

        -- compute write sets for calls
        IR.CallExpr -> var
            where

                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.createConstraint dep val var

                dep a = (Solver.toVar callTargetVar)
                            : (Solver.toVar <$> ((writeSetVars a) `asTypeOf` [var]))
                            ++ (Solver.toVar . snd <$> argVars a )

                val a = aggregateWriteSets a

                -- the variable describing the call target
                callTargetVar = callableValue $ goDown 1 addr

                -- a test whether there are unknown call targets
                hasUniversalTarget a = USet.isUniverse $ ComposedValue.toValue $ Solver.get a callTargetVar

                -- the variables for the write sets of callables
                writeSetVars a = if hasUniversalTarget a then [] else list
                    where
                        trgs = filter (`isChildOf` addr) $ toAddress <$> (USet.toList $ ComposedValue.toValue $ Solver.get a callTargetVar)
                        list = writeSetSummary <$> trgs

                -- a test to see whether there are none unknown write sets
                hasUnknownWriteSet a = hasUniversalTarget a || (any isUnknown $ (Solver.get a) <$> (writeSetVars a `asTypeOf` [var]))

                -- a function to retrieve the list of needed access path arguments
                getNeededArguments a = if hasUnknownWriteSet a then [] else list
                    where
                        list = Set.toList $ Set.fromList $ args
                        args = concat $ (go <$> (writeSetVars a `asTypeOf` [var]))
                        go v = parameters $ Solver.get a v

                -- argument variables
                argVars a = if hasUnknownWriteSet a then [] else list
                    where
                        list = go <$> getNeededArguments a
                        go i = (i,accessPathValue $ goDown (i+2) contextFreeAddr)

                -- aggregate write set of all potential target functions
                aggregateWriteSets a = if hasUnknownWriteSet a then Unknown else res
                    where
                        args = Map.fromList $ (go <$> argVars a)
                            where
                                go (i,v) = (i,ComposedValue.toValue $ Solver.get a v)

                        ws = Solver.get a <$> (writeSetVars a `asTypeOf` [var])
                        res = Solver.join $ bindAccessPaths args <$> ws


        _ | isRoot addr -> everything


        _ -> var
            where
                var = Solver.mkVariable (idGen addr) [con] Solver.bot
                con = Solver.forward (writeSetSummary contextFreeAddr) var

  where

    contextFreeAddr = dropContext addr
    isContextFree a = contextFreeAddr == a

    nothing    = Solver.mkVariable (idGen addr) [] empty
    everything = Solver.mkVariable (idGen addr) [] Unknown

    idGen a = Solver.mkIdentifier writeSetAnalysis a ""
    
    -- get list of calls within current node --
    calls = IRUtils.foldAddressPrune collect filter addr
        where

            filter cur = case getNodePair cur of
                IR.NT IR.Lambda _ -> cur /= addr
                IR.NT n         _ -> IRUtils.isType n

            collect cur l = case getNodePair cur of
                IR.NT IR.CallExpr _ -> cur : l
                _                  -> l


    -- get list of write sets at calls
    writeSetVars = writeSetSummary <$> calls




dropContext :: NodeAddress -> NodeAddress
dropContext addr = case getNodeType addr of

        IR.Literal -> crop addr

        IR.LambdaDefinition -> crop addr

        _ | isRoot addr -> addr

        _ -> goDown (getIndex addr) $ dropContext $ goUp addr
