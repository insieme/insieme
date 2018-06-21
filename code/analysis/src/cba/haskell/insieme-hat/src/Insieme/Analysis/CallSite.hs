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
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.CallSite (
    module Insieme.Analysis.DynamicCallSites,
    callSites
) where

import qualified Data.Set as Set
import Data.Maybe
import Data.Typeable

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.DynamicCallSites
import Insieme.Analysis.Utils.CppSemantic
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.RecursiveLambdaReferences as RecLambdaRefs
import qualified Insieme.Solver as Solver
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * CallSite Analysis
--

data CallSiteAnalysis = CallSiteAnalysis
    deriving (Typeable)

callSiteAnalysis :: Solver.AnalysisIdentifier
callSiteAnalysis = Solver.mkAnalysisIdentifier CallSiteAnalysis "CS"


--
-- * CallSite Variable Generator
--

callSites :: NodeAddress -> Solver.TypedVar CallSiteSet
callSites addr = case Q.getNodeType addr of

    -- for root elements => no call sites
    _ | I.isRoot e -> noCalls

    -- for lambdas and bind exressions: collect all call sites
    I.Lambda   -> var
    I.BindExpr -> var

    -- everything else has no call sites
    _ -> noCalls

  where

    idGen = Solver.mkIdentifierFromExpression callSiteAnalysis

    noCalls = Solver.mkVariable (idGen addr) [] Solver.bot

    -- navigate to the enclosing expression
    e = case Q.getNodeType addr of
        I.Lambda   -> fromJust $ I.getParent =<< I.getParent =<< I.getParent addr
        I.BindExpr -> addr
        _           -> error "unhandled CallSite enclosing expression"

    isCall a = Q.getNodeType a == I.CallExpr


    -- the number of parameters of this callable
    numParams = I.numChildren $ I.goDown 1 addr                     -- TODO: this is wrong in case of variadic parameters!!


    -- create the variable (by binding at least the id) --
    var = Solver.mkVariable (idGen addr) cons Solver.bot

    cons    = if I.isRoot e then [initc] else [con,initc]
    initc   = Solver.constant initial var
    initial = if I.isRoot e then Solver.bot else directCall


    -- assemble the constraint computing all call sites --

    con = Solver.createConstraint dep val var
    dep a = (Solver.toVar <$> recReferencesDep) ++ (Solver.toVar <$> callSiteVars a) ++ (Solver.toVar <$> allCallsVars a)
    val a = indirectCalls a


    -- a test whether the targeted function is a lambda or a bind --
    isLambda = Q.getNodeType addr == I.Lambda

    recReferencesVar = RecLambdaRefs.recursiveCalls addr
    recReferencesVal a = if isLambda then Solver.get a recReferencesVar else Solver.bot
    recReferencesDep   = if isLambda then [recReferencesVar] else []


    -- the list of call site variables to be checked for potential calls to this lambda
    callSiteVars a = case () of
      _ | isStaticallyBound a -> []
        | isLazyArgument      -> Callable.callableValue . (I.goDown 1) <$> lazyArgumentCallSites
        | otherwise           -> Callable.callableValue . (I.goDown 1) <$> allCalls a


    ---- Handling static calls ----

    -- determines whether a given target function is statically bound in its context
    getStaticCall t = case () of
        _ | isDirectStaticCallTarget t       -> Just p1
          | isInstantiatedStaticCallTarget t -> Just p3
          | otherwise                        -> Nothing
      where

        -- it is so, if directly called
        isDirectStaticCallTarget t = I.getIndex t == 1 && isCall p1

        -- if if it is instantiated and directly called
        isInstantiatedStaticCallTarget t = I.depth t > 3 && I.getIndex t == 1 && I.getIndex p1 == 2 && I.getIndex p2 == 1 && isGenericFunctionInstantiation p2

        p1 = fromJust $ I.getParent t     -- the surrounding call or declaration
        p2 = fromJust $ I.getParent p1    -- the init call
        p3 = fromJust $ I.getParent p2    -- the actull call

    isStaticCallTarget t = isJust $ getStaticCall t

    -- determines whether all calls are statically bound (if not, we have to search) --
    isStaticallyBound a = isStaticCallTarget e && all isStaticCallTarget (RecLambdaRefs.unLRS $ recReferencesVal a)


    ---- Special handling: Lazy Operator Arguments ----

    -- determine whether this callable is the argument of a lazy operator (not included in dynamic all site list)
    isLazyArgument = isJust enclosingLazyOperator

    enclosingLazyOperator = if isLazyArgument then Just lazyOperator else Nothing
      where
        enclosingCall = fromJust $ I.getParent =<< I.getParent e
        isLazyArgument = Q.isCallOfAnyBuiltin ["bool_and","bool_or","ite"] enclosingCall
        lazyOperator = I.child 1 enclosingCall

    lazyArgumentCallSites = case enclosingLazyOperator of
        Just op -> callsIn op
        Nothing -> []
      where
        callsIn op = I.collectAllPrune isDynamicBoundCall skipTypes op
          where
            skipTypes node = if Q.isType node then I.PruneHere else I.NoPrune



    ---- Generic handling of all remaining dynamically bound callables ----

    -- add the all-calls variable dependency in case the processed function is not statically bound
    allCallsVars a =
        if isStaticallyBound a || isLazyArgument then []
        else [allCallsVar]

    -- computes a list of addresses of all potentiall call sites
    allCallsVar = dynamicCalls $ I.getRootAddress addr
    allCallsVal a = Solver.get a allCallsVar

    allCalls a = if isLazyArgument then [] else filter p $ peel <$> (Set.toList $ allCallsVal a)
      where
        p a = (I.numChildren a) == 2 + numParams
        peel (CallSite a) = a

    directCall = case getStaticCall e of
        Just p -> Set.singleton $ CallSite p
        Nothing -> Set.empty

    indirectCalls a =
        Set.fromList $ case () of
          _ | isStaticallyBound a -> CallSite . fromJust . getStaticCall <$> Set.toList (RecLambdaRefs.unLRS $ recReferencesVal a)
            | otherwise -> (CallSite <$> reachingCalls) ++ (CallSite <$> directRecursiveCalls)
      where

        reachingCalls = filter f $ (allCalls a ++ lazyArgumentCallSites)
          where
            f c = BSet.member callable $ ComposedValue.toValue $ Solver.get a $ Callable.callableValue $ I.goDown 1 c

            callable = case Q.getNodeType addr of
                I.Lambda   -> Callable.Lambda addr
                I.BindExpr -> Callable.Closure addr
                _           -> error "unexpected NodeType"

        directRecursiveCalls = fromJust . getStaticCall <$> (filter f $ Set.toList $ RecLambdaRefs.unLRS $ recReferencesVal a)
          where
            f = isStaticCallTarget
