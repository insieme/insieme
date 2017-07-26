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

module Insieme.Analysis.CallSite where

import Control.DeepSeq
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Visit
import qualified Data.Set as Set
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.RecursiveLambdaReferences as RecLambdaRefs
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * CallSite Results
--

newtype CallSite = CallSite NodeAddress
 deriving (Eq, Ord, Generic, NFData)

instance Show CallSite where
    show (CallSite na) = "Call@" ++ (prettyShow na)

--
-- * CallSite Lattice
--

type CallSiteSet = Set.Set CallSite

instance Solver.Lattice CallSiteSet where
    bot   = Set.empty
    merge = Set.union


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
callSites addr = case getNodeType addr of

    -- for root elements => no call sites
    _ | isRoot e -> noCalls

    -- for lambdas and bind exressions: collect all call sites
    IR.Lambda   -> var
    IR.BindExpr -> var

    -- everything else has no call sites
    _ -> noCalls

  where

    idGen = Solver.mkIdentifierFromExpression callSiteAnalysis

    noCalls = Solver.mkVariable (idGen addr) [] Solver.bot

    -- navigate to the enclosing expression
    e = case getNodeType addr of
        IR.Lambda   -> fromJust $ getParent =<< getParent =<< getParent addr
        IR.BindExpr -> addr
        _           -> error "unhandled CallSite enclosing expression"

    i = getIndex e
    p = fromJust $ getParent e

    isCall a = getNodeType a == IR.CallExpr


    -- the number of parameters of this callable
    numParams = numChildren $ goDown 1 addr                     -- TODO: this is wrong in case of variadic parameters!!


    -- create the variable (by binding at least the id) --
    var = Solver.mkVariable (idGen addr) cons initial

    cons    = if isRoot addr then [] else [con]
    initial = if isRoot addr then Solver.bot else directCall


    -- assemble the constraint computing all call sites --

    con = Solver.createConstraint dep val var
    dep a = (Solver.toVar <$> recReferencesDep) ++ (Solver.toVar <$> callSiteVars a)
    val a = indirectCalls a


    -- a test whether the targeted function is a lambda or a bind --
    isLambda = getNodeType addr == IR.Lambda

    recReferencesVar = RecLambdaRefs.recursiveCalls addr
    recReferencesVal a = if isLambda then Solver.get a recReferencesVar else Set.empty
    recReferencesDep   = if isLambda then [recReferencesVar] else []


    -- determines whether all calls are statically bound (if not, we have to search) --
    isStaticallyBound a = i == 1 && isCall p && all check (recReferencesVal a)
      where
        check x = getIndex x == 1 && (isCall $ fromJust $ getParent x)


    -- the list of call site variables to be checked for potential calls to this lambda
    callSiteVars a =
        if isStaticallyBound a then []
        else Callable.callableValue . (goDown 1) <$> allCalls


    -- computes a list of addresses of all potentiall call sites
    allCalls = collectAll callable (getRootAddress addr)
      where
        callable node = case IR.getNodeType node of
            IR.CallExpr | IR.numChildren node == 2 + numParams ->
                case IR.getNodeType $ IR.getChildren node !! 1 of
                    IR.Lambda          -> False
                    IR.BindExpr        -> False
                    IR.Literal         -> False
                    IR.LambdaReference -> False
                    _                  -> True
            _ -> False


    directCall = if i == 1 && isCall p then Set.singleton $ CallSite p else Set.empty

    indirectCalls a =
        Set.fromList $ if isStaticallyBound a
            then CallSite . fromJust . getParent <$> Set.toList (recReferencesVal a)
            else (CallSite <$> reachingCalls) ++ (CallSite <$> directRecursiveCalls)
      where

        reachingCalls = filter f $ allCalls
          where
            f c = BSet.member callable $ ComposedValue.toValue $ Solver.get a $ Callable.callableValue $ goDown 1 c

            callable = case getNodeType addr of
                IR.Lambda   -> Callable.Lambda addr
                IR.BindExpr -> Callable.Closure addr
                _           -> error "unexpected NodeType"

        directRecursiveCalls = (fromJust . getParent) <$> (filter f $ Set.toList $ recReferencesVal a)
          where
            f r = getIndex r == 1 && isCall (fromJust $ getParent r)
