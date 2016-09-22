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

module Insieme.Analysis.CallSite where

import Debug.Trace
import Data.Maybe
import Data.Typeable
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
import qualified Data.Set as Set
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.RecursiveLambdaReferences as RecLambdaRefs
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.UnboundSet as USet


import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * CallSite Results
--

newtype CallSite = CallSite NodeAddress
 deriving (Eq, Ord)

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

    -- for lambdas and bind exressions: collect all call sites
    IR.Lambda   -> var
    IR.BindExpr -> var

    -- everything else has no call sites
    _ -> Solver.mkVariable id [] Solver.bot

  where

    id = Solver.mkIdentifierFromExpression callSiteAnalysis addr

    -- navigate to the enclosing expression
    e = case getNodeType addr of
        IR.Lambda   -> fromJust $ getParent =<< getParent =<< getParent addr
        IR.BindExpr -> addr

    i = getIndex e
    p = fromJust $ getParent e

    isCall a = getNodeType a == IR.CallExpr


    -- the number of parameters of this callable
    numParams = numChildren $ goDown 1 addr                     -- TODO: this is wrong in case of variadic parameters!!


    -- create the variable (by binding at least the id) --
    var = Solver.mkVariable id cons init
    
    cons = if isRoot addr then [] else [con]
    init = if isRoot addr then Solver.bot else directCall


    -- assemble the constraint computing all call sites --
    
    con = Solver.createConstraint dep val var
    dep a = (Solver.toVar <$> recReferencesDep) ++ (Solver.toVar <$> callSiteVars a)
    val a = indirectCalls a 
    

    
    isLambda = getNodeType addr == IR.Lambda
    
    recReferencesVar = RecLambdaRefs.recursiveCalls addr
    recReferencesVal a = if isLambda then Solver.get a recReferencesVar else Set.empty
    recReferencesDep   = if isLambda then [recReferencesVar] else []  


    -- determines whether all calls are statically bound (if not, we have to search) --
    isStaticallyBound a = i == 1 && isCall p && all check (recReferencesVal a)
      where
        check a = getIndex a == 1 && (isCall $ fromJust $ getParent a)

    
    -- the list of call site variables to be checked for potential calls to this lambda
    callSiteVars a = 
        if isStaticallyBound a then [] 
        else Callable.callableValue . (goDown 1) <$> allCalls  


    -- computes a list of addresses of all potentiall call sites 
    allCalls = collectAll callable (getRootAddress addr)
      where
        callable node = case IR.getNodeType node of
            IR.CallExpr | IR.numChildren node == numParams + 2 ->
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
        else CallSite <$> reachingCalls
      where
        reachingCalls = filter f $ allCalls
        f c = USet.member callable $ ComposedValue.toValue $ Solver.get a $ Callable.callableValue $ goDown 1 c

        callable = case getNodeType addr of
            IR.Lambda   -> Callable.Lambda addr
            IR.BindExpr -> Callable.Closure addr
            _           -> error "unexpected NodeType"

