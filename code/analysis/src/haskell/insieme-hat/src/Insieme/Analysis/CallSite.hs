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


    -- distinguish binding case --
    var =
        if isRoot addr then noCallSitesVar
        else if isStaticallyBound then staticCallSiteVar
        else allCallSitesVar

    noCallSitesVar = Solver.mkVariable id [] Solver.bot

    recReferences = case getNodeType addr of
        IR.BindExpr -> []          -- bind can not be recursive
        IR.Lambda   -> if isRoot p then [] else res
            where
                ref = getNodePair $ goDown 0 $ fromJust $ getParent addr
                def = fromJust $ getParent $ fromJust $ getParent addr
                res = foldAddressPrune agg filter def

                agg n l = case getNodeType n of
                    IR.LambdaReference | isUse -> n : l --- TODO: filter out refs in bindings
                        where
                            isUse = getNodePair n == ref && (getNodeType . fromJust . getParent) n /= IR.LambdaBinding
                    _                                            -> l

                filter n = isType (getNodeType n) || (IR.LambdaExpr == (getNodeType n))      -- TODO: support for lambda references exceeding local scope


    isStaticallyBound = i == 1 && isCall p && all check recReferences
        where
            check a = getIndex a == 1 && (isCall $ fromJust $ getParent a)

    staticCallSiteVar = Solver.mkVariable id [] (res)
        where
            res = Set.fromList $ CallSite p : recCalls
            recCalls = (CallSite . fromJust . getParent) <$> recReferences

    allCallSitesVar = Solver.mkVariable id [con] Solver.bot
        where
            con = Solver.createConstraint dep val var

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

            allTrgVars = map (\c -> (c , Callable.callableValue $ goDown 1 c ) ) allCalls

            callable = case getNodeType addr of
                IR.Lambda   -> Callable.Lambda addr
                IR.BindExpr -> Callable.Closure addr
                _           -> error "unexpected NodeType"

            dep a = map Solver.toVar (map snd allTrgVars)
            val a = foldr go Set.empty allTrgVars
                where
                    go = \(call,var) set ->
                        if (USet.member callable (ComposedValue.toValue $ Solver.get a var))
                        then Set.insert (CallSite call) set
                        else set
