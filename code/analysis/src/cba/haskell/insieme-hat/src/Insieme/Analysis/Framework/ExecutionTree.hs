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

module Insieme.Analysis.Framework.ExecutionTree (
    ExecutionTreeAnalysis(..),
    mkExecutionTreeAnalysis,
    executionTreeValue
) where


import Data.Maybe
import Data.Typeable

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.Solver as Solver



--
-- * Bottom-Up Execution Tree Analysis summary
--

data ExecutionTreeAnalysis v = ExecutionTreeAnalysis {
      analysisIdentifier         :: Solver.AnalysisIdentifier            -- ^ the analysis identifier
    , variableGenerator          :: NodeAddress -> Solver.TypedVar v     -- ^ the variable generator of the represented analysis
    , opHandler                  :: [OperatorHandler v]                  -- ^ a list of operator handlers handling calls to (external) functions
    , unhandledOperatorHandler   :: NodeAddress -> v                     -- ^ a handler for calls to unhandled operators
    , unknownTargetHandler       :: NodeAddress -> v                     -- ^ a handler for calls to unknown target functions
}

-- a function creating a simple execution tree analysis
mkExecutionTreeAnalysis :: (Typeable a, Solver.ExtLattice v) 
        => a 
        -> String 
        -> (NodeAddress -> Solver.TypedVar v) 
        -> ExecutionTreeAnalysis v
mkExecutionTreeAnalysis a s g = res
  where
    res = ExecutionTreeAnalysis aid g [] unhandledOperatorHandler unknownTargetHandler
    aid = Solver.mkAnalysisIdentifier a s
    justTop _ = Solver.top
    unhandledOperatorHandler = justTop
    unknownTargetHandler = justTop


--
-- * Generic bottom-up execution tree analysis
--

executionTreeValue :: (Solver.ExtLattice a)
         => ExecutionTreeAnalysis a
         -> NodeAddress
         -> Solver.TypedVar a
executionTreeValue analysis addr = case I.getNode addr of

    -- types are not executed (terminal)
    n | Q.isType n -> noExecution

    -- also literals are not executed (terminal)
    I.Node I.Literal _ -> noExecution

    -- also variables are not executed (terminal)
    I.Node I.Variable _ -> noExecution

    -- also lambda expressions are not executed (terminal)
    I.Node I.LambdaExpr _ | not (Q.isEntryPoint addr) -> noExecution

    -- for call expressions, the execution has to include the targeted function
    I.Node I.CallExpr _ -> var
      where
        -- for calls we have to aggregate the effects of the children and the targeted functions
        var = Solver.mkVariable varId cons Solver.bot
        cons = callTargetConstraint : childConstraints var

        -- this constraint computes the contributions of called functions
        callTargetConstraint = Solver.createConstraint dep val var
          where
            dep a = Solver.toVar callableVar : (localEffectVars a) ++ (Solver.toVar <$> callableBodyVars a)

            val a = Solver.join (callTargetEffects a : unknownTargetEffects a : unhandledOperatorEffects a ++ localEffects a )

            -- get access to functions targeted by this call
            callableVar = Callable.callableValue $ I.goDown 1 addr
            callableVal a = toValue $ Solver.get a callableVar


            -- Function Calls --

            callableBodyVars a = case () of
                _ | BSet.isUniverse callTargets -> []
                _                               -> foldr go [] $ BSet.toList callTargets
                  where
                    go (Callable.Lambda  addr) bs = (varGen $ I.goDown 2 addr) : bs
                    go (Callable.Closure addr) bs = (varGen $ I.goDown 2 addr) : bs
                    go (Callable.Literal   _) bs = bs      -- literal calls are handled by operator handler
              where
                callTargets = callableVal a

            -- aggregate effects of call targets
            callTargetEffects a = Solver.join $ Solver.get a <$> callableBodyVars a


            -- Unknown Call Target --

            -- add effects in case of unknown call targets
            unknownTargetEffects a = case () of
                _ | BSet.isUniverse $ callableVal a -> unknownTargetHandler analysis $ I.goDown 1 addr
                _                                   -> Solver.bot


            -- Handled Operators --

            -- collect all handled operators
            getActiveOperators a = if BSet.isUniverse targets then [] else concatMap f (opHandler analysis)
                where
                    targets = callableVal a
                    f o = mapMaybe go $ BSet.toList targets
                        where
                            go l = if covers o trg then Just (o,trg) else Nothing
                                where
                                    trg = Callable.toAddress l

            -- get list of variables local effects are depending on
            localEffectVars a = concat $ map go $ getActiveOperators a
                where
                    go (o,t) = dependsOn o t a

            -- compute local effects due to active operator handlers
            localEffects a = map go $ getActiveOperators a
                where
                    go (o,t) = getValue o t a



            -- Unhandled operators --

            -- collect all unhandled operators
            getUnhandledOperators a = if BSet.isUniverse targets then [] else Callable.toAddress <$> (filter f $ BSet.toList targets)
                where
                    targets = callableVal a
                    f t = case t of 
                        Callable.Literal addr -> any cover $ opHandler analysis
                              where 
                                cover h = covers h addr
                        _                     -> False

            unhandledOperatorEffects a = if null ops then [] else effects
              where
                ops = getUnhandledOperators a
                effects = (unhandledOperatorHandler analysis) <$> ops


    -- for everything else we aggregate the results of the execution of the child nodes
    _ -> var
      where
        var = Solver.mkVariable varId (childConstraints var) Solver.bot

  where
  
    -- some common tools
    idGen = Solver.mkIdentifierFromExpression $ analysisIdentifier analysis
    varGen = variableGenerator analysis

    varId = idGen addr

    -- a variable being empty, indicating a branch not being executed
    noExecution = Solver.mkVariable varId [] Solver.bot

    -- the standard-child-aggregation constraints
    childConstraints var = map go $ I.children addr
      where
        go addr = Solver.forward (varGen addr) var

