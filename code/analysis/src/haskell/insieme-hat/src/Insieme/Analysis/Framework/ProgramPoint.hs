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

module Insieme.Analysis.Framework.ProgramPoint where

import Data.Foldable
import Data.Tree
import qualified Data.Set as Set
import Insieme.Inspire.NodeAddress

import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Predecessor
import Insieme.Analysis.Callable
import qualified Insieme.Analysis.Solver as Solver

import Insieme.Analysis.Framework.Utils.OperatorHandler

import qualified Insieme.Utils.UnboundSet as USet
import qualified Insieme.Inspire as IR
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue


--
-- A generic variable and constraint generator for variables describing
-- properties of program points (e.g. the state of a heap object after
-- the execution of a given expression)
--
programPointValue :: (Solver.Lattice a)
         => ProgramPoint                                    -- ^ the program point for which to compute a variable representing a state value
         -> (ProgramPoint -> Solver.Identifier)             -- ^ a variable ID generator function
         -> (ProgramPoint -> Solver.TypedVar a)             -- ^ a variable generator function for referenced variables
         -> [OperatorHandler a]                             -- ^ a list of operator handlers to intercept the interpretation of certain operators
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information

programPointValue pp@(ProgramPoint addr p) idGen analysis ops = case getNode addr of
    
        -- allow operator handlers to intercept the interpretation of calls
        Node IR.CallExpr _ | p == Post -> ivar
            where
            
                extract = ComposedValue.toValue
                
                -- create a variable and an intertangled constraint
                ivar = Solver.mkVariable (idGen pp) [icon] Solver.bot
                icon = Solver.createConstraint idep ival ivar
                
                -- if an handler is active, use the handlers value, else the default
                idep a = (Solver.toVar targetVar) : (
                        if isHandlerActive a then operatorDeps a else dep a
                    )
                ival a = if isHandlerActive a then operatorVal a else val a 
                
                -- the variable storing the target of the call
                targetVar = callableValue (goDown 1 addr)
                
                -- test whether any operator handlers are active
                getActiveHandlers a = filter fit ops
                    where
                        callables = extract $ Solver.get a targetVar
                    
                        targets = USet.toSet $ 
                            if USet.isUniverse callables 
                            then collectAllCallables addr  
                            else callables
                            
                        fit o = any ( \t -> covers o $ toAddress t ) targets 
                    
                
                isHandlerActive a = not . null $ getActiveHandlers a 
                
                -- compute the dependencies of the active handlers
                operatorDeps a = concat $ map go $ getActiveHandlers a
                    where
                        go o = dependsOn o a
                
                -- compute the value computed by the active handlers
                operatorVal a = Solver.join $ map go $ getActiveHandlers a
                    where
                        go o = getValue o a
                
            
        -- everything else is just forwarded
        _ -> var 
    
    where
    
        -- by default, the state at each program point is the joined state of all its predecessors 
        var = Solver.mkVariable (idGen pp) [con] Solver.bot
        con = Solver.createConstraint dep val var
        
        (dep,val) = mkPredecessorConstraintCredentials pp analysis
        
        
mkPredecessorConstraintCredentials :: (Solver.Lattice a)
        => ProgramPoint
        -> (ProgramPoint -> Solver.TypedVar a)
        -> (Solver.Assignment -> [Solver.Var],Solver.Assignment -> a)
        
mkPredecessorConstraintCredentials pp analysis = (dep,val)
    where
        predecessorVar = predecessor pp
        predecessorStateVars a = map (\p -> analysis p) (Solver.get a predecessorVar) 
        
        dep a = (Solver.toVar predecessorVar) : map Solver.toVar (predecessorStateVars a)
        val a = Solver.join $ map (Solver.get a) (predecessorStateVars a) 
        