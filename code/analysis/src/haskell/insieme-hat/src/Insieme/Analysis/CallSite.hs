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

import Data.Tree
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

callSiteAnalysis = Solver.mkAnalysisIdentifier CallSiteAnalysis "CS"


--
-- * CallSite Variable Generator
--

callSites :: NodeAddress -> Solver.TypedVar CallSiteSet
callSites addr = case getNode addr of
    
    -- for lambdas and bind exressions: collect all call sites
    Node IR.Lambda   _ -> var
    Node IR.BindExpr _ -> var
    
    -- everything else has no call sites
    _ -> Solver.mkVariable id [] Solver.bot
        
  where
  
    id = Solver.mkIdentifier callSiteAnalysis addr ""
        
    var = Solver.mkVariable id [con] Solver.bot
    con = Solver.createConstraint dep val var
    
    allCalls = foldTree collector (getRootIR addr) 
    collector a calls = case getNode a of
        Node IR.CallExpr _  -> a : calls
        _                   -> calls 
    
    allTrgVars = map (\c -> (c , Callable.callableValue $ goDown 1 c ) ) allCalls           
    
    callable = case getNode addr of
        Node IR.Lambda _   -> Callable.Lambda addr
        Node IR.BindExpr _ ->  Callable.Closure addr
    
    dep a = map Solver.toVar (map snd allTrgVars)
    val a = foldr go Set.empty allTrgVars
        where
            go = \(call,var) set ->
                if (USet.member callable (ComposedValue.toValue $ Solver.get a var))
                then Set.insert (CallSite call) set 
                else set
