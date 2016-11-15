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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Callable where

import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import Insieme.Inspire.Visit
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

--
-- * Callable Results
--

data Callable =
      Lambda NodeAddress
    | Literal NodeAddress
    | Closure NodeAddress
 deriving (Eq, Ord, Generic, NFData)

instance Show Callable where
    show (Lambda na) = "Lambda@" ++ (prettyShow na)
    show (Literal na) = "Literal@" ++ (prettyShow na)
    show (Closure na) = "Closure@" ++ (prettyShow na)

toAddress :: Callable -> NodeAddress
toAddress (Lambda a) = a
toAddress (Literal a) = a
toAddress (Closure a) = a

--
-- * Callable Lattice
--

type CallableSet = BSet.UnboundSet Callable

instance Solver.Lattice CallableSet where
    bot   = BSet.empty
    merge = BSet.union

instance Solver.ExtLattice CallableSet where
    top   = BSet.Universe


--
-- * Callable Analysis
--

data CallableAnalysis = CallableAnalysis
    deriving (Typeable)


--
-- * Callable Variable Generator
--

callableValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex CallableSet)
callableValue addr = case getNodeType addr of
    IR.LambdaExpr ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton (Lambda (fromJust $ getLambda addr )))

    IR.LambdaReference ->
        Solver.mkVariable (idGen addr) [] (compose $ getCallables4Ref addr)

    IR.BindExpr ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton (Closure addr))

    IR.Literal ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton (Literal addr))

    _ -> dataflowValue addr analysis []

  where

    analysis = mkDataFlowAnalysis CallableAnalysis "C" callableValue

    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed

    getCallables4Ref r = search (getNode r) r
        where
            search r cur = case getNode cur of
                IR.Node IR.LambdaDefinition cs | isJust pos -> BSet.singleton (Lambda $ goDown 1 $ goDown (fromJust pos) cur)
                    where
                        pos = findIndex filter cs
                        filter (IR.Node IR.LambdaBinding [a,_]) = a == r
                _ | isRoot cur      -> BSet.Universe
                _                   -> search r $ goUp cur


-- | a utility to collect all callables of a program
collectAllCallables :: NodeAddress -> CallableSet
collectAllCallables addr = BSet.fromList $ foldTree collector (getRoot addr)
    where
        collector cur callables = case getNodeType cur of
            IR.Lambda   -> ((Lambda  cur) : callables)
            IR.BindExpr -> ((Closure cur) : callables)
            IR.Literal  -> ((Literal cur) : callables)
            _ -> callables