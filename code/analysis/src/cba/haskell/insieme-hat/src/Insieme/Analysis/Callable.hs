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

    getCallables4Ref ref = search (getNode ref) ref
        where
            search r cur = case getNode cur of
                IR.Node IR.LambdaDefinition cs | isJust pos -> BSet.singleton (Lambda $ goDown 1 $ goDown (fromJust pos) cur)
                    where
                        pos = findIndex filter' cs

                        filter' (IR.Node IR.LambdaBinding [a,_]) = a == r
                        filter' _ = error "unhandled getCallables4Ref filter"

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
