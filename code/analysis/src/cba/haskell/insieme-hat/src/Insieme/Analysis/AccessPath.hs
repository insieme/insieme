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

module Insieme.Analysis.AccessPath where

import Control.DeepSeq (NFData)
import Data.Typeable
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Entities.AccessPath as AP
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow

--
-- * AccessPath Lattice
--

newtype AccessPathSet i = AccessPathSet { unAPS :: BSet.BoundSet BSet.Bound10 (AP.AccessPath i) }
  deriving (Eq, Ord, Show, Generic, NFData)

instance (FieldIndex i) => Solver.Lattice (AccessPathSet i) where
    bot = AccessPathSet BSet.empty
    (AccessPathSet x) `merge` (AccessPathSet y) = AccessPathSet $ BSet.union x y

instance (FieldIndex i) => Solver.ExtLattice (AccessPathSet i) where
    top = AccessPathSet BSet.Universe


--
-- * AccessPath Analysis
--


data AccessPathAnalysis = AccessPathAnalysis
    deriving (Typeable)


--
-- * AccessPath Variable Generator
--

accessPathValue :: FieldIndex i => NodeAddress -> Solver.TypedVar (ValueTree.Tree i (AccessPathSet i))
accessPathValue addr = case Q.getNodeType addr of

    I.Variable -> dataflowValue addr analysis ops

    _ -> dataflowValue addr analysis ops

  where

    analysis = (mkDataFlowAnalysis AccessPathAnalysis "AP" accessPathValue) {
        initialValueHandler = initValueHandler
    }

    compose = ComposedValue.toComposed . AccessPathSet

    initValueHandler a | I.isRoot a = compose $ BSet.singleton $ AP.global $ I.getNode a
    initValueHandler a = compose $ BSet.singleton $ AP.parameter $ I.getIndex a

    -- add operator support

    ops = [ allocHandler , declHandler , refNarrow , refExpand , refCast , refReinterpret ]

    allocHandler = OperatorHandler cov noDep val
        where
            cov a = Q.isBuiltin a "ref_alloc"
            val _ _ = compose $ BSet.singleton AP.local

    declHandler = OperatorHandler cov noDep val
        where
            cov a = Q.isBuiltin a "ref_decl"
            val _ _ = compose $ BSet.singleton AP.local

    refNarrow = OperatorHandler cov subRefDep val
        where
            cov a = Q.isBuiltin a "ref_narrow"
            val _ a = compose $ narrow (baseAccessPathVal a) (dataPathVal a)
            narrow = BSet.lift2 $ \a d -> AP.append a d

    refExpand = OperatorHandler cov subRefDep val
        where
            cov a = Q.isBuiltin a "ref_expand"
            val _ a = compose $ expand (baseAccessPathVal a) (dataPathVal a)
            expand = BSet.lift2 $ \a d -> AP.append a (DP.invert d)

    refCast = OperatorHandler cov dep val
        where
            cov a = Q.isBuiltin a "ref_cast"
            dep _ _ = [Solver.toVar baseAccessPathVar]
            val _ a = Solver.get a baseAccessPathVar

    refReinterpret = OperatorHandler cov dep val
        where
            cov a = Q.isBuiltin a "ref_reinterpret"
            dep _ _ = [Solver.toVar baseAccessPathVar]
            val _ a = Solver.get a baseAccessPathVar            -- TODO: check when this conversion is actually valid

    noDep _ _ = []

    subRefDep _ _ = [Solver.toVar baseAccessPathVar, Solver.toVar dataPathVar]

    baseAccessPathVar   = accessPathValue $ I.goDown 1 $ I.goDown 2 addr
    baseAccessPathVal a = unAPS $ ComposedValue.toValue $ Solver.get a baseAccessPathVar

    dataPathVar   = dataPathValue $ I.goDown 3 addr
    dataPathVal a = BSet.changeBound $ unDPS $ ComposedValue.toValue $ Solver.get a dataPathVar
