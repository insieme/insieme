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

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.DataPath where

import Data.Typeable
import Insieme.Analysis.Arithmetic
import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Identifier
import Insieme.Inspire.NodeAddress hiding (append)
import Insieme.Inspire.Query
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow

--
-- * DataPath Lattice
--

type DataPathSet i = BSet.UnboundSet (DataPath i)

instance (FieldIndex i) => Solver.Lattice (DataPathSet i) where
    bot   = BSet.empty
    merge = BSet.union

instance (FieldIndex i) => Solver.ExtLattice (DataPathSet i) where
    top = BSet.Universe


--
-- * DataPath Analysis
--


data DataPathAnalysis = DataPathAnalysis
    deriving (Typeable)


--
-- * DataPath Variable Generator
--

dataPathValue :: (FieldIndex i) => NodeAddress -> Solver.TypedVar (ValueTree.Tree i (DataPathSet i))
dataPathValue addr = dataflowValue addr analysis ops

  where

    analysis = mkDataFlowAnalysis DataPathAnalysis "DP" dataPathValue

    compose = ComposedValue.toComposed

    -- add operator support

    ops = [ rootOp, member, element] -- TODO: add parent

    -- handle the data path root constructore --
    rootOp = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "dp_root"

        dep _ _ = []

        val _ _ = compose $ BSet.singleton root


    -- the handler for the member access path constructore --
    member = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "dp_member"

        dep _ _ = (Solver.toVar nestedPathVar) : (Solver.toVar fieldNameVar) : []

        val _ a = compose $ combine (paths a) fieldNames
            where
                combine = BSet.lift2 $ \p i -> append p ((step . field) (toString i))
                fieldNames = ComposedValue.toValue $ Solver.get a fieldNameVar

        fieldNameVar = identifierValue $ goDown 3 addr


    -- the handler for the element and component access path constructore --
    element = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) ["dp_element","dp_component"]

        dep _ _ = (Solver.toVar nestedPathVar) : (Solver.toVar indexVar) : []

        val _ a = compose $ combine (paths a) indexes
            where
                combine BSet.Universe  _ = BSet.Universe
                combine ps BSet.Universe = BSet.map (\p -> append p (step unknownIndex)) ps
                combine ps is = (BSet.lift2 $ \p i -> append p ((step . index) i)) ps is

                indexes = BSet.toUnboundSet $ ComposedValue.toValue $ Solver.get a indexVar

        indexVar = arithmeticValue $ goDown 3 addr


    -- common utilities --

    nestedPathVar = dataPathValue $ goDown 2 addr

    paths a = ComposedValue.toValue $ Solver.get a nestedPathVar
