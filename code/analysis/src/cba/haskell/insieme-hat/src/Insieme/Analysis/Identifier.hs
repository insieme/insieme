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

module Insieme.Analysis.Identifier where

import Control.DeepSeq
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

--
-- * Identifier Value
--

data Identifier = Identifier String
    deriving (Eq, Ord, Generic, NFData)


instance Show Identifier where
    show (Identifier s) = s


toString :: Identifier -> String
toString (Identifier s) = s


--
-- * Identifier Lattice
--

type IdentifierSet = BSet.UnboundSet Identifier

instance Solver.Lattice IdentifierSet where
    bot   = BSet.empty
    merge = BSet.union

instance Solver.ExtLattice IdentifierSet where
    top   = BSet.Universe


--
-- * Identifier Analysis
--

data IdentifierAnalysis = IdentifierAnalysis
    deriving (Typeable)


--
-- * Identifier Variable Generator
--

identifierValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex IdentifierSet)
identifierValue addr = case getNode addr of

    IR.Node IR.Literal [_, IR.Node (IR.StringValue x) _] ->
        Solver.mkVariable (idGen addr) [] (compose $ BSet.singleton (Identifier x))

    _ -> dataflowValue addr analysis []

  where

    analysis = mkDataFlowAnalysis IdentifierAnalysis "I" identifierValue

    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed
