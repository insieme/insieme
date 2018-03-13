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

module Insieme.Analysis.Boolean where

import Control.DeepSeq
import Data.Typeable
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import Insieme.Utils.Arithmetic (NumOrdering(..), numCompare)
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Arithmetic
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Solver as Solver
import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow

--
-- * Boolean Value Results
--
data Result = AlwaysTrue | AlwaysFalse | Both | Neither
  deriving (Eq, Ord, Show, Generic, NFData)

--
-- * Boolean Lattice
--

instance Solver.Lattice Result where
    bot = Neither

    merge Neither x = x
    merge x Neither = x
    merge x y | x == y = x
    merge _ _ = Both


instance Solver.ExtLattice Result where
    top = Both


--
-- * Boolean Value Analysis
--


data BooleanAnalysis = BooleanAnalysis
    deriving (Typeable)

booleanAnalysis :: Solver.AnalysisIdentifier
booleanAnalysis = Solver.mkAnalysisIdentifier BooleanAnalysis "B"


--
-- * Boolean Value Variable Generator
--

booleanValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex Result)
booleanValue addr =
    case () of _
                | Q.isBuiltin addr "true"  -> Solver.mkVariable (idGen addr) [] $ compose AlwaysTrue
                | Q.isBuiltin addr "false" -> Solver.mkVariable (idGen addr) [] $ compose AlwaysFalse
                | otherwise      -> dataflowValue addr analysis ops
  where

    compose = ComposedValue.toComposed
    extractSFS = unSFS . ComposedValue.toValue

    analysis = mkDataFlowAnalysis BooleanAnalysis "B" booleanValue
    idGen = mkVarIdentifier analysis

    ops = [ lt, le, eq, ne, ge, gt ]

    lt = OperatorHandler cov dep (val cmp)
      where
        cov a = any (Q.isBuiltin a) ["int_lt", "uint_lt"]
        cmp x y = case numCompare x y of
            NumLT     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    le = OperatorHandler cov dep (val cmp)
      where
        cov a = any (Q.isBuiltin a) ["int_le", "uint_le"]
        cmp x y = case numCompare x y of
            NumEQ     -> AlwaysTrue
            NumLT     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    eq = OperatorHandler cov dep (val cmp)
      where
        cov a = any (Q.isBuiltin a) ["int_eq", "uint_eq"]
        cmp x y = case numCompare x y of
            NumEQ     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    ne = OperatorHandler cov dep (val cmp)
      where
        cov a = any (Q.isBuiltin a) ["int_ne", "uint_ne"]
        cmp x y = case numCompare x y of
            NumEQ     -> AlwaysFalse
            Sometimes -> Both
            _         -> AlwaysTrue

    ge = OperatorHandler cov dep (val cmp)
      where
        cov a = any (Q.isBuiltin a) ["int_ge", "uint_ge"]
        cmp x y = case numCompare x y of
            NumGT     -> AlwaysTrue
            NumEQ     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    gt = OperatorHandler cov dep (val cmp)
      where
        cov a = any (Q.isBuiltin a) ["int_gt", "uint_gt"]
        cmp x y = case numCompare x y of
            NumGT     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    lhs = arithmeticValue $ I.goDown 1 $ I.goDown 2 addr
    rhs = arithmeticValue $ I.goDown 1 $ I.goDown 3 addr

    dep _ _ = Solver.toVar <$> [lhs, rhs]

    val op _ a = combine (extractSFS $ Solver.get a lhs) (extractSFS $ Solver.get a rhs)
      where
        combine BSet.Universe _ = compose Both
        combine _ BSet.Universe = compose Both
        combine x y = compose . Solver.join $ [ u `op` v | u <- BSet.toList x, v <- BSet.toList y]
