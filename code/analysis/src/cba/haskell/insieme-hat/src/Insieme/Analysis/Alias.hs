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

module Insieme.Analysis.Alias where

import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Reference
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Utils.BoundSet as BSet

data Result = AreAlias | MayAlias | NotAlias
  deriving (Eq, Ord, Show)

checkAlias :: Solver.SolverState -> NodeAddress -> NodeAddress -> (Result,Solver.SolverState)
checkAlias initial x y = (checkAlias' rx ry, final)
  where
    -- here we determine the kind of filed index to be used for the reference analysis
    rx :: BSet.UnboundSet (Reference SimpleFieldIndex)
    (rx:ry:[]) = ComposedValue.toValue <$> res
    (res,final) = Solver.resolveAll initial [ referenceValue x, referenceValue y ]


checkAlias' :: Eq i => BSet.UnboundSet (Reference i) -> BSet.UnboundSet (Reference i) -> Result

checkAlias' BSet.Universe s | BSet.null s = NotAlias
checkAlias' BSet.Universe _               = MayAlias

checkAlias' s BSet.Universe  = checkAlias' BSet.Universe s


checkAlias' x y | areSingleton = areAlias (toReference x) (toReference y)
  where
    areSingleton = BSet.size x == 1 && BSet.size y == 1
    toReference = head . BSet.toList

checkAlias' x y = if any (==AreAlias) us then MayAlias else NotAlias
  where
    us = [areAlias u v | u <- BSet.toList x, v <- BSet.toList y]


areAlias :: Eq i => Reference i -> Reference i -> Result
areAlias x y | x == y = AreAlias
areAlias _ _          = NotAlias
