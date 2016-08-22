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

module Insieme.Analysis.Alias where

import Data.Set as Set
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Reference
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

import qualified Insieme.Utils.UnboundSet as USet

#include "alias_analysis.h"

{#enum AliasAnalysisResult as Results {}
  with prefix = "AliasAnalysisResult_"
  deriving (Eq, Show)
 #}

checkAlias :: NodeAddress -> NodeAddress -> Results
checkAlias x y = checkAlias' rx ry
  where
    -- here we determine the kind of filed index to be used for the reference analysis
    rx :: USet.UnboundSet (Reference SimpleFieldIndex)
    (rx:ry:[]) = ComposedValue.toValue <$> Solver.resolveAll [ referenceValue x, referenceValue y ]


checkAlias' :: Eq i => USet.UnboundSet (Reference i) -> USet.UnboundSet (Reference i) -> Results

checkAlias' USet.Universe s | USet.null s = NotAlias
checkAlias' USet.Universe s               = MayAlias

checkAlias' s USet.Universe  = checkAlias' USet.Universe s


checkAlias' x y | areSingleton = areAlias (toReference x) (toReference y)
  where
    areSingleton = USet.size x == 1 && USet.size y == 1
    toReference = head . USet.toList

checkAlias' x y = if any (==AreAlias) u then MayAlias else NotAlias
  where
    u = [areAlias u v | u <- USet.toList x, v <- USet.toList y]


areAlias :: Eq i => Reference i -> Reference i -> Results
areAlias x y | x == y = AreAlias
areAlias _ _          = NotAlias
