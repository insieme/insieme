module Insieme.Analysis.Alias where

import Data.Set as Set
import Insieme.Analysis.Reference
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

#include "alias_analysis.h"

{#enum AliasAnalysisResult as Results {}
  with prefix = "AliasAnalysisResult_"
  deriving (Eq, Show)
 #}

checkAlias :: NodeAddress -> NodeAddress -> Results
checkAlias x y = checkAlias' (toSetReference x) (toSetReference y)
  where
    toSetReference = ComposedValue.toValue . Solver.resolve . referenceValue


checkAlias' :: Eq i => Set (Reference i) -> Set (Reference i) -> Results

checkAlias' x y | areSingleton = areAlias (toReference x) (toReference y)
  where
    areSingleton = Set.size x == 1 && Set.size y == 1
    toReference = head . Set.toList

checkAlias' x y = if any (==AreAlias) u then MayAlias else NotAlias
  where
    u = [areAlias u v | u <- Set.toList x, v <- Set.toList y]


areAlias :: Eq i => Reference i -> Reference i -> Results
areAlias x y | x == y = AreAlias
areAlias _ _          = NotAlias
