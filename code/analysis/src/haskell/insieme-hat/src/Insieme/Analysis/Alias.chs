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
checkAlias x y = checkAlias' (toSetReference x) (toSetReference y)
  where
    -- here we determine the kind of filed index to be used for the reference analysis
    toSetReference :: NodeAddress -> USet.UnboundSet (Reference SimpleFieldIndex)
    toSetReference = ComposedValue.toValue . Solver.resolve . referenceValue


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
