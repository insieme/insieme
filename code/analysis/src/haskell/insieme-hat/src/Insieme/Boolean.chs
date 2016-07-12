module Insieme.Boolean where

import Insieme.Analysis.Solver

#include "boolean_analysis.h"

{#enum BooleanAnalysisResult as Result {}
  with prefix = "BooleanAnalysisResult_"
  deriving (Eq, Show)
 #}

instance Lattice Result where
    join [] = Neither
    join xs = foldr1 join' xs
      where
        join' Neither x = x
        join' x Neither = x
        join' x y | x == y = x
        join' _ _ = Both
