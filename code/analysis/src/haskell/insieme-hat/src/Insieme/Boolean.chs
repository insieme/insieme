module Insieme.Boolean where

import Compiler.Analysis

#include "boolean_analysis.h"

{#enum BooleanAnalysisResult as Result {} with prefix = "BooleanAnalysisResult_" deriving (Eq, Show)#}

instance Lattice Result where
    bot = Neither

    join Neither x = x
    join x Neither = x
    join x y | x == y = x
    join _ _ = Both
