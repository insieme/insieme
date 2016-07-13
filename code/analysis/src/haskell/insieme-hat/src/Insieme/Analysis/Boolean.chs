module Insieme.Analysis.Boolean where

import Data.Tree
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow

--
-- * Boolean Value Results
--

#include "boolean_analysis.h"

{#enum BooleanAnalysisResult as Result {}
  with prefix = "BooleanAnalysisResult_"
  deriving (Eq, Show)
 #}

--
-- * Boolean Lattice
--

instance Solver.Lattice Result where
    join [] = Neither
    join xs = foldr1 join' xs
      where
        join' Neither x = x
        join' x Neither = x
        join' x y | x == y = x
        join' _ _ = Both

--
-- * Boolean Value Analysis
--

booleanValue :: NodeAddress -> Solver.TypedVar Result
booleanValue addr = case getNode addr of
    Node IR.Literal [_, Node (IR.StringValue "true") _] ->
        Solver.mkVariable (idGen addr) [] AlwaysTrue

    Node IR.Literal [_, Node (IR.StringValue "false") _] ->
        Solver.mkVariable (idGen addr) [] AlwaysFalse

    _ -> dataflowValue addr Both idGen booleanValue

  where
    idGen = Solver.mkIdentifier . ("B"++) . prettyShow
