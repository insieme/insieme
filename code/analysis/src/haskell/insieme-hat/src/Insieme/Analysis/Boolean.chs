module Insieme.Analysis.Boolean where

import Data.Maybe
import Data.Tree
import Insieme.Analysis.Arithmetic
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.NodeAddress
import Insieme.Utils.Arithmetic (NumOrdering(..), numCompare)
import qualified Data.Map as Map
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

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
booleanValue addr =
    case () of _
                | isBuiltin addr "true"  -> Solver.mkVariable (idGen addr) [] AlwaysTrue
                | isBuiltin addr "false" -> Solver.mkVariable (idGen addr) [] AlwaysFalse
                | otherwise      -> dataflowValue addr analysis ops
  where

    analysis = DataFlowAnalysis "B" booleanValue Both
    idGen = mkVarIdentifier analysis

    ops = [ lt, le, eq, ne, ge, gt ]

    lt = OperatorHandler cov dep (val cmp)
      where
        cov a = any (isBuiltin a) ["int_lt", "uint_lt"]
        cmp x y = case numCompare x y of
            NumLT     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    le = OperatorHandler cov dep (val cmp)
      where
        cov a = any (isBuiltin a) ["int_le", "uint_le"]
        cmp x y = case numCompare x y of
            NumEQ     -> AlwaysTrue
            NumLT     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    eq = OperatorHandler cov dep (val cmp)
      where
        cov a = any (isBuiltin a) ["int_eq", "uint_eq"]
        cmp x y = case numCompare x y of
            NumEQ     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    ne = OperatorHandler cov dep (val cmp)
      where
        cov a = any (isBuiltin a) ["int_ne", "uint_ne"]
        cmp x y = case numCompare x y of
            NumEQ     -> AlwaysFalse
            Sometimes -> Both
            _         -> AlwaysTrue

    ge = OperatorHandler cov dep (val cmp)
      where
        cov a = any (isBuiltin a) ["int_ge", "uint_ge"]
        cmp x y = case numCompare x y of
            NumGT     -> AlwaysTrue
            NumEQ     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    gt = OperatorHandler cov dep (val cmp)
      where
        cov a = any (isBuiltin a) ["int_gt", "uint_gt"]
        cmp x y = case numCompare x y of
            NumGT     -> AlwaysTrue
            Sometimes -> Both
            _         -> AlwaysFalse

    lhs = arithmeticValue $ goDown 2 addr
    rhs = arithmeticValue $ goDown 3 addr

    dep a = Solver.toVar <$> [lhs, rhs]

    val op a = combine (Solver.get a lhs) (Solver.get a rhs)
      where
        combine BSet.Universe _ = Both
        combine _ BSet.Universe = Both
        combine x y = Solver.join [ u `op` v | u <- BSet.toList x, v <- BSet.toList y]
