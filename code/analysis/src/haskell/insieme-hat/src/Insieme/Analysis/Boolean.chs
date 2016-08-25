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

module Insieme.Analysis.Boolean where

import Data.Typeable
import Insieme.Analysis.Arithmetic
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.NodeAddress
import Insieme.Utils.Arithmetic (NumOrdering(..), numCompare)
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

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
                | isBuiltin addr "true"  -> Solver.mkVariable (idGen addr) [] $ compose AlwaysTrue
                | isBuiltin addr "false" -> Solver.mkVariable (idGen addr) [] $ compose AlwaysFalse
                | otherwise      -> dataflowValue addr analysis ops
  where

    compose = ComposedValue.toComposed 
    extract = ComposedValue.toValue

    analysis = DataFlowAnalysis BooleanAnalysis booleanAnalysis booleanValue (compose Both)
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

    val op a = combine (extract $ Solver.get a lhs) (extract $ Solver.get a rhs)
      where
        combine BSet.Universe _ = compose Both
        combine _ BSet.Universe = compose Both
        combine x y = compose . Solver.join $ [ u `op` v | u <- BSet.toList x, v <- BSet.toList y]
