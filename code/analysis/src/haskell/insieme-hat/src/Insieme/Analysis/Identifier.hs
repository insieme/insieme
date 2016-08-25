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

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Identifier where

import Data.Tree
import Data.Typeable
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.UnboundSet as USet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

--
-- * Identifier Value
--

data Identifier = Identifier String
    deriving (Eq, Ord)


instance Show Identifier where
    show (Identifier s) = s


toString :: Identifier -> String
toString (Identifier s) = s


--
-- * Identifier Lattice
--

type IdentifierSet = USet.UnboundSet Identifier

instance Solver.Lattice IdentifierSet where
    bot   = USet.empty
    merge = USet.union

instance Solver.ExtLattice IdentifierSet where
    top   = USet.Universe


--
-- * Identifier Analysis
--

data IdentifierAnalysis = IdentifierAnalysis
    deriving (Typeable)

identifierAnalysis = Solver.mkAnalysisIdentifier IdentifierAnalysis "I"

--
-- * Identifier Variable Generator
--

identifierValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex IdentifierSet)
identifierValue addr = case getNode addr of

    Node IR.Literal [_,Node (IR.StringValue x) _] ->
        Solver.mkVariable (idGen addr) [] (compose $ USet.singleton (Identifier x))

    _ -> dataflowValue addr analysis []

  where
  
    analysis = DataFlowAnalysis IdentifierAnalysis identifierAnalysis identifierValue $ compose USet.Universe
  
    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed
