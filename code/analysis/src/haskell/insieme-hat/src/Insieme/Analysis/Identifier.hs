{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Identifier where

import Data.List
import Data.Tree
import Data.Typeable
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils (foldTree)
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
