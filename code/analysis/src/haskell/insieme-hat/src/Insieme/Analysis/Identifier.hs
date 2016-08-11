{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Identifier where

import Data.List
import Data.Tree
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils (foldTree)
import qualified Data.Set as Set
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

--
-- * Identifier Value
--

data Identifier = Identifier String
    deriving (Eq, Ord, Show)


toString :: Identifier -> String
toString (Identifier s) = s


--
-- * Identifier Lattice
--

type IdentifierSet = Set.Set Identifier

instance Solver.Lattice IdentifierSet where
    bot   = Set.empty
    merge = Set.union

--
-- * Identifier Analysis
--

identifierValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex IdentifierSet)
identifierValue addr = case getNode addr of

    Node IR.Literal [_,Node (IR.StringValue x) _] ->
        Solver.mkVariable (idGen addr) [] (compose $ Set.singleton (Identifier x))

    _ -> dataflowValue addr analysis []

  where
  
    analysis = DataFlowAnalysis "ID" identifierValue $ compose allIdentifier
  
    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed

    allIdentifier = Set.fromList $ foldTree collector (getRootIR addr)
    collector cur identifier = case getNode cur of
        Node (IR.StringValue x) _  -> (Identifier x : identifier)
        _ -> identifier
