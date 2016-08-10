{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.DataPath where

import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set

import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Framework.Dataflow

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex


--
-- * DataPath Lattice
--

type DataPathSet = Set.Set (DataPath SimpleFieldIndex)

instance Lattice DataPathSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs
    
    
--
-- * DataPath Analysis
--

dataPathValue :: NodeAddress -> TypedVar (ValueTree.Tree SimpleFieldIndex DataPathSet)
dataPathValue addr = 
    case () of _
                | isBuiltin addr "dp_root"  -> mkVariable (idGen addr) [] (compose $ Set.singleton root)
                | otherwise                 -> dataflowValue addr analysis []
                
  where
  
    analysis = DataFlowAnalysis "DP" dataPathValue top
  
    idGen = mkVarIdentifier analysis
  
    top = compose Set.empty     -- TODO: compute actual top
    
    compose = ComposedValue.toComposed
  