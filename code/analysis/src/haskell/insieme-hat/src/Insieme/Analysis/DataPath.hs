{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.DataPath where


import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set

import Insieme.Analysis.Entities.DataPath
import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler

import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Identifier
import Insieme.Analysis.Arithmetic

import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex


--
-- * DataPath Lattice
--

type DataPathSet = Set.Set (DataPath SimpleFieldIndex)

instance Solver.Lattice DataPathSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs
    
    
--
-- * DataPath Analysis
--

dataPathValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex DataPathSet)
dataPathValue addr = 
    case () of _
                | isBuiltin addr "dp_root"  -> Solver.mkVariable (idGen addr) [] (compose $ Set.singleton root)
                | otherwise                 -> dataflowValue addr analysis ops
                
  where
  
    analysis = DataFlowAnalysis "DP" dataPathValue top
  
    idGen = mkVarIdentifier analysis
  
    top = compose Set.empty     -- TODO: compute actual top
    
    compose = ComposedValue.toComposed
    
    -- add operator support
    
    ops = [member,element] -- TODO: add parent
    
    -- the handler for the member access path constructore --
    member = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "dp_member"
        
        dep a = (Solver.toVar nestedPathVar) : (Solver.toVar fieldNameVar) : []
         
        val a = compose $ Set.fromList [ append p ((step . field) i) | p <- paths a, i <- identifier ]
            where
                identifier = toString <$> (Set.toList $ ComposedValue.toValue $ Solver.get a fieldNameVar)
                
        fieldNameVar = identifierValue $ goDown 3 addr
    
    
    -- the handler for the element and component access path constructore --
    element = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) ["dp_element","dp_component"]
        
        dep a = (Solver.toVar nestedPathVar) : (Solver.toVar indexVar) : []
         
        val a = compose $ Set.fromList [ append p ((step . index) i) | p <- paths a, i <- indices ]
            where
                indices = BSet.toList $ ComposedValue.toValue $ Solver.get a indexVar
                
        indexVar = arithmeticValue $ goDown 3 addr
    
    
    -- common utilities --
    
    nestedPathVar = dataPathValue $ goDown 2 addr
    
    paths a  =  Set.toList $ ComposedValue.toValue $ Solver.get a nestedPathVar