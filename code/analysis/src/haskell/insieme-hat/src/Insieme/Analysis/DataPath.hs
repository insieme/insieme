{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.DataPath where


import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set

import Insieme.Analysis.Entities.DataPath
import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler

import qualified Insieme.Utils.BoundSet as BSet
import qualified Insieme.Utils.UnboundSet as USet

import Insieme.Analysis.Identifier
import Insieme.Analysis.Arithmetic

import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex


--
-- * DataPath Lattice
--

type DataPathSet i = USet.UnboundSet (DataPath i)

instance (FieldIndex i) => Solver.Lattice (DataPathSet i) where
    bot   = USet.empty
    merge = USet.union

instance (FieldIndex i) => Solver.ExtLattice (DataPathSet i) where
    top = USet.Universe
    
    
--
-- * DataPath Analysis
--

dataPathValue :: (FieldIndex i) => NodeAddress -> Solver.TypedVar (ValueTree.Tree i (DataPathSet i))
dataPathValue addr = dataflowValue addr analysis ops
                
  where
  
    analysis = DataFlowAnalysis "DP" dataPathValue top
  
    idGen = mkVarIdentifier analysis
  
    top = compose USet.Universe
    
    compose = ComposedValue.toComposed
    
    -- add operator support
    
    ops = [ rootOp, member, element] -- TODO: add parent
    
    -- handle the data path root constructore --
    rootOp = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "dp_root"
        
        dep a = []
         
        val a = compose $ USet.singleton root
    
    
    -- the handler for the member access path constructore --
    member = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "dp_member"
        
        dep a = (Solver.toVar nestedPathVar) : (Solver.toVar fieldNameVar) : []
        
        val a = compose $ combine (paths a) fieldNames
            where
                combine = USet.lift2 $ \p i -> append p ((step . field) (toString i))
                fieldNames = ComposedValue.toValue $ Solver.get a fieldNameVar
                
        fieldNameVar = identifierValue $ goDown 3 addr
    
    
    -- the handler for the element and component access path constructore --
    element = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) ["dp_element","dp_component"]
        
        dep a = (Solver.toVar nestedPathVar) : (Solver.toVar indexVar) : []
        
        val a = compose $ combine (paths a) indexes 
            where
                combine = USet.lift2 $ \p i -> append p ((step . index) i)
                indexes = BSet.toUnboundSet $ ComposedValue.toValue $ Solver.get a indexVar 
                
        indexVar = arithmeticValue $ goDown 3 addr
    
    
    -- common utilities --
    
    nestedPathVar = dataPathValue $ goDown 2 addr
    
    paths a = ComposedValue.toValue $ Solver.get a nestedPathVar