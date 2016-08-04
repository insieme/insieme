{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference where

import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler

--
-- * DataPaths
--

data DataPath =
      Root
    | Field DataPath String
    | Element DataPath Int 
  deriving (Eq,Ord)
  

instance Show DataPath where
    show Root = "⊥"
    show (Field d f) = (show d) ++ "." ++ f
    show (Element d i) = (show d) ++ "." ++ (show i)
    
-- concatenation of paths
concatPath :: DataPath -> DataPath -> DataPath
concatPath a         Root  =                          a
concatPath a (  Field b s) =   Field (concatPath a b) s
concatPath a (Element b i) = Element (concatPath a b) i 



--
-- * References
--

data Reference = Reference {
        creationPoint :: NodeAddress,
        dataPath      :: DataPath
    }
  deriving (Eq,Ord,Show)


--
-- * Reference Lattice
--

type ReferenceSet = Set.Set Reference

instance Lattice ReferenceSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs
    
    
--
-- * Reference Analysis
--

referenceValue :: NodeAddress -> TypedVar ReferenceSet
referenceValue addr = dataflowValue addr analysis [createHandler]
    where
        analysis = DataFlowAnalysis "R" referenceValue top
        
        top = Set.empty             -- TODO: replace by a real top value!
        
        createHandler = OperatorHandler cov dep val
        
        cov a = any (isBuiltin a) ["ref_alloc","ref_decl"]
        
        dep a = []
        
        val a = Set.singleton $ Reference addr Root
