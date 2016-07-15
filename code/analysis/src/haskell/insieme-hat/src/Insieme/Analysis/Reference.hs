{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference where

import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set

import Insieme.Analysis.Framework.Dataflow


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
referenceValue addr = undefined