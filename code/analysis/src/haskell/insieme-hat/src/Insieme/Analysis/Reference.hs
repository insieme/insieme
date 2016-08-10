{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference where

import Data.Tree
import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

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

referenceValue :: NodeAddress -> TypedVar (ValueTree.Tree SimpleFieldIndex ReferenceSet)
referenceValue addr = case getNode addr of 

        d@(Node IR.Declaration _) ->  
            if isMaterializing d
            then mkVariable (idGen addr) [] (compose $ Set.singleton $ Reference addr Root)
            else dataflowValue addr analysis opsHandler                    

        _ -> dataflowValue addr analysis opsHandler
        
    where
    
        analysis = DataFlowAnalysis "R" referenceValue top
        idGen = mkVarIdentifier analysis
        
        compose = ComposedValue.toComposed
        
        top = compose Set.empty             -- TODO: replace by a real top value!
        
        opsHandler = [ allocHandler , declHandler ]
        
        allocHandler = OperatorHandler cov dep val
            where 
                cov a = isBuiltin a "ref_alloc"
                val a = compose $ Set.singleton $ Reference addr Root

        declHandler = OperatorHandler cov dep val
            where 
                cov a = isBuiltin a "ref_decl"
                val a = compose $ Set.singleton $ Reference (getEnclosingDecl addr) Root
        
        dep a = []
        

-- Tests whether an IR node represents a reference type or not
isReference :: Tree IR.NodeType -> Bool
isReference (Node IR.GenericType ((Node (IR.StringValue "ref") _) : _)) = True
isReference _                                                           = False
        
getTypeParam :: Int -> Tree IR.NodeType -> Tree IR.NodeType
getTypeParam i (Node IR.GenericType ( _ : _ : (Node IR.Types params) : [] )) = params !! i
        
sameNumReferences :: Tree IR.NodeType -> Tree IR.NodeType -> Bool
sameNumReferences a b | (not . isReference $ a) && (not . isReference $ b) = True
sameNumReferences a b | isReference a && isReference b = sameNumReferences (getTypeParam 0 a) (getTypeParam 0 b)
sameNumReferences _ _ = False

-- TODO: count number of nested refs in type and init type instead
isMaterializing :: Tree IR.NodeType -> Bool
isMaterializing decl@(Node IR.Declaration [declType,Node _ (initType:_)]) = not $ sameNumReferences declType initType

getEnclosingDecl :: NodeAddress -> NodeAddress
getEnclosingDecl addr = case getNode addr of
    Node IR.Declaration _ -> addr
    _                     -> getEnclosingDecl addr
