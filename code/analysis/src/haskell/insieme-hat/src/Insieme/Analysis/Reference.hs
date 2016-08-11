{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference where

import Data.Typeable
import Data.Maybe
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


import qualified Insieme.Analysis.Entities.DataPath as DP
import Insieme.Analysis.DataPath



--
-- * References
--

data Reference i = Reference {
        creationPoint :: NodeAddress,
        dataPath      :: DP.DataPath i
    }
  deriving (Eq,Ord,Show)


--
-- * Reference Lattice
--

type ReferenceSet i = Set.Set (Reference i)

instance (Eq i,Ord i,Show i,Typeable i) => Lattice (ReferenceSet i) where
    join [] = Set.empty
    join xs = foldr1 Set.union xs
    
    
--
-- * Reference Analysis
--

referenceValue :: NodeAddress -> TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
referenceValue addr = case getNode addr of 

        d@(Node IR.Declaration _) ->  
            if isMaterializing d
            then mkVariable (idGen addr) [] (compose $ Set.singleton $ Reference addr DP.root)
            else dataflowValue addr analysis opsHandler                    

        _ -> dataflowValue addr analysis opsHandler
        
    where
    
        analysis = DataFlowAnalysis "R" referenceValue top
        idGen = mkVarIdentifier analysis
        
        compose = ComposedValue.toComposed
        
        top = compose Set.empty             -- TODO: replace by a real top value!
        
        opsHandler = [ allocHandler , declHandler , refNarrow , refExpand ]
        
        allocHandler = OperatorHandler cov noDep val
            where 
                cov a = isBuiltin a "ref_alloc"
                val a = compose $ Set.singleton $ Reference addr DP.root

        declHandler = OperatorHandler cov noDep val
            where 
                cov a = isBuiltin a "ref_decl"
                val a = compose $ Set.singleton $ Reference (getEnclosingDecl addr) DP.root
        
        refNarrow = OperatorHandler cov subRefDep val
            where 
                cov a = isBuiltin a "ref_narrow"
                val a = compose $ Set.fromList [ Reference l (DP.append p d)  | Reference l p <- baseRefVal a, d <- dataPathVal a]
        
        refExpand = OperatorHandler cov subRefDep val
            where 
                cov a = isBuiltin a "ref_expand"
                val a = compose $ Set.fromList [ Reference l (DP.append p (DP.invert d))  | Reference l p <- baseRefVal a, d <- dataPathVal a]
        
        
        noDep a = []
        
        subRefDep a = [toVar baseRefVar, toVar dataPathVar] 
        
        baseRefVar   = referenceValue $ goDown 2 addr
        baseRefVal a = Set.toList $ ComposedValue.toValue $ get a baseRefVar
        
        dataPathVar   = dataPathValue $ goDown 3 addr
        dataPathVal a = Set.toList $ ComposedValue.toValue $ get a dataPathVar
        
        
        

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
    _ | isRoot addr       -> error "getEnclosingDecl has no parent to go to"
    _                     -> getEnclosingDecl $ fromJust $ getParent addr
