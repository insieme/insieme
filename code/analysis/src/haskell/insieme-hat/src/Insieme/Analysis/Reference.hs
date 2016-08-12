{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference where

import Debug.Trace
import Data.Typeable
import Data.Maybe
import Data.Tree
import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR

import qualified Insieme.Utils.UnboundSet as USet

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

type ReferenceSet i = USet.UnboundSet (Reference i)

instance (Eq i,Ord i,Show i,Typeable i) => Lattice (ReferenceSet i) where
    bot   = USet.empty
    merge = USet.union

instance (Eq i,Ord i,Show i,Typeable i) => ExtLattice (ReferenceSet i) where
    top   = USet.Universe
        
    
--
-- * Reference Analysis
--

referenceValue :: (FieldIndex i) => NodeAddress -> TypedVar (ValueTree.Tree i (ReferenceSet i))
referenceValue addr = case getNode addr of 

        d@(Node IR.Declaration _) | isMaterializingDeclaration d ->  
            mkVariable (idGen addr) [] (compose $ USet.singleton $ Reference addr DP.root)

        c@(Node IR.CallExpr _) | isMaterializingCall c -> 
            trace ("Found materializing call: " ++ (show addr)) $
            mkVariable (idGen addr) [] (compose $ USet.singleton $ Reference addr DP.root)

        _ -> dataflowValue addr analysis opsHandler
        
    where
    
        analysis = DataFlowAnalysis "R" referenceValue top
        idGen = mkVarIdentifier analysis
        
        compose = ComposedValue.toComposed
        
        top = compose USet.Universe
        
        opsHandler = [ allocHandler , declHandler , refNarrow , refExpand , refCast ]
        
        allocHandler = OperatorHandler cov noDep val
            where 
                cov a = isBuiltin a "ref_alloc"
                val a = compose $ USet.singleton $ Reference addr DP.root

        declHandler = OperatorHandler cov noDep val
            where 
                cov a = isBuiltin a "ref_decl"
                val a = compose $ USet.singleton $ Reference (getEnclosingDecl addr) DP.root
        
        refNarrow = OperatorHandler cov subRefDep val
            where 
                cov a = isBuiltin a "ref_narrow"
                val a = compose $ USet.fromList [ Reference l (DP.append p d)  | Reference l p <- baseRefVal a, d <- dataPathVal a]
        
        refExpand = OperatorHandler cov subRefDep val
            where 
                cov a = isBuiltin a "ref_expand"
                val a = compose $ USet.fromList [ Reference l (DP.append p (DP.invert d))  | Reference l p <- baseRefVal a, d <- dataPathVal a]
        
        refCast = OperatorHandler cov dep val
            where 
                cov a = isBuiltin a "ref_cast"
                dep _ = [toVar baseRefVar]
                val a = get a baseRefVar
        
        noDep a = []
        
        subRefDep a = [toVar baseRefVar, toVar dataPathVar] 
        
        baseRefVar   = referenceValue $ goDown 1 $ goDown 2 addr
        baseRefVal a = USet.toList $ ComposedValue.toValue $ get a baseRefVar
        
        dataPathVar   = dataPathValue $ goDown 3 addr
        dataPathVal a = USet.toList $ ComposedValue.toValue $ get a dataPathVar
        
        
        

-- Tests whether an IR node represents a reference type or not
isReference :: Tree IR.NodeType -> Bool
isReference (Node IR.GenericType ((Node (IR.StringValue "ref") _) : _)) = True
isReference _                                                           = False
        
getTypeParam :: Int -> Tree IR.NodeType -> Tree IR.NodeType
getTypeParam i (Node IR.GenericType ( _ : _ : (Node IR.Types params) : [] )) = params !! i
        
hasMoreReferences :: Tree IR.NodeType -> Tree IR.NodeType -> Bool
hasMoreReferences a b | isReference a && (not . isReference $ b) = True
hasMoreReferences a b | isReference a && isReference b = hasMoreReferences (getTypeParam 0 a) (getTypeParam 0 b)
hasMoreReferences _ _ = False


-- tests whether the given node is a materializing declaration
isMaterializingDeclaration :: Tree IR.NodeType -> Bool
isMaterializingDeclaration (Node IR.Declaration [declType,Node _ (initType:_)]) = hasMoreReferences declType initType
isMaterializingDeclaration _ = False


-- tests whether the given node is a materializing call
isMaterializingCall :: Tree IR.NodeType -> Bool
isMaterializingCall _ = False        -- the default, for now - TODO: implement real check
-- isMaterializingCall (Node IR.CallExpr ( resType : (Node _ ((Node IR.FunctionType (_:retType:_)):_)) : _)) = hasMoreReferences resType retType 
-- isMaterializingCall _ = False

getEnclosingDecl :: NodeAddress -> NodeAddress
getEnclosingDecl addr = case getNode addr of
    Node IR.Declaration _ -> addr
    _ | isRoot addr       -> error "getEnclosingDecl has no parent to go to"
    _                     -> getEnclosingDecl $ fromJust $ getParent addr
