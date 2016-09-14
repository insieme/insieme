{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference where

import Data.Typeable
import Data.Maybe
import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

import Insieme.Utils.Arithmetic
import qualified Insieme.Utils.BoundSet as BSet
import qualified Insieme.Utils.UnboundSet as USet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula


import qualified Insieme.Analysis.Entities.DataPath as DP
import Insieme.Analysis.Arithmetic
import Insieme.Analysis.DataPath


--
-- * Location
--

type Location = NodeAddress



--
-- * References
--

data Reference i = Reference {
        creationPoint :: Location,
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


data ReferenceAnalysis = ReferenceAnalysis
    deriving (Typeable)


--
-- * Reference Variable Generator
--

referenceValue :: (FieldIndex i) => NodeAddress -> TypedVar (ValueTree.Tree i (ReferenceSet i))
referenceValue addr = case getNodeType addr of

        IR.Literal ->
            mkVariable (idGen addr) [] (compose $ USet.singleton $ Reference (crop addr) DP.root)

        IR.Declaration | isMaterializingDeclaration (getNodePair addr) ->
            mkVariable (idGen addr) [] (compose $ USet.singleton $ Reference addr DP.root)

        IR.CallExpr | isMaterializingCall (getNodePair addr) ->
            mkVariable (idGen addr) [] (compose $ USet.singleton $ Reference addr DP.root)

        _ -> dataflowValue addr analysis opsHandler

    where

        analysis = (mkDataFlowAnalysis ReferenceAnalysis "R" referenceValue){
            entryPointParameterHandler=epParamHandler
        }
        
        epParamHandler a = mkConstant analysis a $ compose $ USet.singleton $ Reference a DP.root

        idGen = mkVarIdentifier analysis

        compose = ComposedValue.toComposed

        opsHandler = [ allocHandler , declHandler , refNarrow , refExpand , refCast , refReinterpret , ptrToRef , ptrFromRef ]

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
                val a = compose $ narrow (baseRefVal a) (dataPathVal a)
                narrow = USet.lift2 $ \(Reference l p) d -> Reference l (DP.append p d)

        refExpand = OperatorHandler cov subRefDep val
            where
                cov a = isBuiltin a "ref_expand"
                val a = compose $ expand (baseRefVal a) (dataPathVal a)
                expand = USet.lift2 $ \(Reference l p) d -> Reference l (DP.append p (DP.invert d))

        refCast = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ref_cast"
                dep _ = [toVar baseRefVar]
                val a = get a baseRefVar

        refReinterpret = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ref_reinterpret"
                dep _ = [toVar baseRefVar]
                val a = get a baseRefVar            -- TODO: check when this conversion is actually valid

        ptrToRef = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ptr_to_ref"
                dep _ = [toVar baseRefVar, toVar offsetVar]
                val a = compose $ access (baseRefVal a) (offsetVal a)
                
                baseRefVal a = ComposedValue.toValue $ ComposedValue.getElement (DP.step $ component 0) $ get a baseRefVar 
                
                offsetVar = arithmeticValue $ goDown 1 $ goDown 2 addr
                offsetVal a = BSet.toUnboundSet $ ComposedValue.toValue $ ComposedValue.getElement (DP.step $ component 1) $ get a offsetVar
                
                access = USet.lift2 $ \(Reference l p) offset -> Reference l (DP.append p (DP.step $ index offset))  

        ptrFromRef = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ptr_from_ref"
                dep _ = [toVar baseRefVar]
                val a = ComposedValue.composeElements [(component 0,compose res)]
                    where
                        res = lower $ baseRefVal a
                        lower = USet.lift $ \(Reference l p) -> Reference l (DP.append p (DP.invert $ DP.step $ component 0))


        noDep a = []

        subRefDep a = [toVar baseRefVar, toVar dataPathVar]

        baseRefVar   = referenceValue $ goDown 1 $ goDown 2 addr
        baseRefVal a = ComposedValue.toValue $ get a baseRefVar

        dataPathVar   = dataPathValue $ goDown 3 addr
        dataPathVal a = ComposedValue.toValue $ get a dataPathVar




-- Tests whether an IR node represents a reference type or not
isReference :: IR.Tree -> Bool
isReference (IR.NT IR.GenericType ((IR.NT (IR.StringValue "ref") _) : _)) = True
isReference _                                                             = False

getTypeParam :: Int -> IR.Tree -> IR.Tree
getTypeParam i (IR.NT IR.GenericType ( _ : _ : (IR.NT IR.Types params) : [] )) = params !! i
getTypeParam _ _ = error "unexpected NodeType"

hasMoreReferences :: IR.Tree -> IR.Tree -> Bool
hasMoreReferences a b | isReference a && (not . isReference $ b) = True
hasMoreReferences a b | isReference a && isReference b = hasMoreReferences (getTypeParam 0 a) (getTypeParam 0 b)
hasMoreReferences _ _ = False


-- tests whether the given node is a materializing declaration
isMaterializingDeclaration :: IR.Tree -> Bool
isMaterializingDeclaration (IR.NT IR.Declaration [declType,IR.NT _ (initType:_)]) = hasMoreReferences declType initType
isMaterializingDeclaration _ = False


-- tests whether the given node is a materializing call
isMaterializingCall :: IR.Tree -> Bool
isMaterializingCall _ = False        -- the default, for now - TODO: implement real check
-- isMaterializingCall (IR.NT IR.CallExpr ( resType : (IR.NT _ ((IR.NT IR.FunctionType (_:retType:_)):_)) : _)) = hasMoreReferences resType retType
-- isMaterializingCall _ = False

getEnclosingDecl :: NodeAddress -> NodeAddress
getEnclosingDecl addr = case getNodeType addr of
    IR.Declaration  -> addr
    _ | isRoot addr -> error "getEnclosingDecl has no parent to go to"
    _               -> getEnclosingDecl $ fromJust $ getParent addr
