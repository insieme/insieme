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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference (
    Location,
    Reference(..),
    ReferenceSet,
    referenceValue,

    isMaterializingDeclaration,
    isMaterializingCall
) where

import Control.DeepSeq
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Analysis.Arithmetic
import Insieme.Analysis.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress as Addr
import Insieme.Inspire.Query
import Insieme.Utils.Arithmetic
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow

--
-- * Location
--

type Location = NodeAddress



--
-- * References
--

data Reference i =
      Reference {
          creationPoint :: Location,
          dataPath      :: DP.DataPath i
      }
    | NullReference
    | UninitializedReference
  deriving (Eq,Ord,Show,Generic,NFData)


--
-- * Reference Lattice
--

type ReferenceSet i = BSet.UnboundSet (Reference i)

instance (Eq i,Ord i,Show i,Typeable i,NFData i) => Lattice (ReferenceSet i) where
    bot   = BSet.empty
    merge = BSet.union

instance (Eq i,Ord i,Show i,Typeable i,NFData i) => ExtLattice (ReferenceSet i) where
    top   = BSet.singleton UninitializedReference       -- What is not known, is not a valid reference


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
            mkVariable (idGen addr) [] (compose $ BSet.singleton $ Reference (crop addr) DP.root)

        IR.Declaration | isMaterializingDeclaration (Addr.getNode addr) ->
            mkVariable (idGen addr) [] (compose $ BSet.singleton $ Reference addr DP.root)

        IR.CallExpr | isMaterializingCall (Addr.getNode addr) ->
            mkVariable (idGen addr) [] (compose $ BSet.singleton $ Reference addr DP.root)

        _ -> dataflowValue addr analysis opsHandler

    where

        analysis = (mkDataFlowAnalysis ReferenceAnalysis "R" referenceValue){
            entryPointParameterHandler=epParamHandler,
            initValueHandler = compose $ BSet.singleton $ NullReference
        }

        epParamHandler a = mkConstant analysis a $ compose $ BSet.singleton $ Reference a DP.root

        idGen = mkVarIdentifier analysis

        compose = ComposedValue.toComposed

        opsHandler = [ allocHandler , declHandler , refNull, refNarrow , refExpand , refCast , refReinterpret , ptrToRef , ptrFromRef ]

        allocHandler = OperatorHandler cov noDep val
            where
                cov a = isBuiltin a "ref_alloc"
                val a = compose $ BSet.singleton $ Reference addr DP.root

        declHandler = OperatorHandler cov noDep val
            where
                cov a = isBuiltin a "ref_decl"
                val a = compose $ BSet.singleton $ Reference (getEnclosingDecl addr) DP.root

        refNull = OperatorHandler cov noDep val
            where
                cov a = isBuiltin a "ref_null"
                val a = compose $ BSet.singleton NullReference

        refNarrow = OperatorHandler cov subRefDep val
            where
                cov a = isBuiltin a "ref_narrow"
                val a = compose $ narrow (baseRefVal a) (dataPathVal a)
                narrow = BSet.lift2 $ onRefs2 $ \(Reference l p) d -> Reference l (DP.append p d)

        refExpand = OperatorHandler cov subRefDep val
            where
                cov a = isBuiltin a "ref_expand"
                val a = compose $ expand (baseRefVal a) (dataPathVal a)
                expand = BSet.lift2 $ onRefs2 $ \(Reference l p) d -> Reference l (DP.append p (DP.invert d))

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

                access = BSet.lift2 $ onRefs2 $ \(Reference l p) offset -> Reference l (DP.append p (DP.step $ index offset))

        ptrFromRef = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ptr_from_ref"
                dep _ = [toVar baseRefVar]
                val a = ComposedValue.composeElements [(component 0,compose res)]
                    where
                        res = lower $ baseRefVal a
                        lower = BSet.map $ onRefs $ \(Reference l p) -> Reference l (DP.append p (DP.invert $ DP.step $ component 0))


        noDep a = []

        subRefDep a = [toVar baseRefVar, toVar dataPathVar]

        baseRefVar   = referenceValue $ goDown 1 $ goDown 2 addr
        baseRefVal a = ComposedValue.toValue $ get a baseRefVar

        dataPathVar   = dataPathValue $ goDown 3 addr
        dataPathVal a = ComposedValue.toValue $ get a dataPathVar


        -- a utility filtering out actual references
        onRefs _ NullReference          = NullReference
        onRefs _ UninitializedReference = UninitializedReference
        onRefs f r = f r

        onRefs2 _ NullReference          _ = NullReference
        onRefs2 _ UninitializedReference _ = UninitializedReference
        onRefs2 f r d = f r d


getTypeParam :: Int -> IR.Tree -> IR.Tree
getTypeParam i (IR.Node IR.GenericType ( _ : _ : (IR.Node IR.Types params) : [] )) = params !! i
getTypeParam _ _ = error "unexpected NodeType"

hasMoreReferences :: IR.Tree -> IR.Tree -> Bool
hasMoreReferences a b | isReference a && (not . isReference $ b) = True
hasMoreReferences a b | isReference a && isReference b = hasMoreReferences (getTypeParam 0 a) (getTypeParam 0 b)
hasMoreReferences _ _ = False


-- tests whether the given node is a materializing declaration
isMaterializingDeclaration :: IR.Tree -> Bool
isMaterializingDeclaration (IR.Node IR.Declaration [declType,IR.Node _ (initType:_)]) = hasMoreReferences declType initType
isMaterializingDeclaration _ = False


-- tests whether the given node is a materializing call
isMaterializingCall :: IR.Tree -> Bool
isMaterializingCall _ = False        -- the default, for now - TODO: implement real check
-- isMaterializingCall (IR.Node IR.CallExpr ( resType : (IR.Node _ ((IR.Node IR.FunctionType (_:retType:_)):_)) : _)) = hasMoreReferences resType retType
-- isMaterializingCall _ = False

getEnclosingDecl :: NodeAddress -> NodeAddress
getEnclosingDecl addr = case getNodeType addr of
        IR.Declaration | not isCtorThisParam && not isRefCastParam -> addr
        _ | isRoot addr -> error "getEnclosingDecl has no parent to go to"
        _               -> getEnclosingDecl $ fromJust $ getParent addr
    where

        fun = (goDown 1) <$> getParent addr

        isCtorThisParam = isCtorCall && getIndex addr == 2
        isCtorCall = fromMaybe False $ (isConstructor . Addr.getNode) <$> fun

        isRefCastParam = isRefCastCall && getIndex addr == 2
        isRefCastCall = case fun of
            Just f  -> case getNodeType f of
                IR.LambdaExpr -> isRefCast f
                IR.Literal    -> isRefCast f
                _             -> False
            Nothing -> False

        isRefCast f = any (isBuiltin f) [
                        "ref_cast",
                        "ref_const_cast",
                        "ref_volatile_cast",
                        "ref_kind_cast",
                        "ref_reinterpret",
                        "ref_parent_cast"
                    ]