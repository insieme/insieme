{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
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
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Reference (
    Location,
    Reference(..),
    ReferenceSet(..),
    referenceValue,

    isMaterializingDeclaration,
    isMaterializingCall
) where

import Control.DeepSeq (NFData)
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Analysis.Arithmetic
import Insieme.Analysis.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress as Addr
import Insieme.Inspire.Query
import Insieme.Utils.Arithmetic (mkConst)

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

newtype ReferenceSet i = ReferenceSet { unRS :: BSet.UnboundSet (Reference i) }
  deriving (Eq, Ord, Show, Generic, NFData)

instance (Eq i,Ord i,Show i,Typeable i,NFData i) => Lattice (ReferenceSet i) where
    bot = ReferenceSet BSet.empty
    (ReferenceSet x) `merge` (ReferenceSet y) = ReferenceSet $ BSet.union x y

instance (Eq i,Ord i,Show i,Typeable i,NFData i) => ExtLattice (ReferenceSet i) where
    top = ReferenceSet $ BSet.singleton UninitializedReference       -- What is not known, is not a valid reference


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
            mkVariable (idGen addr) [] (compose $ BSet.singleton $ Reference (crop addr) DP.Root)

        IR.Declaration | isMaterializingDeclaration (Addr.getNode addr) ->
            mkVariable (idGen addr) [] (compose $ BSet.singleton $ Reference addr DP.Root)

        IR.CallExpr | isMaterializingCall (Addr.getNode addr) ->
            mkVariable (idGen addr) [] (compose $ BSet.singleton $ Reference addr DP.Root)

        _ -> dataflowValue addr analysis opsHandler

    where

        analysis = (mkDataFlowAnalysis ReferenceAnalysis "R" referenceValue){
            freeVariableHandler=epParamHandler,
            entryPointParameterHandler=epParamHandler,
            initValueHandler = compose $ BSet.singleton $ NullReference
        }

        epParamHandler a = mkConstant analysis a $ compose $ BSet.singleton $ Reference a DP.Root

        idGen = mkVarIdentifier analysis

        compose = ComposedValue.toComposed . ReferenceSet

        opsHandler = [ allocHandler , declHandler , refNull, refNarrow , refExpand , refCast , refReinterpret , ptrToRef , ptrFromRef , stdArraySubscript ]

        allocHandler = OperatorHandler cov noDep val
            where
                cov a = isBuiltin a "ref_alloc"
                val _ _ = compose $ BSet.singleton $ Reference addr DP.Root

        declHandler = OperatorHandler cov noDep val
            where
                cov a = isBuiltin a "ref_decl"
                val _ _ = compose $ BSet.singleton $ Reference (getEnclosingDecl addr) DP.Root

        refNull = OperatorHandler cov noDep val
            where
                cov a = isBuiltin a "ref_null"
                val _ _ = compose $ BSet.singleton NullReference

        refNarrow = OperatorHandler cov subRefDep val
            where
                cov a = isBuiltin a "ref_narrow"
                val _ a = compose $ narrow (baseRefVal a) (unDPS $ dataPathVal a)
                narrow = BSet.lift2 $ onRefs2 $ \(Reference l p) d -> Reference l (DP.append p d)

        refExpand = OperatorHandler cov subRefDep val
            where
                cov a = isBuiltin a "ref_expand"
                val _ a = compose $ expand (baseRefVal a) (unDPS $ dataPathVal a)
                expand = BSet.lift2 $ onRefs2 $ \(Reference l p) d -> Reference l (DP.append p (DP.invert d))

        refCast = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ref_cast"
                dep _ _ = [toVar baseRefVar]
                val _ a = get a baseRefVar

        refReinterpret = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ref_reinterpret"
                dep _ _ = [toVar baseRefVar]
                val _ a = get a baseRefVar            -- TODO: check when this conversion is actually valid

        ptrToRef = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ptr_to_ref"
                dep _ _ = [toVar baseRefVar, toVar offsetVar]
                val _ a = compose $ access (baseRefVal a) (offsetVal a)

                baseRefVal a = unRS $ ComposedValue.toValue $ ComposedValue.getElement (DP.step $ component 0) $ get a baseRefVar

                offsetVar = arithmeticValue $ goDown 1 $ goDown 2 addr
                offsetVal a = BSet.toUnboundSet $ unSFS $ ComposedValue.toValue $ ComposedValue.getElement (DP.step $ component 1) $ get a offsetVar

                access = BSet.lift2 $ onRefs2 $ \(Reference l p) offset -> Reference l (DP.append p (DP.step $ arrayIndex offset))

        ptrFromRef = OperatorHandler cov dep val
            where
                cov a = isBuiltin a "ptr_from_ref"
                dep _ _ = [toVar baseRefVar]
                val _ a = ComposedValue.composeElements [(component 0,compose res)]
                    where
                        res = lower $ baseRefVal a
                        lower = BSet.map $ onRefs $ \(Reference l p) -> Reference l (DP.append p (DP.invert $ DP.step $ arrayIndex $ mkConst 0))


        -- container support --
        
        stdArraySubscript = OperatorHandler cov dep val
          where
            cov = isOneOf [
                        "IMP_std_colon__colon_array::IMP__operator_subscript_",
                        "IMP_std_colon__colon_array::IMP_at"
                    ]
            dep _ _ = [toVar baseRefVar, toVar indexVar]
            val _ a = compose $ access (baseRefVal a) (indexVal a)
            
            indexVar = arithmeticValue $ goDown 1 $ goDown 3 addr
            indexVal a = BSet.toUnboundSet $ unSFS $ ComposedValue.toValue $ get a indexVar

            access = BSet.lift2 $ onRefs2 $ \(Reference l p) offset -> Reference l (DP.append p (DP.step $ stdArrayIndex offset))


        isOperator n a = fromMaybe False $ (==n) <$> getLiteralValue a
        
        isOneOf ns a = any go ns
          where
            go n = isOperator n a

        noDep _ _ = []

        subRefDep _ _ = [toVar baseRefVar, toVar dataPathVar]

        baseRefVar   = referenceValue $ goDown 1 $ goDown 2 addr
        baseRefVal a = unRS $ ComposedValue.toValue $ get a baseRefVar

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

        isCallParam = fromMaybe False $ isCallExpr <$> getParent addr
        isCtorThisParam = isCallParam && isCtorCall && getIndex addr == 2
        isCtorCall = fromMaybe False $ (isConstructor . Addr.getNode) <$> fun

        isRefCastParam = isCallParam && isRefCastCall && getIndex addr == 2
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
