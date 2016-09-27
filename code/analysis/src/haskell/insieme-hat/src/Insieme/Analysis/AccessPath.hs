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

module Insieme.Analysis.AccessPath where

import Data.Typeable

import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

import qualified Insieme.Analysis.Entities.AccessPath as AP
import qualified Insieme.Analysis.Entities.DataPath as DP

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.Utils.OperatorHandler

import qualified Insieme.Utils.BoundSet as BSet

import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

import Insieme.Analysis.DataPath

--
-- * AccessPath Lattice
--

type AccessPathSet i = BSet.BoundSet BSet.Bound10 (AP.AccessPath i)

instance (FieldIndex i) => Solver.Lattice (AccessPathSet i) where
    bot   = BSet.empty
    merge = BSet.union

instance (FieldIndex i) => Solver.ExtLattice (AccessPathSet i) where
    top = BSet.Universe


--
-- * AccessPath Analysis
--


data AccessPathAnalysis = AccessPathAnalysis
    deriving (Typeable)


--
-- * AccessPath Variable Generator
--

accessPathValue :: (FieldIndex i) => NodeAddress -> Solver.TypedVar (ValueTree.Tree i (AccessPathSet i))
accessPathValue addr = case getNodeType addr of 

    IR.Variable -> dataflowValue addr analysis ops

    _ -> dataflowValue addr analysis ops

  where

    analysis = (mkDataFlowAnalysis AccessPathAnalysis "AP" accessPathValue) {
        initialValueHandler = initValueHandler
    }

    compose = ComposedValue.toComposed

    initValueHandler a | isRoot a = compose $ BSet.singleton $ AP.global $ getNodePair a
    initValueHandler a = compose $ BSet.singleton $ AP.parameter $ getIndex a 

    -- add operator support

    ops = [ allocHandler , declHandler , refNarrow , refExpand , refCast , refReinterpret ]

    allocHandler = OperatorHandler cov noDep val
        where
            cov a = isBuiltin a $ getBuiltin addr "ref_alloc"
            val a = compose $ BSet.singleton AP.local

    declHandler = OperatorHandler cov noDep val
        where
            cov a = isBuiltin a $ getBuiltin addr "ref_decl"
            val a = compose $ BSet.singleton AP.local

    refNarrow = OperatorHandler cov subRefDep val
        where
            cov a = isBuiltin a $ getBuiltin addr "ref_narrow"
            val a = compose $ narrow (baseAccessPathVal a) (dataPathVal a)
            narrow = BSet.lift2 $ \a d -> AP.append a d

    refExpand = OperatorHandler cov subRefDep val
        where
            cov a = isBuiltin a $ getBuiltin addr "ref_expand"
            val a = compose $ expand (baseAccessPathVal a) (dataPathVal a)
            expand = BSet.lift2 $ \a d -> AP.append a (DP.invert d)

    refCast = OperatorHandler cov dep val
        where
            cov a = isBuiltin a $ getBuiltin addr "ref_cast"
            dep _ = [Solver.toVar baseAccessPathVar]
            val a = Solver.get a baseAccessPathVar

    refReinterpret = OperatorHandler cov dep val
        where
            cov a = isBuiltin a $ getBuiltin addr "ref_reinterpret"
            dep _ = [Solver.toVar baseAccessPathVar]
            val a = Solver.get a baseAccessPathVar            -- TODO: check when this conversion is actually valid

    noDep a = []

    subRefDep a = [Solver.toVar baseAccessPathVar, Solver.toVar dataPathVar]

    baseAccessPathVar   = accessPathValue $ goDown 1 $ goDown 2 addr
    baseAccessPathVal a = ComposedValue.toValue $ Solver.get a baseAccessPathVar

    dataPathVar   = dataPathValue $ goDown 3 addr
    dataPathVal a = BSet.fromUnboundSet $ ComposedValue.toValue $ Solver.get a dataPathVar
