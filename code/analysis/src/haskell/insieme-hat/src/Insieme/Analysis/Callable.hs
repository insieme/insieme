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

module Insieme.Analysis.Callable where

import Data.List
import Data.Maybe
import Data.Tree
import Data.Typeable
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.UnboundSet as USet

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Entities.FieldIndex

--
-- * Callable Results
--

data Callable =
      Lambda NodeAddress
    | Literal NodeAddress
    | Closure NodeAddress
 deriving (Eq, Ord)

instance Show Callable where
    show (Lambda na) = "Lambda@" ++ (prettyShow na)
    show (Literal na) = "Literal@" ++ (prettyShow na)
    show (Closure na) = "Closure@" ++ (prettyShow na)

toAddress :: Callable -> NodeAddress
toAddress (Lambda a) = a
toAddress (Literal a) = a
toAddress (Closure a) = a

--
-- * Callable Lattice
--

type CallableSet = USet.UnboundSet Callable

instance Solver.Lattice CallableSet where
    bot   = USet.empty
    merge = USet.union

instance Solver.ExtLattice CallableSet where
    top   = USet.Universe


--
-- * Callable Analysis
--

data CallableAnalysis = CallableAnalysis
    deriving (Typeable)

callableAnalysis :: Solver.AnalysisIdentifier
callableAnalysis = Solver.mkAnalysisIdentifier CallableAnalysis "C"


--
-- * Callable Variable Generator
--

callableValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex CallableSet)
callableValue addr = case getNode addr of
    Node IR.LambdaExpr _ ->
        Solver.mkVariable (idGen addr) [] (compose $ USet.singleton (Lambda (fromJust $ getLambda addr )))

    Node IR.BindExpr _ ->
        Solver.mkVariable (idGen addr) [] (compose $ USet.singleton (Closure addr))

    Node IR.Literal _ ->
        Solver.mkVariable (idGen addr) [] (compose $ USet.singleton (Literal addr))

    _ -> dataflowValue addr analysis []

  where

    analysis = DataFlowAnalysis CallableAnalysis callableAnalysis callableValue $ compose USet.Universe

    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed


-- | a utility to collect all callables of a program
collectAllCallables :: NodeAddress -> CallableSet
collectAllCallables addr = USet.fromList $ foldTree collector (getInspire addr)
    where
        collector cur callables = case getNode cur of
            Node IR.Lambda _   -> ((Lambda  cur) : callables)
            Node IR.BindExpr _ -> ((Closure cur) : callables)
            Node IR.Literal _  -> ((Literal cur) : callables)
            _ -> callables


getLambda :: NodeAddress -> Maybe NodeAddress
getLambda addr = case getNode addr of
    Node IR.LambdaExpr [_, ref, Node IR.LambdaDefinition defs] ->
        findLambdaIndex ref defs >>= walk addr
    _ -> Nothing
  where
    findLambdaIndex ref defs = findIndex ((ref==) . (!!0) . subForest) defs
    walk addr i = Just . goDown 1 . goDown i . goDown 2 $ addr
