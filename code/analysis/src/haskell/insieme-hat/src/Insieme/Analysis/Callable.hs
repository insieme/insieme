{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Callable where

import Data.List
import Data.Maybe
import Data.Tree
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
  
    analysis = DataFlowAnalysis "C" callableValue $ compose USet.Universe
  
    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed


-- | a utility to collect all callables of a program
collectAllCallables :: NodeAddress -> CallableSet
collectAllCallables addr = USet.fromList $ foldTree collector (getRootIR addr)
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
