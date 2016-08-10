{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Callable where

import Data.List
import Data.Maybe
import Data.Tree
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
import qualified Data.Set as Set
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR

import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import Insieme.Analysis.Framework.PropertySpace.FieldIndex

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

type CallableSet = Set.Set Callable

instance Solver.Lattice CallableSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs

--
-- * Callable Analysis
--

callableValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex CallableSet)
callableValue addr = case getNode addr of
    Node IR.LambdaExpr _ ->
        Solver.mkVariable (idGen addr) [] (compose $ Set.singleton (Lambda (fromJust $ getLambda addr )))

    Node IR.BindExpr _ ->
        Solver.mkVariable (idGen addr) [] (compose $ Set.singleton (Closure addr))

    Node IR.Literal _ ->
        Solver.mkVariable (idGen addr) [] (compose $ Set.singleton (Literal addr))

    _ -> dataflowValue addr analysis []

  where
  
    analysis = DataFlowAnalysis "C" callableValue $ compose allCallables
  
    idGen = mkVarIdentifier analysis

    compose = ComposedValue.toComposed

    allCallables = Set.fromList $ foldTree collector (getRootIR addr)
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
