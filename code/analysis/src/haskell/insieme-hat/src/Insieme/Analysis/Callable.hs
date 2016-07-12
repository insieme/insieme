
module Insieme.Analysis.Callable (
    callableValue
) where

import qualified Insieme.Analysis.Solver as Solver
import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Callable as Callable
import Insieme.Inspire.NodeAddress
import Data.List
import Data.Tree
import Data.Maybe
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR
import Insieme.Inspire.Utils



--
-- * Callable Analysis
--

callableValue :: NodeAddress -> Solver.TypedVar Callable.CallableSet
--callableValue a | trace ("Resolving callable value for " ++ (prettyShow a ) ) False = undefined
callableValue addr = case getNode addr of
    Node IR.LambdaExpr _ ->
        Solver.mkVariable (idGen addr) [] (Set.singleton (Callable.Lambda (fromJust $ getLambda addr )))

    Node IR.BindExpr _ ->
        Solver.mkVariable (idGen addr) [] (Set.singleton (Callable.Closure addr))

    Node IR.Literal _ ->
        Solver.mkVariable (idGen addr) [] (Set.singleton (Callable.Literal addr))

    _ -> dataflowValue addr allCallables idGen callableValue

  where

    idGen = Solver.mkIdentifier . ("C"++) . prettyShow

    allCallables = Set.fromList $ foldTree collector (getRoot addr)
    collector cur callables = case getNode cur of
        Node IR.Lambda _   -> ((Callable.Lambda  cur) : callables)
        Node IR.BindExpr _ -> ((Callable.Closure cur) : callables)
        Node IR.Literal _  -> ((Callable.Literal cur) : callables)
        _ -> callables  



getLambda :: NodeAddress -> Maybe NodeAddress
getLambda addr =
    case getNode addr of
        Node IR.LambdaExpr [_, ref, Node IR.LambdaDefinition defs] ->
            findLambda ref defs >>= walk addr
        _ -> Nothing
  where
    findLambda ref defs = findIndex ((ref==) . (!!0) . subForest) defs
    walk addr x = Just . goDown 1 . goDown x . goDown 2 $ addr