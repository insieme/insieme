
module Insieme.Analysis.Boolean (
    booleanValue
) where


import qualified Insieme.Analysis.Solver as Solver
import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow

import Data.Tree
import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

import qualified Insieme.Boolean as Boolean


--
-- * Boolean Value Analysis
--

booleanValue :: NodeAddress -> Solver.TypedVar Boolean.Result
--booleanValue a | trace ("Resolving boolean value for " ++ (prettyShow a ) ) False = undefined
booleanValue addr = case getNode addr of
    Node IR.Literal [_, Node (IR.StringValue "true") _] ->
        Solver.mkVariable (idGen addr) [] Boolean.AlwaysTrue

    Node IR.Literal [_, Node (IR.StringValue "false") _] ->
        Solver.mkVariable (idGen addr) [] Boolean.AlwaysFalse

    _ -> dataflowValue addr Boolean.Both idGen booleanValue

  where

    idGen = Solver.mkIdentifier . ("B"++) . prettyShow