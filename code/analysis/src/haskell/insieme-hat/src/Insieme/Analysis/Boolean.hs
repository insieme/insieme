
module Insieme.Analysis.Boolean (
    booleanValue
) where


import qualified Insieme.Analysis.Solver as Solver
import {-# SOURCE #-} Insieme.Analysis.Framework.Dataflow
import qualified Insieme.Boolean as Boolean

import Insieme.Inspire.NodeAddress
--import Data.List
import Data.Tree
--import Data.Maybe
--import qualified Data.Set as Set
import qualified Insieme.Inspire as IR
--import Insieme.Inspire.Utils



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