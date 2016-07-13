module Insieme.Analysis.Framework.Dataflow (
    dataflowValue
) where

import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver

--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (Solver.Lattice a)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> a                                               -- ^ the top value of the lattice
         -> (NodeAddress -> Solver.Identifier)              -- ^ a variable ID generator function
         -> (NodeAddress -> Solver.TypedVar a)              -- ^ a variable generator function for referenced variables
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
