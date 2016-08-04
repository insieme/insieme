module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(DataFlowAnalysis,analysisID,variableGenerator,topValue),
    mkVarIdentifier,
    dataflowValue
) where

import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver

import Insieme.Analysis.Framework.Utils.OperatorHandler

--
-- * Generic Data Flow Value Analysis
--

data DataFlowAnalysis a = DataFlowAnalysis {
    analysisID         :: String,                               -- ^ an identifier for the represented analysis
    variableGenerator  :: NodeAddress -> Solver.TypedVar a,     -- ^ the variable generator of the represented analysis
    topValue           :: a                                     -- ^ the top value of this analysis
}

-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a -> NodeAddress -> Solver.Identifier

--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (Solver.Lattice a)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis a                              -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
