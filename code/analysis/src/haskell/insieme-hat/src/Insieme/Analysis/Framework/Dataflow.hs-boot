module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(DataFlowAnalysis,analysis,analysisIdentifier,variableGenerator,topValue),
    mkVarIdentifier,
    dataflowValue
) where

import Data.Typeable
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver

import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * Generic Data Flow Value Analysis
--

data DataFlowAnalysis a v = DataFlowAnalysis {
    analysis           :: a,                                    -- ^ the analysis type token
    analysisIdentifier :: Solver.AnalysisIdentifier,            -- ^ the analysis identifier
    variableGenerator  :: NodeAddress -> Solver.TypedVar v,     -- ^ the variable generator of the represented analysis
    topValue           :: v                                     -- ^ the top value of this analysis
}

-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a v -> NodeAddress -> Solver.Identifier


--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (ComposedValue.ComposedValue a i v, Typeable d)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis d a                            -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information

