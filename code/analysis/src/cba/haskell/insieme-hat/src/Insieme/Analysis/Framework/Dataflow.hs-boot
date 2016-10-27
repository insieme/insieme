module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(
        DataFlowAnalysis,
        analysisIdentifier,
        variableGenerator,
        topValue,
        freeVariableHandler,
        entryPointParameterHandler,
        initialValueHandler,
        initValueHandler
    ),
    mkDataFlowAnalysis,
    mkVarIdentifier,
    mkConstant,
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
    analysis                   :: a,                                    -- ^ the analysis type token
    analysisIdentifier         :: Solver.AnalysisIdentifier,            -- ^ the analysis identifier
    variableGenerator          :: NodeAddress -> Solver.TypedVar v,     -- ^ the variable generator of the represented analysis
    topValue                   :: v,                                    -- ^ the top value of this analysis
    freeVariableHandler        :: NodeAddress -> Solver.TypedVar v,     -- ^ a function computing the value of a free variable
    entryPointParameterHandler :: NodeAddress -> Solver.TypedVar v,     -- ^ a function computing the value of a entry point parameter
    initialValueHandler        :: NodeAddress -> v,                     -- ^ a function computing the initial value of a memory location
    initValueHandler           :: v                                     -- ^ default value of a memory location
}

-- a function creating a simple data flow analysis
mkDataFlowAnalysis :: (Typeable a, Solver.ExtLattice v) => a -> String -> (NodeAddress -> Solver.TypedVar v) -> DataFlowAnalysis a v

-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a v -> NodeAddress -> Solver.Identifier

-- a function creating a data flow analysis variable representing a constant value
mkConstant :: (Typeable a, Solver.ExtLattice v) => DataFlowAnalysis a v -> NodeAddress -> v -> Solver.TypedVar v

--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (ComposedValue.ComposedValue a i v, Typeable d)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis d a                            -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information

