module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(
        DataFlowAnalysis,
        analysisIdentifier,
        variableGenerator,
        topValue,
        iteratorVariableHandler,
        freeVariableHandler,
        entryPointParameterHandler,
        initialValueHandler,
        initialValue,
        uninitializedValue,
        excessiveFileAccessHandler,
        unknownOperatorHandler,
        forwardCtorDtorResultValue,
        implicitCtorHandler
    ),
    mkDataFlowAnalysis,
    mkVarIdentifier,
    mkConstant,
    dataflowValue
) where

import GHC.Stack

import Data.Typeable

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Analysis.Solver as Solver

import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * Generic Data Flow Value Analysis
--

data DataFlowAnalysis a v i = DataFlowAnalysis {
    analysis                   :: a,                                         -- ^ the analysis type token
    analysisIdentifier         :: Solver.AnalysisIdentifier,                 -- ^ the analysis identifier
    variableGenerator          :: NodeAddress -> Solver.TypedVar v,          -- ^ the variable generator of the represented analysis
    topValue                   :: v,                                         -- ^ the top value of this analysis
    iteratorVariableHandler    :: NodeAddress -> Solver.TypedVar v,          -- ^ a function creating the constraints for the given iterator variable
    freeVariableHandler        :: NodeAddress -> Solver.TypedVar v,          -- ^ a function computing the value of a free variable
    entryPointParameterHandler :: NodeAddress -> Solver.TypedVar v,          -- ^ a function computing the value of a entry point parameter
    initialValueHandler        :: NodeAddress -> v,                          -- ^ a function computing the initial value of a memory location
    initialValue               :: v,                                         -- ^ default value of a memory location
    uninitializedValue         :: v,                                         -- ^ value of an uninitialized memory location
    excessiveFileAccessHandler :: v -> i -> v,                               -- ^ a handler processing excessive field accesses (if ref_narrow calls navigate too deep)
    unknownOperatorHandler     :: NodeAddress -> v,                          -- ^ a handler invoked for unknown operators
    forwardCtorDtorResultValue :: Bool,                                      -- ^ a flag to enable / disable the implicit return of constructors and destructurs
    implicitCtorHandler        :: Maybe (NodeAddress -> Solver.TypedVar v)   -- ^ an optional override for the handling of constructors in the defined value analysis
}

-- a function creating a simple data flow analysis
mkDataFlowAnalysis :: (Typeable a, Solver.ExtLattice v) => a -> String -> (NodeAddress -> Solver.TypedVar v) -> DataFlowAnalysis a v i

-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a v i -> NodeAddress -> Solver.Identifier

-- a function creating a data flow analysis variable representing a constant value
mkConstant :: (Typeable a, Solver.ExtLattice v) => DataFlowAnalysis a v i -> NodeAddress -> v -> Solver.TypedVar v

--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (HasCallStack, ComposedValue.ComposedValue a i v, Typeable d)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis d a i                          -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information

