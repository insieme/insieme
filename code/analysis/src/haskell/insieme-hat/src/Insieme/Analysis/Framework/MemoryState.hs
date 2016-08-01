
module Insieme.Analysis.Framework.MemoryState where


import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Predecessor
import Insieme.Inspire.NodeAddress

import qualified Data.Set as Set
import qualified Insieme.Analysis.Solver as Solver


data MemoryLocation = MemoryLocation NodeAddress
    deriving (Eq,Ord,Show)

data MemoryState = MemoryState ProgramPoint MemoryLocation
    deriving (Eq,Ord,Show)

memoryStateValue :: (Solver.Lattice a)
         => MemoryState
         -> a
         -> Solver.TypedVar a
memoryStateValue ms top = undefined         


-- reaching definitions

newtype ReachingDefinition = ReachingDefinition NodeAddress
    deriving (Eq,Ord,Show)
    
type ReachingDefinitions = Set.Set ReachingDefinition

reachingDefinitions :: MemoryState -> Solver.TypedVar ReachingDefinitions 
reachingDefinitions (MemoryState pp ml) = undefined


-- killed definitions
    
newtype KilledDefinition = KilledDefinition NodeAddress
    deriving (Eq,Ord,Show)
    
type KilledDefinitions = Set.Set KilledDefinition
    
killedDefinitions :: MemoryState -> Solver.TypedVar KilledDefinitions 
killedDefinitions ms = undefined