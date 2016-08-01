
module Insieme.Analysis.Framework.ProgramPoint where


import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Predecessor
import qualified Insieme.Analysis.Solver as Solver


programPointValue :: (Solver.Lattice a)
         => ProgramPoint                                    -- ^ the program point for which to compute a variable representing a state value
         -> a                                               -- ^ the top value of the lattice
         -> (ProgramPoint -> Solver.Identifier)             -- ^ a variable ID generator function
         -> (ProgramPoint -> Solver.TypedVar a)             -- ^ a variable generator function for referenced variables
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information

programPointValue pp top idGen analysis = var
    where 
        var = Solver.mkVariable (idGen pp) [con] top
        con = Solver.createConstraint dep val var
        
        predecessorVar = predecessor pp
        predecessorStateVars a = map (\p -> analysis p) (Solver.get a predecessorVar) 
        
        dep a = (Solver.toVar predecessorVar) : map Solver.toVar (predecessorStateVars a)
        val a = Solver.join $ map (Solver.get a) (predecessorStateVars a) 