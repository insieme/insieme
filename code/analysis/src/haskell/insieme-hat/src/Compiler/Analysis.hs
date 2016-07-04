-- | An analysis based on this framework can be broken down into the
-- following steps:
--
-- 1. Define constraints (analysis dependent). This is being done
--    using the function 'constr' which takes a single constraint
--    and the identifiers of other constraints it depends upon. How
--    these constraints are identified is up to the user.
--
-- 2. Collect these constraints in a Constraint 'Provider'. A
--    constraint provider is a function (or a map that is turned into
--    a function by partial function application with 'Map.!') that
--    returns a requested constraint.
--
-- 3. Call the solver with target constraints. This is done with the
--    functions 'solve' and 'using'. The latter function turns the
--    constraint provider into an 'Analysis' data type which is then
--    used to solve the analysis variables we are interested in
--    ("target constraints").
--
-- 4. Extract the 'results' from the analysis, then inspect individual
--    variables using 'inspect' or 'inspectDefault'.


module Compiler.Analysis (
  -- * Setting up the analysis
  V.Lattice(..), C.Constr, C.constr, C.Guard, C.guard, C.Provider,
  -- * Solving constraints
  S.using, S.Analysis, S.solve,
  -- * Retrieving analysis results
  V.Assignments, S.results, V.inspect, V.inspectDefault
  ) where

import Compiler.Analysis.Constraint as C
import Compiler.Analysis.Solver     as S
import Compiler.Analysis.Values     as V
