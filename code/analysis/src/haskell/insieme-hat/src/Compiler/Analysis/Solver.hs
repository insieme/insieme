{-# LANGUAGE FlexibleContexts #-}

-- | Solve constraints using a maximum fixed point solver.

module Compiler.Analysis.Solver (
  -- * Creating and solving an analysis
  Analysis, solve, using,
  -- * Value lookup
  results
  ) where

import Compiler.Analysis.Constraint as Con hiding (constr)
import Compiler.Analysis.Dependence
import Compiler.Analysis.Values as Val
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- | Opaque data type to represent state information for the maximum
-- fixed point iterative solver.
data Analysis k = Analysis {
  constr :: Provider k,
  assign :: Assignments k,
  revdep :: Map.Map k (Set.Set k),
  wklist :: Set.Set k
  }

-- | Return the possible values for all analysis variables as a map
-- from a label to a 'Dynamic' lattice element. These intermediate or
-- final values of the analysis can then be inspected with 'inspect'
-- or 'inspectDefault'.
results :: Analysis k -> Assignments k
results = assign

-- | Set up an analysis using the given provider.
using :: Provider k -> Analysis k
using = flip (flip (flip Analysis empty) Map.empty) Set.empty

-- | Solve the given constraints and recursively all other constraints
-- that are needed for solving the given ones. However, variables not
-- involved in these constraints will not be evaluated, and so
-- multiple calls to this function could be necessary, depending on
-- the application. Internally, this algorithm uses a maximum fixed
-- point algorithm with a work list and will terminate once the work
-- list is empty.
solve :: Ord k => [k] -> Analysis k -> Analysis k
solve ks = (iterateWhile pick solveStep) . init
  where init a = let (r, w) = invert ks (preds . constr $ a)
                 in a { revdep = r, wklist = w }

-- | Repeatedly apply the function @f@ to the value @v@ until the
-- predicate @p@ returns 'Nothing'. This value will then be returned.
iterateWhile :: (a -> Maybe b) -> (b -> a -> a) -> a -> a
iterateWhile p f v = maybe v (\i -> iterateWhile p f (f i v)) (p v)

-- | Take a single element from the worklist and update the Analysis
-- accordingly. Additionally, put some new elements into the worklist
-- if necessary.
solveStep :: Ord k => k -> Analysis k -> Analysis k
solveStep k a =
  let ks = Set.delete k (wklist a)
  in case evaluate (constr a) k (assign a) of
      Same    _ -> a { wklist = ks }       -- analysis just got smaller
      Steady  r -> a { assign = r, wklist = progress k (revdep a) ks }
      Erratic r -> a { assign = r, wklist = progress k (revdep a) ks }

-- | If available, pick a single work item from the work list.
pick :: Analysis k -> Maybe k
pick = (\s -> if null s then Nothing else Just $ Set.findMax s) . wklist

-- | Given an element, a work list and a map of dependences, make
-- progress by adding dependences of the distinct element to the work
-- list.
progress :: Ord k => k -> Map.Map k (Set.Set k) -> Set.Set k -> Set.Set k
progress k rdep m = fromMaybe m (Map.lookup k rdep >>= return . (Set.union m))
