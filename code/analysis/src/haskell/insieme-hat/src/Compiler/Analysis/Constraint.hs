-- | Representation of a constraint. Internally, constraints can be
-- represented in various ways, so this module defines a common
-- interface all implementations have to adhere to.

module Compiler.Analysis.Constraint (
  -- * Constraint creation
  Provider, Constr, constr, Guard, guard,
  -- * Describing updates
  Update(..), update,
  -- * Evaluation of constraints
  preds, evaluate
  ) where

import Compiler.Analysis.Constraint.Dynamic
import Compiler.Analysis.Values
import Data.Maybe (fromMaybe)

-- | A constraint provider is a function that returns a constraint for
-- a given index. If the constraint provider needs to be a 'Data.Map'
-- then using the partial function @(map 'Data.Map.!')@ should satisfy
-- the interface.
type Provider k = k -> Constr k

-- | Return the variables the constraint needs for calculating its
-- value.
preds :: Eq k => Provider k -> k -> [k]
preds = (inputs .)

-- | Replace all 'Nothing' values in an associative list by a default
-- value based upon their keys.
assocMaybe :: (a -> b) -> [(a, Maybe b)] -> [(a, b)]
assocMaybe f = map (\(a, m) -> (a, fromMaybe (f a) m))

-- | Evaluate the given constraint for the current assignments.
evaluate :: Ord k => Provider k -> k -> Assignments k -> Update (Assignments k)
evaluate p k a = if checkeq constr oldval newval
                 then Same a
                 else Steady (insertWith (combine constr) k newval a)
  where
    constr = p k
    oldval = inspectDyn a k
    -- evaluate the constraint after replacing 'Nothing' with 'bottom'
    newval = eval (assocMaybe (bottom . p) digest) constr
    -- map inputs to current values according to the list of assignments
    digest = map (\k -> (k, inspectDyn a k)) (inputs constr)
