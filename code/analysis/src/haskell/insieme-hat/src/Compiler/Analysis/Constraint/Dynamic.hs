{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Implementation of constraints using the 'Dynamic' data type.

module Compiler.Analysis.Constraint.Dynamic (
  -- * Constraint creation
  Constr, constr, Guard, guard,
  -- * Using and evaluating constraints
  inputs, eval, checkeq, bottom, combine
  ) where

import qualified Compiler.Analysis.Values as Values (Lattice, bot, join)
import Data.Dynamic
import Data.List (union, nub)
import Data.Maybe (fromMaybe)

-- | Final return value of a function, expressed by a closed type
-- family.
type family Returns (f :: *) :: * where
  Returns (a -> b) = Returns b
  Returns r = r

-- | A @Guard@ is a function that evaluates to 'Bool' given some
-- inputs. These inputs are stored in an environment and indexed by
-- an 'Ord' value.
data Guard k = Guard [k] Dynamic

-- | Type-agnostic representation of a constraint. The type parameter
-- is used to identify the constraint from other constraints, and can
-- be basically any type where an ordering 'Ord' can be defined.
data Constr k = Constr
  { grd :: Maybe (Guard k)   -- ^ guard determines whether this constraint fires
  , dep :: [k]       -- ^ analysis variables this constraint depends upon
  , upd :: Dynamic   -- ^ function to return a new 'Lattice' based on some vars
  , eqf :: Dynamic   -- ^ function to compare two 'Lattice' vals with each other
  , bot :: Dynamic   -- ^ bottom value to read if 'Assignments' yields 'Nothing'
  , djn :: Dynamic   -- ^ join two 'Lattice' values (dynamic join)
  }

-- | Returns the bottom element for a constraint.
bottom = bot

-- | Combine two 'Dynamic' values with the 'join' function associated
-- with the given constraint.
combine = combine' . djn

-- | @guard args pred@ creates a 'Guard' data structure from a
-- predicate @pred@ and its arguments @args@.
guard :: (Typeable g, Returns g ~ Bool) => [k] -> g -> Guard k
guard = ( . toDyn) . Guard

-- | This function allows to specify a constraint definition by giving
-- its dependences (analysis variables it expects as arguments) and a
-- function which calculates a new 'Lattice' value using these
-- variables. This function should be monomorphic and variadic with as
-- many arguments as there are elements in the dependence vector. The
-- optional 'Guard' specifies whether the constraint needs to trigger
-- (return value 'True', default), or not (return value 'False').
constr :: (Typeable c, Returns c ~ r, Typeable r, Values.Lattice r) =>
         Maybe (Guard k)   -- ^ optional guard determines when constraint fires
       -> [k]      -- ^ dependences: 'Lattice' values to feed to update function
       -> c        -- ^ update function, needs to return a 'Lattice'
       -> Constr k -- ^ the resulting constraint
constr g ks c = Constr g ks (toDyn c)
                (toDyn $ mkeq c) (toDyn $ mkbot c) (toDyn $ mkjoin c)

-- | Given an arbitrary function return a monomorphic function which
-- is able to compare two return values of the input function.
mkeq :: (Returns a ~ b, Eq b) => a -> (b -> b -> Bool)
mkeq _ = (==)

-- | Given a function that returns a 'Lattice' value return the bottom
-- element of this lattice.
mkbot :: (Returns a ~ b, Values.Lattice b) => a -> b
mkbot _ = Values.bot

-- | Given a function that returns a 'Lattice' value return the join
-- function for this lattice.
mkjoin :: (Returns a ~ b, Values.Lattice b) => a -> b -> b -> b
mkjoin _ = Values.join

-- | Return the input values that are needed for evaluating a
-- constraint.
inputs :: Eq k => Constr k -> [k]
inputs k = nub $ union (inC k) (maybe [] inG (grd k))
  where
    inG (Guard p _) = p
    inC = dep

-- | @combine' f a b@ applies two dynamic arguments @a, b@ to a dynamic
-- function @f@.
combine' :: Dynamic -> Dynamic -> Dynamic -> Maybe Dynamic
combine' f a b = dynApply f a >>= flip dynApply b

-- | Evaluates the constraint under the given input, and return 'Just'
-- a 'Dynamic' representation of the calculated 'Lattice' value if
-- successful, 'Nothing' otherwise.
eval :: Ord k => [(k, Dynamic)] -> Constr k -> Maybe Dynamic
eval a c = if (maybe True (evalGuard a) (grd c))
           then evalConstr a c else Nothing

-- | Runs a guard, which yields a 'Bool' value for the given
-- 'Assignments'.
evalGuard :: Ord k => [(k, Dynamic)] -> Guard k -> Bool
evalGuard a (Guard ps gf) = fromMaybe True $   -- in case of error, eval Constr
  applyD (Just gf) (map (flip lookup a) ps) >>= fromDynamic

-- | Runs a constraint to get a new 'Lattice' value. If all of the
-- arguments were correct in their types (according to the
-- constraint's update function) then the return value is 'Just' a
-- 'Dynamic' representation of the 'Lattice' value which can be
-- retrieved with 'unwrapD'. In case of an error, 'Nothing' is
-- returned.
evalConstr :: Ord k => [(k, Dynamic)] -> Constr k -> Maybe Dynamic
evalConstr a c = applyD (Just $ upd c) $ map (flip lookup a) (dep c)

-- | Checks the equality of two 'Lattice' values. Returns 'True' only
-- when results match in type and value.
checkeq :: Constr k -> Maybe Dynamic -> Maybe Dynamic -> Bool
checkeq c n1 n2 =
  fromMaybe False $ applyD (Just $ eqf c) [n1, n2] >>= fromDynamic

-- | Lifts a value into the 'Maybe' 'Dynamic' context. See 'pure'.
pureD :: (Typeable a) => a -> Maybe Dynamic
pureD = Just . toDyn

-- | Without phantom types 'Maybe' 'Dynamic' cannot be made into a
-- 'Functor' but we can map over such a value nevertheless.
fmapD :: (Typeable a, Typeable b) => (a -> b) -> Maybe Dynamic -> Maybe Dynamic
fmapD = maybe Nothing . dynApply . toDyn

-- | Apply a single argument to a function. Just like 'ap' applies a
-- function in a monad context to a value in a monad context, @apD@
-- applies a function in a 'Maybe' 'Dynamic' context to a value in the
-- same context. Because 'dynApply' wraps its result in a 'Maybe'
-- context, we expect the input values to 'apD' in the 'Maybe'
-- 'Dynamic' context as well so that we can symmetrically compose
-- function application.
apD :: Maybe Dynamic -> Maybe Dynamic -> Maybe Dynamic
apD df dv = df >>= \f -> dv >>= \v -> dynApply f v

-- | Apply multiple dynamic arguments to a dynamic function. See 'apD'.
applyD :: Maybe Dynamic -> [Maybe Dynamic] -> Maybe Dynamic
applyD = Prelude.foldl apD

-- | Try to extract a particular type from a 'Maybe' 'Dynamic' context.
unwrapD :: Typeable a => Maybe Dynamic -> Maybe a
unwrapD = (fromDynamic =<<)
