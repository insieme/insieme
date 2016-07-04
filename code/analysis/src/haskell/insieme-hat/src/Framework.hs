module Framework where

import Compiler.Analysis
import Data.Typeable

resolve' :: Typeable a => AVar -> a -> a
resolve' v d = inspectDefault as v d
  where
    an = using genprov
    fp = solve [v] an
    as = results fp

data AVar = AVar Int (() -> Constr AVar)

instance Ord AVar where
    (AVar x _) <= (AVar y _) = x <= y

instance Eq AVar where
    (AVar x _) == (AVar y _ ) = x == y

instance Show AVar where
    show (AVar x _) = show x

genprov :: AVar -> Constr AVar
genprov (AVar _ f) = f ()
