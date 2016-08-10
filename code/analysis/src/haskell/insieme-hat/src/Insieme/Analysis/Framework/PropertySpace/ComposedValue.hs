{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Insieme.Analysis.Framework.PropertySpace.ComposedValue where

import Data.Typeable
import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Entities.FieldIndex
import qualified Insieme.Analysis.Solver as Solver


class (Solver.Lattice c, FieldIndex i, Solver.Lattice v) => ComposedValue c i v | c -> i v where

    toComposed :: v -> c
    toValue    :: c -> v
    
    setElement :: DataPath i -> c -> c -> c
    getElement :: DataPath i -> c -> c
     
    composeElements :: [(i,c)] -> c
    
