{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Insieme.Analysis.Framework.PropertySpace.ComposedValue where

import Data.Typeable
import Insieme.Analysis.Entities.SymbolicFormula
import qualified Insieme.Analysis.Solver as Solver


class (Solver.Lattice c, Solver.Lattice v) => ComposedValue c v | c -> v where

    toComposed :: v -> c
    toValue    :: c -> v
    
    composeFields :: [(String,c)] -> c
    accessField   :: String -> c -> c
    
    setIndex :: SymbolicFormula -> c -> c -> c
    getIndex :: SymbolicFormula -> c -> c 