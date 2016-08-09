{-# LANGUAGE MultiParamTypeClasses #-}

module Insieme.Analysis.Framework.PropertySpace.ComposedValue where

import Insieme.Analysis.Arithmetic
import qualified Insieme.Analysis.Solver as Solver


class (Solver.Lattice (c v), Solver.Lattice v) => ComposedValue c v where

    toComposed :: v -> c v
    toValue    :: c v -> v
    
    composeFields :: [(String,c v)] -> c v
    accessField   :: String -> c v -> c v
    
    setIndex :: SymbolicFormula -> c v -> c v -> c v
    getIndex :: SymbolicFormula -> c v -> c v 