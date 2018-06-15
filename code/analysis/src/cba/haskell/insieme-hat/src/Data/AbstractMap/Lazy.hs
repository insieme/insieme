{-# LANGUAGE ConstraintKinds #-}

module Data.AbstractMap.Lazy
    ( module Data.Map.Lazy
    , Map
    , MapKey
    ) where

import Data.Hashable
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy hiding (Map)

import Data.AbstractMap.Orphan

type Map = Map.Map
type MapKey a = (Ord a)
    
