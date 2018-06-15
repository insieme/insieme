{-# LANGUAGE ConstraintKinds #-}

module Data.AbstractMap.Strict
    ( module Data.Map.Strict
    , Map
    , MapKey
    ) where

import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Map.Strict hiding (Map)

import Data.AbstractMap.Orphan

type Map = Map.Map
type MapKey a = (Ord a)
    
