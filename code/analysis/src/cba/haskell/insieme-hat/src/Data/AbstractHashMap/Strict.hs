{-# LANGUAGE ConstraintKinds #-}

module Data.AbstractHashMap.Strict
    ( module Data.HashMap.Strict
    , Map
    , MapKey
    ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict

type Map = HashMap
type MapKey a = (Eq a, Hashable a)
    
