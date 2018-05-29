{-# LANGUAGE ConstraintKinds #-}

module Data.AbstractHashMap.Lazy
    ( module Data.HashMap.Lazy
    , Map
    , MapKey
    ) where

import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy

type Map = HashMap
type MapKey a = (Eq a, Hashable a)
    
