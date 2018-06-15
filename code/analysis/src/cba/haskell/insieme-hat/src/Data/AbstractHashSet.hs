{-# LANGUAGE ConstraintKinds #-}

module Data.AbstractHashSet
    ( module Data.HashSet
    , Set
    , SetKey
    ) where

import Data.Hashable
import qualified Data.HashSet as HashSet
import Data.HashSet

type Set = HashSet
type SetKey a = (Eq a, Hashable a)
    
