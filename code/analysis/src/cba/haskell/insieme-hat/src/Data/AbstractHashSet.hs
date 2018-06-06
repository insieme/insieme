{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.AbstractHashSet
    ( module Data.HashSet
    , Set
    , SetKey
    ) where

import Data.Hashable
import qualified Data.HashSet as HashSet
import Data.HashSet
import Data.Function

type Set = HashSet
type SetKey a = (Eq a, Hashable a)

instance Ord a => Ord (HashSet a) where
    compare = compare `on` toList
    
