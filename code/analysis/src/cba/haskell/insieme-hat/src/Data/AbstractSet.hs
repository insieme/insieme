{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.AbstractSet
    ( module Data.Set
    , Set
    , SetKey
    ) where

import Data.Hashable
import qualified Data.Set as Set
import Data.Set hiding (Set)

type Set = Set.Set
type SetKey a = (Ord a)    

instance (Hashable a) => Hashable (Set.Set a) where
    hashWithSalt s = hashWithSalt s . Set.toList
