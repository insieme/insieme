{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.AbstractMap.Orphan () where

import Data.Hashable
import Data.Map.Lazy

instance (Hashable a, Hashable b) => Hashable (Map a b) where
    hashWithSalt s = hashWithSalt s . toList
    
