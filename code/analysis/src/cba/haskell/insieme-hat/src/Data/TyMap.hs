{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}

module Data.TyMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dynamic
import Data.Type.Equality
import Control.Monad
import Type.Reflection
import Prelude hiding (lookup)

data TyMap where
    Empty :: TyMap
    Node  :: TypeRep a -> a -> TyMap -> TyMap -> TyMap

empty :: TyMap
empty = Empty

lookup :: TypeRep a -> TyMap -> Maybe a
lookup _ Empty = Nothing
lookup rep (Node rep' a l r) =
    case compare (SomeTypeRep rep) (SomeTypeRep rep') of
      LT -> lookup rep l
      GT -> lookup rep r
      EQ -> case testEquality rep rep' of
              Just Refl -> Just a
              Nothing -> Nothing -- unreachable

insert :: TypeRep a -> a -> TyMap -> TyMap
insert = insertWith (\new _old -> new)

insertWith :: forall a. (a -> a -> a) -> TypeRep a -> a -> TyMap -> TyMap
insertWith _ rep a Empty = Node rep a Empty Empty
insertWith f rep a n@(Node rep' a' l r) =
    case compare (SomeTypeRep rep) (SomeTypeRep rep') of
      LT -> Node rep' a' (insertWith f rep a l) r
      GT -> Node rep' a' l (insertWith f rep a r)
      EQ | Just Refl <- rep `testEquality` rep' -> Node rep' (f a a') l r
      EQ -> n -- unreachable
