{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Insieme.Solver.Assignment 
    ( Assignment(..)
    , empty
    , get'
    , set
    , reset
    ) where

import Control.DeepSeq
import Data.Dynamic
import Data.Maybe
import Type.Reflection

import           Data.AbstractSet (Set)
import qualified Data.AbstractSet as Set

import           Data.AbstractMap (Map, MapKey)
import qualified Data.AbstractMap as Map

import           Data.TyMap (TyMap)
import qualified Data.TyMap as TyMap

import {-# SOURCE #-} Insieme.Solver.Var

import           Insieme.Solver.VarMap (VarMap)
import qualified Insieme.Solver.VarMap as VarMap

import           Insieme.Solver.VariableIndex (IndexedVar)
import qualified Insieme.Solver.VariableIndex as VariableIndex


-- Assignments ----------------------------------------------

data Assignment = Assignment TyMap

-- instance Show Assignment where
--     show a@(Assignment m) = "Assignment {\n\t"
--             ++
--             ( intercalate ",\n\t\t" ( map (\v -> (show v) ++ " #" ++ (show $ hash v) ++ " = " ++ (valuePrint v a) ) vars ) )
--             ++
--             "\n}"
--         where
--             vars = keys m

empty :: Assignment
empty = Assignment TyMap.empty

-- | retrieves a value from the assignment
--
-- if the value is not present, the bottom value of the variable will be returned
get' :: forall a. Typeable a => Assignment -> TypedVar a -> a
get' (Assignment m) (TypedVar v) = maybeValToBottom v $ do
  let rep = typeRep @(VarMap a)
  vm <- TyMap.lookup rep m
  VarMap.lookup v vm

-- updates the value for the given variable stored within the given assignment
set :: forall a. Typeable a => Assignment -> TypedVar a -> a -> Assignment
set (Assignment m) (TypedVar v) d = Assignment $
    let rep = typeRep @(VarMap a) in
    TyMap.insertWith VarMap.union rep (VarMap.singleton v d) m

-- resets the values of the given variables within the given assignment
reset :: Assignment -> Set IndexedVar -> Assignment
reset (Assignment m) vars = Assignment $ foldr go m botVars

  where 
    go :: (Var, Dynamic) -> TyMap -> TyMap
    go (v, Dynamic rep a) =
        TyMap.insertWith VarMap.union (App (typeRep @VarMap) rep) (VarMap.singleton v a)

    botVars :: [(Var, Dynamic)]
    botVars = map go $ Set.toList vars
      where
        go iv = (v, bottom v)
          where
            v = VariableIndex.ivVar iv
