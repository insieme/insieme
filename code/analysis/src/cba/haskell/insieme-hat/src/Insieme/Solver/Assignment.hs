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

import           Data.AbstractSet (Set)
import qualified Data.AbstractSet as Set

import {-# SOURCE #-} Insieme.Solver.Var


import           Insieme.Solver.VarMap (VarMap)
import qualified Insieme.Solver.VarMap as VarMap

import           Insieme.Solver.VariableIndex (IndexedVar)
import qualified Insieme.Solver.VariableIndex as VariableIndex


-- Assignments ----------------------------------------------

newtype Assignment = Assignment (VarMap Dynamic)

-- instance Show Assignment where
--     show a@(Assignment m) = "Assignment {\n\t"
--             ++
--             ( intercalate ",\n\t\t" ( map (\v -> (show v) ++ " #" ++ (show $ hash v) ++ " = " ++ (valuePrint v a) ) vars ) )
--             ++
--             "\n}"
--         where
--             vars = keys m

empty :: Assignment
empty = Assignment VarMap.empty

-- retrieves a value from the assignment
-- if the value is not present, the bottom value of the variable will be returned
get' :: (Typeable a) => Assignment -> TypedVar a -> a
get' (Assignment m) (TypedVar v) =
        fromJust $ (fromDynamic :: ((Typeable a) => Dynamic -> (Maybe a)) ) $ maybeValToBottom v (VarMap.lookup v m)


-- updates the value for the given variable stored within the given assignment
set :: (Typeable a, NFData a) => Assignment -> TypedVar a -> a -> Assignment
set (Assignment a) (TypedVar v) d = Assignment (VarMap.insert v (toDyn d) a)


-- resets the values of the given variables within the given assignment
reset :: Assignment -> Set IndexedVar -> Assignment
reset (Assignment m) vars = Assignment $ VarMap.insertAll reseted m
    where
        reseted = go <$> Set.toList vars
        go iv = (v, bottom v)
            where
                v = VariableIndex.ivVar iv
