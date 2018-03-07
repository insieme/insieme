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

{-# LANGUAGE NamedFieldPuns #-}

module Insieme.Analysis.Solver.VariableIndex
    ( IndexedVar
    , ivIndex
    , ivVar

    , VariableIndex
    , empty
    , numVars
    , knownVariables
    , varToIndexedVar
    , varsToIndexedVars
    ) where

import Data.Maybe

import qualified Data.Set as Set

import {-# SOURCE #-} Insieme.Analysis.Solver.Var

import           Insieme.Analysis.Solver.VarMap (VarMap)
import qualified Insieme.Analysis.Solver.VarMap as VarMap

-- Variable Index -----------------------------------------------

-- a utility for indexing variables
data IndexedVar = IndexedVar
    { _ivIndex :: !Int
    , _ivVar   :: !Var
    }

instance Eq IndexedVar where
    IndexedVar {_ivIndex=a} == IndexedVar {_ivIndex=b} = a == b

instance Ord IndexedVar where
    compare IndexedVar {_ivIndex=a} IndexedVar {_ivIndex=b} = compare a b

instance Show IndexedVar where
    show IndexedVar {_ivVar} = show _ivVar


ivIndex :: IndexedVar -> Int
ivIndex IndexedVar {_ivIndex} = _ivIndex

ivVar :: IndexedVar -> Var
ivVar IndexedVar {_ivVar} = _ivVar


data VariableIndex = VariableIndex Int (VarMap IndexedVar)

empty :: VariableIndex
empty = VariableIndex 0 VarMap.empty

numVars :: VariableIndex -> Int
numVars (VariableIndex n _) = n

knownVariables :: VariableIndex -> Set.Set Var
knownVariables (VariableIndex _ m) = VarMap.keysSet m

varToIndexedVar :: VariableIndex -> Var -> (IndexedVar, VariableIndex)
varToIndexedVar (VariableIndex n m) v = (res, VariableIndex nn nm)
    where

        ri = VarMap.lookup v m

        nm = if isNothing ri then VarMap.insert v ni m else m

        ni = IndexedVar n v           -- the new indexed variable, if necessary

        res = fromMaybe ni ri

        nn = if isNothing ri then n+1 else n

varsToIndexedVars :: VariableIndex -> [Var] -> ([IndexedVar], VariableIndex)
varsToIndexedVars i vs = foldr go ([],i) vs
    where
        go v (rs,i') = (r:rs,i'')
            where
                (r,i'') = varToIndexedVar i' v
