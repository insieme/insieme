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

module Insieme.Analysis.Solver.Var 
    ( Var
    , varIdent
    , constraints
    , bottom
    , valuePrint
    , maybeValToBottom
    , mkVariable

    , TypedVar(..)
    , toVar
    ) where


import Prelude hiding (lookup,print)
import Data.Dynamic

import           Data.Hashable (Hashable)
import qualified Data.Hashable as Hash

import Insieme.Analysis.Solver.Constraint
import Insieme.Analysis.Solver.Identifier
import Insieme.Analysis.Solver.Assignment
import Insieme.Analysis.Solver.Lattice

-- * Analysis Variables ---------------------------------------

-- | General variables (management)
data Var = Var 
    { _varIdent    :: Identifier
        -- ^ the variable identifier
    , _constraints :: [Constraint]
        -- ^ the list of constraints
    , _bottom     :: Dynamic              
        -- ^ the bottom value for this variable
    , _valuePrint  :: Assignment -> String
        -- ^ a utility for unpacking an printing a value assigned to this
        -- variable
    }

instance Eq Var where
        (==) a b = (_varIdent a) == (_varIdent b)

instance Ord Var where
        compare a b = compare (_varIdent a) (_varIdent b)

instance Show Var where
        show v = show (_varIdent v)

instance Hashable Var where
    hashWithSalt s Var {_varIdent} = Hash.hashWithSalt s _varIdent
    hash Var {_varIdent} = Hash.hash _varIdent

-- | typed variables (user interaction)
newtype TypedVar a = TypedVar Var
    deriving (Show, Eq, Ord)

varIdent    :: Var -> Identifier
varIdent = _varIdent

constraints :: Var -> [Constraint]
constraints = _constraints

bottom      :: Var -> Dynamic
bottom = _bottom

valuePrint  :: Var -> Assignment -> String
valuePrint = _valuePrint

-- showVarWithValue :: Var -> String
-- showVarWithValue v = show v ++ " #" ++ show (hash v) ++ " = " ++ valuePrint v a

maybeValToBottom :: Var -> Maybe Dynamic -> Dynamic
maybeValToBottom v Nothing  = _bottom v
maybeValToBottom _ (Just v) = v


mkVariable :: Lattice a => Identifier -> [Constraint] -> a -> TypedVar a
mkVariable i cs b = var
    where
        var = TypedVar (Var i cs (toDyn b) print')
        print' = (\a -> print $ get' a var)

toVar :: TypedVar a -> Var
toVar (TypedVar x) = x
