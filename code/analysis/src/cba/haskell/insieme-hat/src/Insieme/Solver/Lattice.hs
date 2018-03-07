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

module Insieme.Solver.Lattice where

import Prelude hiding (lookup,print)
import Control.DeepSeq
import Data.Typeable

-- Lattice --------------------------------------------------

-- this is actually a bound join-semilattice
class (Eq v, Show v, Typeable v, NFData v) => Lattice v where
        {-# MINIMAL join | merge, bot #-}
        -- | combine elements. a default implementation for the join operator,
        join :: [v] -> v                    
        join [] = bot
        join xs = foldr1 merge xs

        -- | binary version of the join, its default implementation derived from
        -- join.
        merge :: v -> v -> v  
        merge a b = join [ a , b ]          

        -- | bottom element of the join.
        bot :: v
        bot = join []

        -- | induced order, determines whether one element of the lattice is
        -- less than another
        less :: v -> v -> Bool
        less a b = (a `merge` b) == b

        -- | debug printing, print a value of the lattice readable
        print :: v -> String
        print = show

class (Lattice v) => ExtLattice v where
        -- | top element of this lattice
        top  :: v
