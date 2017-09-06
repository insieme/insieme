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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Insieme.Analysis.Entities.DataPath (

    Direction(..),
    DataPath(..),
    step,
    narrow,

    append,
    invert

) where

import Control.DeepSeq
import GHC.Generics (Generic)

-- 
-- * Data Path Direction
--

data Direction = Up | Down
  deriving (Eq,Ord,Generic,NFData)

instance Show Direction where
    show Up   = "↑"
    show Down = "↓"



--
-- * DataPaths
--

data DataPath i =
          Root
        | DataPath (DataPath i) Direction i
        | Invalid
  deriving (Eq,Ord,Generic,NFData)


instance (Show i) => Show (DataPath i) where
    show  Root            = "⊥"
    show (DataPath p d s) = (show p) ++ "." ++ (show d) ++ (show s)
    show  Invalid         = "?"



-- creates a single-step data path
step :: i -> DataPath i
step = DataPath Root Down

-- creates multiple steps of a data path
narrow :: [i] -> DataPath i
narrow s = foldl go Root s
  where
    go p i = DataPath p Down i


-- concatenation of paths
append :: (Eq i) => DataPath i -> DataPath i -> DataPath i

-- handle root cases
append Root p = p
append p Root = p

-- handle invalid paths
append Invalid _ = Invalid
append _ Invalid = Invalid

-- let same steps in different directions cancel out
append (DataPath p Up a) (DataPath Root Down b) | a == b = p
append (DataPath p Down a) (DataPath Root Up b) | a == b = p
append (DataPath _ Up _) (DataPath Root Down _) = Invalid  -- TODO: improve this

-- append paths as required
append a (DataPath b d i)    = DataPath (append a b) d i


-- inverts a path's direction. Every up becomes a down and vica versa.
invert :: DataPath i -> DataPath i
invert Root = Root
invert Invalid = Invalid
invert (DataPath p Down i) = DataPath (invert p) Up i
invert (DataPath p Up i) = DataPath (invert p) Down i

