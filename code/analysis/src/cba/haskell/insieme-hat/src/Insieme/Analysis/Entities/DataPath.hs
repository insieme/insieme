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

    DataPath(Root,Invalid),
    root,
    invalid,
    step,
    narrow,
    expand,

    isRoot,
    isInvalid,
    isNarrow,
    isExpand,
    getPath,

    append,
    invert

) where

import Control.DeepSeq
import Data.List
import GHC.Generics (Generic)

--
-- * DataPaths
--


data DataPath i =
          Root
        | Narrow [i]
        | Expand [i]
        | Invalid
  deriving (Eq,Ord,Generic,NFData)


instance (Show i) => Show (DataPath i) where
    show  Root      = "⊥"
    show (Narrow p) = "⊥." ++ (intercalate "." $ show <$> (reverse p))
    show (Expand p) = (intercalate "." $ show <$> p) ++ ".⊥"
    show  Invalid   = "?"


-- a token for an empty path
root :: DataPath i
root = Root

-- a token for an invalid path
invalid :: DataPath i
invalid = Invalid


-- creates a single-step data path
step :: i -> DataPath i
step i = Narrow [i]


narrow :: [i] -> DataPath i
narrow [] = Root
narrow p  = Narrow p


expand :: [i] -> DataPath i
expand [] = Root
expand p  = Expand p


isRoot :: DataPath i -> Bool
isRoot Root = True
isRoot _    = False


isInvalid :: DataPath i -> Bool
isInvalid Invalid = True
isInvalid _       = False

isNarrow :: DataPath i -> Bool
isNarrow  Root      = True
isNarrow (Narrow _) = True
isNarrow  _         = False

isExpand :: DataPath i -> Bool
isExpand  Root      = True
isExpand (Expand _) = True
isExpand  _         = False


getPath :: DataPath i -> [i]
getPath  Root      = []
getPath (Narrow p) = p
getPath (Expand p) = p
getPath Invalid    = error "invalid path"


-- concatenation of paths
append :: (Eq i) => DataPath i -> DataPath i -> DataPath i
append a Root = a
append Root a = a

append Invalid _ = Invalid
append _ Invalid = Invalid

append (Narrow a) (Narrow b) = Narrow (b ++ a)
append (Expand a) (Expand b) = Expand (b ++ a)

append (Narrow [x]) (Expand [y])       | x == y = Root
append (Narrow (x:xs)) (Expand [y])    | x == y = Narrow xs
append (Narrow (x:xs)) (Expand (y:ys)) | x == y = append (Narrow xs) (Expand ys)
append (Narrow _)      (Expand _)               = Invalid

append a@(Expand _) b@(Narrow _) = invert $ append (invert a) (invert b)



-- inverts a path from narrow to expand and vica versa
invert :: DataPath i -> DataPath i
invert  Root      = Root
invert  Invalid   = Invalid
invert (Narrow p) = Expand p
invert (Expand p) = Narrow p
