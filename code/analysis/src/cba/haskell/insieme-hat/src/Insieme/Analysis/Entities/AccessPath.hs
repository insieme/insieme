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

module Insieme.Analysis.Entities.AccessPath (

    BaseVar(..),

    AccessPath(..),
    unknown,
    parameter,
    global,
    local,

    append,
    deref,
    extend

) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Insieme.Analysis.Entities.FieldIndex
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Inspire as IR

--
-- * AccessPaths
--

data BaseVar =
          Parameter Int
        | Global IR.Tree
    deriving (Eq,Ord,Generic,NFData)

instance Show BaseVar where
    show (Parameter i) = "p" ++ (show i)
    show (Global n)    = show n

data AccessPath i =
          AccessPath BaseVar [DP.DataPath i]
        | Local
        | Unknown
    deriving (Eq,Ord,Generic,NFData)

instance (Show i) => Show (AccessPath i) where
    show (AccessPath b s) = (show b) ++ (concat $ (++ ".*") . tail . show <$> reverse s) ++ ")"
    show  Local           = "local"
    show  Unknown         = "?"


-- constructors for access paths

unknown :: AccessPath i
unknown = Unknown

parameter :: Int -> AccessPath i
parameter i = AccessPath (Parameter i) [DP.Root]

global :: IR.Tree -> AccessPath i
global g = AccessPath (Global g) [DP.Root]

local :: AccessPath i
local = Local



-- manipulation

append :: (FieldIndex i) => AccessPath i -> DP.DataPath i -> AccessPath i
append Unknown    _ = Unknown
append Local      _ = Local
append _ DP.Invalid = Unknown
append (AccessPath _ []    ) _ = error "empty access path"
append (AccessPath b (d:ds)) p = case DP.append d p of
    DP.Invalid -> Unknown
    r@_        -> AccessPath b (r:ds)


deref :: AccessPath i -> AccessPath i
deref  Unknown          = Unknown
deref  Local            = Local
deref (AccessPath b ds) = AccessPath b (DP.Root:ds)


extend :: AccessPath i -> AccessPath i -> AccessPath i
extend Unknown _ = Unknown
extend _ Unknown = Unknown
extend Local   _ = Local
extend _   Local = Local
extend (AccessPath v a) (AccessPath _ b) = AccessPath v (a ++ (tail b))
