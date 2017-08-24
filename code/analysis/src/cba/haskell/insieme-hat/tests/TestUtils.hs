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

module TestUtils where

import Insieme.Inspire
import Insieme.Inspire.Transform
import Test.Tasty
import Test.Tasty.HUnit

import Text.Show.Pretty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

nil n = n 0 []

nodeType = IntValue 0

node ch = MkTree Nothing (InnerTree nodeType ch ["dummy"])

nodeWithoutId, nodeWithId :: Int -> [Tree] -> Tree
nodeWithoutId _id ch = MkTree Nothing (InnerTree nodeType ch ["dummy"])
nodeWithId     id ch = MkTree (Just id) (InnerTree nodeType ch ["dummy"])

nodeWithBuiltinTags bt ch = MkTree Nothing (InnerTree nodeType ch bt)

-- | Assert node equality modulo IDs using the ordinary Eq instance
a @?~ b = (a == b) @?
    "expected:\n"++simplShow 0 b++"\ngot:\n"++simplShow 0 a++"\n"++
    "expected:\n"++ppShow b++"\ngot:\n"++ppShow a++"\n"

-- | Assert exact node equality including IDs
a @?=== b = (treeExactEq a b) @?
    "expected:\n"++simplShow 0 b++"\ngot:\n"++simplShow 0 a++"\n"++
    "expected:\n"++ppShow b++"\ngot:\n"++ppShow a++"\n"

-- | Show a simplified graphical representation of a node
simplShow d (Tree i _ ch bt) =
    "|"++replicate (4*d) ' ' ++ "o "++ maybe "" id (show <$> i) ++ " " ++ (case bt of ["dummy"] -> ""; _ -> show bt) ++ "\n" ++
    (concat $ map (simplShow (d+1)) ch)
