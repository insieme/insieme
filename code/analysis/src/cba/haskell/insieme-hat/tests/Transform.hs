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

{-# LANGUAGE ViewPatterns #-}

module Transform (transformTests) where

import Insieme.Inspire.IR.Transform
import Insieme.Inspire.IR.Tree
import Insieme.Inspire.IR.NodeType
import Test.Tasty
import Test.Tasty.HUnit
import TreeUtils

import Text.Show.Pretty
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe


ex1, ex1_disj, ex1_subst :: ([Tree] -> Tree) -> Tree
ex1 n = n [ nil n, nil n, nil n ]
ex1_disj n = n [ nil n, nil n, nil n, nil n ]
ex1_subst n =
    nodeWithBuiltinTags [ ex1 n, ex1 n, ex1 n ] []

-- | Singleton substitution
a |-> b = HashMap.fromList [(a,b)]

transformTests =
    testGroup "Transform" $ concat
        [ map (identityTests node)
                  [ ("ex1", ex1, ex1_disj)
                  ]
        , [ testCase "substituting root works" $
                substitute (ex1 node |-> nil node) (ex1 node) @?~ nil node
          , testCase "substituting childs works" $
                substitute (node [] |-> ex1 node) (ex1 node)
                    @?~ (ex1_subst node)
          , testCase "doesn't substitute in replacement" $ let n = node in
                substitute (n [] |-> n [ n [] ]) (n []) @?~ n [ n [] ]
          , testCase "removes builtinTags on change" $
                let n = node
                    n0 = flip nodeWithBuiltinTags ["foo", "bar"]
                    n1 = flip nodeWithBuiltinTags [] in
                substitute (n [] |-> n [n []]) (n0 [n [], n []]) @?~
                    (n1 [n [n []], n [n []]])

          ]
        ]

identityTests n (msg, ex, disj) = testGroup ("identity tests with "++msg)
    [ testCase "Empty subst yields identity" $
          substitute HashMap.empty (ex n) @?~ ex n
    , testCase "Identity susbt yields identity" $
          substitute (nil n |-> nil n) (ex n) @?~ ex n
    , testCase "Disjunct subst yields identity" $
          substitute (disj n |-> nil n) (ex n) @?~ ex n
    , testCase "Disjunct subst yields exact identity" $
          substitute (disj n |-> nil n) (ex n) @?=== ex n
    ]

