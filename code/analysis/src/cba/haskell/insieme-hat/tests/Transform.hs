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

module Transform (transformTests) where

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

ex1, ex1_disj, ex1_subst :: (Int -> [Tree] -> Tree) -> Tree
ex1 n = n 10 [ nil n, nil n, nil n ]
ex1_disj n = n 20 [ nil n, nil n, nil n, nil n ]
ex1_subst n = 
    nodeWithBuiltinTags [] [ ex1 n, ex1 n, ex1 n ]

transformTests = 
    testGroup "Transform" $ concat
        [ map (identityTests nodeWithId)
                  [ ("ex1", ex1, ex1_disj)
                  ]
        , [ testCase "substituting root works" $ 
                substitute (ex1 nodeWithId |-> nil') (ex1 nodeWithId) @?~ nil'
          , testCase "substituting childs works" $ 
                substitute (node [] |-> ex1 nodeWithId) (ex1 nodeWithId)
                    @?~ (ex1_subst nodeWithId)
          , testCase "doesn't loop forever" $ let n = node in
                substitute (n [] |-> n [ n [] ]) (n []) @?~ n [ n [] ]
          , testCase "doesn't destroy sharing" $ let n = node in
                substitute (ex1 nodeWithId |-> ex1 nodeWithId) (ex1 nodeWithId) @?=== ex1 nodeWithId

          , testCase "removes builtinTags on change" $
                let n = node
                    n0 = nodeWithBuiltinTags ["foo", "bar"]
                    n1 = nodeWithBuiltinTags [] in
                substitute (n [] |-> n [n []]) (n0 [n [], n []]) @?===
                    (n1 [n [n []], n [n []]])

          ]
        ]
 where
   nil' = nil nodeWithId

identityTests n (msg, ex, disj) = testGroup ("identity tests with "++msg)
    [ testCase "Empty subst yields identity" $ 
          substitute Map.empty (ex n) @?~ ex n
    , testCase "Empty subst yields exact identity" $
          substitute Map.empty (ex n) @?=== ex n

    , testCase "Identity susbt yields identity" $ 
          substitute (nil n |-> nil n) (ex n) @?~ ex n
    , testCase "Identity susbt yields exact identity" $
          substitute (nil n |-> nil n) (ex n) @?=== ex n


    , testCase "Disjunct subst yields identity" $ 
          substitute (disj n |-> nil n) (ex n) @?~ ex n
    , testCase "Disjunct subst yields exact identity" $
          substitute (disj n |-> nil n) (ex n) @?=== ex n


    , testCase "Disjunct subst withoutId yields identity" $
          substitute (disj nodeWithoutId |-> nil n) (ex n) @?~ ex n
    , testCase "Disjunct subst on withoutId yields identity" $
          substitute (disj n |-> nil n) (ex nodeWithoutId) @?~ ex nodeWithoutId
    , testCase "Disjunct subst on withoutId yields identity" $
          let n = nodeWithoutId in
          substitute (disj n |-> nil n) (ex n) @?~ ex n
    ]


a |-> b = Map.fromList [(a,b)]

a @?~ b = (a == b) @? 
    "expected:\n"++simplShow 0 b++"\ngot:\n"++simplShow 0 a++"\n"++
    "expected:\n"++ppShow b++"\ngot:\n"++ppShow a++"\n"

a @?=== b = (treeExactEq a b) @?
    "expected:\n"++simplShow 0 b++"\ngot:\n"++simplShow 0 a++"\n"++
    "expected:\n"++ppShow b++"\ngot:\n"++ppShow a++"\n"

simplShow d (Tree i _ ch bt) =
    "|"++replicate (4*d) ' ' ++ "o "++ maybe "" id (show <$> i) ++ " " ++ (case bt of ["dummy"] -> ""; _ -> show bt) ++ "\n" ++
    (concat $ map (simplShow (d+1)) ch)
