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

module Visit (visitTests) where

import Insieme.Inspire
import Insieme.Inspire.Visit
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils

import Data.List
import Text.Show.Pretty

import Debug.Trace

ex1, ex2 :: (Int -> [Tree] -> Tree) -> Tree
ex1 n = n 10 [ nil n, nil n, nil n ]

ex2 n = n 10 [ n 20 [], n 21 [ nil n ], n 22 [ nil n, nil n ] ]

visitTests =
    testGroup "Visit" $ [
        testGroup "collectAllPrunePaths" $
            let collect  = collectAllPrunePaths (const True) (const NoPrune)
                collect' = collectAllPaths'Naive (const True)

                tests n ex r = [ testCase (n ++ " withoutId") $ do
                                 let n = ex nodeWithoutId
                                 collect n @?= collect' n
                                 collect n @?= r
                             , testCase (n ++ " withId") $ do
                                 let n = ex nodeWithId
                                 collect n @?= collect' n
                                 collect n @?= r
                             ]
            in
            [ testCase "nil withoutId" $ do
                   let n = nil nodeWithoutId
                   collect n @?= collect' n
                   collect n @?= [[]]
            ] ++ tests "ex1" ex1 [[], [0], [1], [2]]
              ++ tests "ex2" ex2 [[], [0], [1], [1,0], [2], [2,0], [2,1]]
      ]
