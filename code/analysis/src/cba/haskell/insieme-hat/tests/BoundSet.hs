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

module BoundSet (boundSetTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Insieme.Utils.BoundSet as BSet

boundSetTests = testGroup "BoundSet"
    [ testCase "empty"         $ BSet.empty @?= b0
    , testCase "singleton"     $ BSet.singleton 1 @?= b1
    , testCase "toList"        $ BSet.toList b5 @?= [1..5]
    , testCase "member T"      $ BSet.member 3 b5 @?= True
    , testCase "member F"      $ BSet.member 7 b5 @?= False
    , testCase "member U"      $ BSet.member 20 b15 @?= True
    , testCase "Universe   0"  $ BSet.isUniverse b0 @?= False
    , testCase "Universe  15"  $ BSet.isUniverse b15 @?= True
    , testCase "Universe  50"  $ BSet.isUniverse b50 @?= False
    , testCase "Universe 105"  $ BSet.isUniverse b105 @?= True
    , testCase "Union 5  0"    $ BSet.union b5 empty @?= b5
    , testCase "Union 5 15"    $ BSet.union b5 b15 @?= Universe
    , testCase "cartProd 1  5" $ BSet.cartProduct b1 b5 @?= BSet.fromList [(x, y) | x <- [1], y <- [1..5]]
    , testCase "cartProd 1 15" $ BSet.cartProduct b1 b15 @?= Universe
    , testCase "map 1"         $ BSet.map succ b1 @?= BSet.singleton 2
    , testCase "getBound  5"   $ BSet.getBound b5 @?= 10
    , testCase "getBound 50"   $ BSet.getBound b50 @?= 100
    ]


b0   = BSet.fromList []       :: BoundSet Bound10 Int
b1   = BSet.fromList [1]      :: BoundSet Bound10 Int
b5   = BSet.fromList [1..5]   :: BoundSet Bound10 Int
b15  = BSet.fromList [1..15]  :: BoundSet Bound10 Int
b50  = BSet.fromList [1..50]  :: BoundSet Bound100 Int
b105 = BSet.fromList [1..105] :: BoundSet Bound100 Int
