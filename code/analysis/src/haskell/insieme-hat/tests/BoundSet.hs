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
