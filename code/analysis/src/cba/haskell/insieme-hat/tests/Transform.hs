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
