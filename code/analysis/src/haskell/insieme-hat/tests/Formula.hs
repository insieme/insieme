{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
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
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

module Formula (formulaTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Insieme.Utils.Arithmetic

formulaTests = testGroup "Formula"
    [ normProducts
    , normFormulas
    , addFormulas
    , mulFormulas
    ]


normProducts = testGroup "Normalize Product"
    [ testCase "empty"   $ norm [] @?= ""
    , testCase "combine" $ norm [fa, fb, fa] @?= "'a'^4 'b'^2"
    , testCase "cut"     $ norm [fa, fc] @?= ""
    ]
  where
    norm = prettyShowProduct . normalizeProduct . Product


normFormulas = testGroup "Normalize Formula"
    [ testCase "empty"     $ norm (Formula []) @?= "0"
    , testCase "const"     $ norm (Formula [Term 42 (Product [])]) @?= "42"
    , testCase "combine"   $ norm (Formula [t2, t2, t2]) @?= "15 'y'^3"
    , testCase "cut"       $ norm (Formula [t1, t4]) @?= prettyShowFactor fa
    , testCase "two terms" $ norm (Formula [t4, t2]) @?= "-1 'a'^2 + 5 'y'^3"
    ]
  where
    norm :: Formula Int Char -> String
    norm = prettyShow . normalize


addFormulas = testGroup "Add Formulas"
    [ testCase "double" $ add f1 f1 @?= norm (Formula [t1, t1, t2, t2, t3, t3])
    , testCase "add"    $ add f1 f2 @?= norm (Formula [t1, t2, t3, t4, t5, t6])
    ]
  where
    add :: Formula Int Char -> Formula Int Char -> String
    add x y = prettyShow $ addFormula x y

    norm :: Formula Int Char -> String
    norm = prettyShow . normalize


mulFormulas = testGroup "Multiply Formulas"
    [ testCase "square" $ mul f1 f1 @?= "20 'a'^2 'y'^3 + 8 'a'^2 'z'^1 + 4 'a'^4 + 20 'y'^3 'z'^1 + 25 'y'^6 + 4 'z'^2"
    ]
  where
    mul :: Formula Int Char -> Formula Int Char -> String
    mul x y = prettyShow $ mulFormula x y

    norm :: Formula Int Char -> String
    norm = prettyShow . normalize


fa = Factor 'a' 2
fb = Factor 'b' 2
fc = Factor 'a' (-2)
fx = Factor 'x' 2
fy = Factor 'y' 3
fz = Factor 'z' 1
t1 = Term 2 (Product [fa])
t2 = Term 5 (Product [fy])
t3 = Term 2 (Product [fz])
t4 = Term (-1) (Product [fa])
t5 = Term 4 (Product [fb])
t6 = Term 7 (Product [fx])
f1 = Formula [t1, t2, t3]
f2 = Formula [t4, t5, t6]
