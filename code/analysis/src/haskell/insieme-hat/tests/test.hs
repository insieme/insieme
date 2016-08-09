import Test.Tasty
import Test.Tasty.HUnit

import BoundSet
import Formula
import ParseInt

main = defaultMain tests

tests = testGroup " Tests" [utils]

utils = testGroup "Utilities"
    [ boundSetTests
    , formulaTests
    , parseIntTests
    ]
