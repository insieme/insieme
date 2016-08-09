import Test.Tasty
import Test.Tasty.HUnit

import ParseInt

main = defaultMain tests
tests = testGroup " Tests" [utils]
utils = testGroup "Utilities" [parseIntTests]
