import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)" [t]
  where
    t = QC.testProperty "sort == sort . reverse" $
            \list -> sort (list :: [Int]) == sort (reverse list)

unitTests = testGroup "Unit tests" [t]
  where
    t = testCase "List comparison (different length)" $
            [1, 2, 3] `compare` [1, 2] @?= GT
