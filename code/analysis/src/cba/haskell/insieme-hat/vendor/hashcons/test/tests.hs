import TestHashCons
import TestMemo

import Control.Monad
import System.Exit

main :: IO ()
main = do
  ok <- fmap and . sequence $
    [TestHashCons.runTests,
     TestMemo.runTests]
  unless ok exitFailure
