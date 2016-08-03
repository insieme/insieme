
module Insieme.Analysis.Framework.Utils.OperatorHandler where

import Data.Tree
import Insieme.Inspire.NodeAddress
import qualified Insieme.Analysis.Solver as Solver

data OperatorHandler a = OperatorHandler {
    covers     :: NodeAddress -> Bool, 
    dependsOn  :: Solver.Assignment -> [Solver.Var],
    getValue   :: Solver.Assignment -> a
}
