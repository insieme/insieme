
module Insieme.Analysis.Examples where

-- import Compiler.Analysis
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Tree
import Debug.Trace
import Framework
import Insieme.Inspire.NodeAddress as Addr
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR
import Insieme.Inspire.Utils as IRUtils

import qualified Insieme.Analysis.Solver as Solver
import Insieme.Analysis.Boolean
import qualified Insieme.Boolean as Boolean

--
-- * Get Definition Point Analysis
--

findDeclr :: NodeAddress -> Maybe NodeAddress
findDeclr = IRUtils.findDeclr


--
-- * Boolean Value Analysis
--

checkBoolean :: NodeAddress -> Boolean.Result
checkBoolean addr = Solver.resolve $ booleanValue addr






