
module Insieme.Analysis.Examples where

import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils as IRUtils

import Insieme.Analysis.Solver as Solver

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






