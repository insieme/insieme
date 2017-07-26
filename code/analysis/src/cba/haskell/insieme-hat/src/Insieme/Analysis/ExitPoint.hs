{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
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
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.ExitPoint where

import Control.DeepSeq
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Visit
import qualified Data.Set as Set
import qualified Insieme.Analysis.Reachable as Reachable
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR


--
-- * ExitPoint Results
--

newtype ExitPoint = ExitPoint NodeAddress
 deriving (Eq, Ord, Generic, NFData)

instance Show ExitPoint where
    show (ExitPoint na) = "Exit@" ++ (prettyShow na)

--
-- * ExitPoint Lattice
--

type ExitPointSet = Set.Set ExitPoint

instance Solver.Lattice ExitPointSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs


--
-- * ExitPoint Analysis
--

data ExitPointAnalysis = ExitPointAnalysis
    deriving (Typeable)

exitPointAnalysis :: Solver.AnalysisIdentifier
exitPointAnalysis = Solver.mkAnalysisIdentifier ExitPointAnalysis "EP"


--
-- * ExitPoint Variable Generator
--

exitPoints :: NodeAddress -> Solver.TypedVar ExitPointSet
exitPoints addr = case getNodeType addr of

    -- for lambdas: collect all reachable return statements and end of body
    IR.Lambda -> var
        where
            var = Solver.mkVariable id [con] Solver.bot
            con = Solver.createConstraint dep val var

            dep a = (Solver.toVar endOfBodyReachable) : (map Solver.toVar (getReturnReachVars a))
            val a = exits
                where
                    exits =
                        if Reachable.toBool $ Solver.get a endOfBodyReachable
                        then Set.insert (ExitPoint body) reachableReturns
                        else reachableReturns

                    reachableReturns = foldr go Set.empty returns
                        where
                            go = \r s ->
                                if Reachable.toBool $ Solver.get a (Reachable.reachableIn r)
                                then Set.insert (ExitPoint r) s
                                else s

            returns = collectReturns addr

            getReturnReachVars _ = map Reachable.reachableIn returns

            body = (goDown 2 addr)

            endOfBodyReachable = Reachable.reachableOut body


    -- for bind expressions: use nested call expression
    IR.BindExpr -> Solver.mkVariable id [] $ Set.singleton $ ExitPoint $ goDown 2 addr

    -- everything else has no call sites
    _ -> Solver.mkVariable id [] Solver.bot

  where

    id = Solver.mkIdentifierFromExpression exitPointAnalysis addr



collectReturns :: NodeAddress -> [NodeAddress]
collectReturns = foldAddressPrune collector filter
  where
    filter cur = getNodeType cur == IR.LambdaExpr
    collector cur returns = if getNodeType cur == IR.ReturnStmt
                               then (goDown 0 cur : returns)
                               else returns
