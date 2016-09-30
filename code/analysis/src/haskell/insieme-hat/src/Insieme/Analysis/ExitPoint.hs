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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.ExitPoint where

import Control.DeepSeq
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
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

            getReturnReachVars a = map (\a -> Reachable.reachableIn a) returns

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
