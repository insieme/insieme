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

module Insieme.Analysis.Reachable (
    Reachable,
    toBool,
    reachableIn,
    reachableOut
) where


import Control.DeepSeq
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)

import qualified Insieme.Solver as Solver

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q

import qualified Insieme.Analysis.Boolean as Boolean
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * Reachable Lattice
--

newtype Reachable = Reachable Bool
    deriving (Eq,Show,Generic,NFData)

instance Solver.Lattice Reachable where
    bot = Reachable False
    merge (Reachable a) (Reachable b) = Reachable $ a || b


toBool :: Reachable -> Bool
toBool (Reachable b) = b


--
-- * Reachable-In Analysis
--

data ReachableInAnalysis = ReachableInAnalysis
    deriving (Typeable)

reachableInAnalysis :: Solver.AnalysisIdentifier
reachableInAnalysis = Solver.mkAnalysisIdentifier ReachableInAnalysis "R[in]"


--
-- * Reachable-In Variable Generator
--
reachableIn :: NodeAddress -> Solver.TypedVar Reachable

reachableIn a | I.isRoot a = Solver.mkVariable (reachableInIdGen a) [] (Reachable True)

reachableIn a = case Q.getNodeType parent of

    I.Lambda -> Solver.mkVariable (idGen a) [] (Reachable True)

    I.CompoundStmt -> var
        where
            n = I.getIndex a
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = if n == 0
                then Solver.forward (reachableIn parent) var
                else Solver.forward (reachableOut $ I.goDown (n-1) parent) var

    I.IfStmt -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = case I.getIndex a of
                0 -> Solver.forward (reachableIn parent) var
                1 -> Solver.forwardIf (compose Boolean.AlwaysTrue)  (Boolean.booleanValue $ I.goDown 0 parent) (reachableOut $ I.goDown 0 parent) var
                2 -> Solver.forwardIf (compose Boolean.AlwaysFalse) (Boolean.booleanValue $ I.goDown 0 parent) (reachableOut $ I.goDown 0 parent) var
                _ -> error "index out of bound"

    I.WhileStmt -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = case I.getIndex a of
                0 -> Solver.forward (reachableIn parent) var
                1 -> Solver.forwardIf (compose Boolean.AlwaysTrue)  (Boolean.booleanValue $ I.goDown 0 parent) (reachableOut $ I.goDown 0 parent) var
                _ -> error "index out of bound"

    -- for all others: if the parent is reachable, so is this node
    _ -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = Solver.forward (reachableIn parent) var

    where

        parent = fromJust $ I.getParent a

        idGen = reachableInIdGen

        compose = ComposedValue.toComposed



reachableInIdGen :: NodeAddress -> Solver.Identifier
reachableInIdGen a = Solver.mkIdentifierFromExpression reachableInAnalysis a



--
-- * Reachable-Out Analysis
--

data ReachableOutAnalysis = ReachableOutAnalysis
    deriving (Typeable)

reachableOutAnalysis :: Solver.AnalysisIdentifier
reachableOutAnalysis = Solver.mkAnalysisIdentifier ReachableOutAnalysis "R[out]"


--
-- * Reachable-Out Variable Generator
--
reachableOut :: NodeAddress -> Solver.TypedVar Reachable
reachableOut a = case Q.getNodeType a of

    -- the end of a return is not reachable
    I.ReturnStmt -> Solver.mkVariable (idGen a) [] Solver.bot

    -- the end of continue is not reachable
    I.ContinueStmt -> Solver.mkVariable (idGen a) [] Solver.bot

    -- the end of break is not reachable
    I.BreakStmt -> Solver.mkVariable (idGen a) [] Solver.bot

    -- a commound statement ends if the last statement ends
    I.CompoundStmt -> var
        where
            var = Solver.mkVariable (idGen a) [cnt] Solver.bot
            cnt = if I.numChildren a == 0
                then Solver.forward (reachableIn a) var
                else Solver.forward (reachableOut $ I.goDown ( (I.numChildren a) - 1 ) a) var

    -- the end of a if is reached if any of the bodies is finished
    I.IfStmt -> var
        where
            var = Solver.mkVariable (idGen a) [t,e] Solver.bot
            t = Solver.forward (reachableOut $ I.goDown 1 a) var
            e = Solver.forward (reachableOut $ I.goDown 2 a) var

    -- the end of a while is reached after a false condition
    I.WhileStmt -> var
        where
            var = Solver.mkVariable (idGen a) [cnt] Solver.bot
            cnt = Solver.forwardIf (ComposedValue.toComposed Boolean.AlwaysFalse) (Boolean.booleanValue $ I.goDown 0 a) (reachableOut $ I.goDown 0 a) var


    -- everything else: if the begin is reachable, so is the end
    _ -> var
        where
            var = Solver.mkVariable (idGen a) [con] Solver.bot
            con = Solver.forward (reachableIn a) var

    where

        idGen x = Solver.mkIdentifierFromExpression reachableOutAnalysis x

