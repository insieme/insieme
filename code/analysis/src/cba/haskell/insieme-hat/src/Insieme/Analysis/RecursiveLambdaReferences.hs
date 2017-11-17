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

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.RecursiveLambdaReferences (

    LambdaReferenceSet,
    recursiveCalls

) where

import Data.Typeable
import qualified Data.Set as Set

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q

import Insieme.Analysis.Solver
import Insieme.Analysis.FreeLambdaReferences


--
-- * RecursiveLambdaReferences Analysis
--

data RecursiveLambdaReferenceAnalysis = RecursiveLambdaReferenceAnalysis
    deriving (Typeable)


--
-- * the constraint generator
--

recursiveCalls :: NodeAddress -> TypedVar LambdaReferenceSet
recursiveCalls addr = case Q.getNodeType addr of

        I.Lambda | I.depth addr >= 3 -> var

        _ -> error "Can only compute recursive calls for lambdas with sufficient context!"

    where

        var = mkVariable varId [con] bot
        con = createConstraint dep val var

        varId = mkIdentifierFromExpression analysis addr
        analysis = mkAnalysisIdentifier RecursiveLambdaReferenceAnalysis "RecLambdaRefs"

        dep _ = toVar <$> freeRefVars
        val a = LambdaReferenceSet $ Set.filter f $ unLRS $ join $ get a <$> freeRefVars
            where
                f r = I.getNode r == tag

        tag = I.getNode $ I.goDown 0 $ I.goUp addr
        def = I.goUp $ I.goUp addr

        lambdas = I.goDown 1 <$> I.children def

        freeRefVars = freeLambdaReferences <$> lambdas


