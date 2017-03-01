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

module Insieme.Analysis.FreeLambdaReferences (

    LambdaReferenceSet,
    freeLambdaReferences,

{-
    FreeRefsMark,
    hasFreeLambdaReferences
-}

) where

import Data.Typeable
import Insieme.Analysis.Solver
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import Insieme.Inspire.Visit
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR

--
-- * the lattice of this analysis
--

type LambdaReferenceSet = Set.Set NodeAddress

instance Lattice LambdaReferenceSet where
    bot = Set.empty
    merge = Set.union



--
-- * FreeLambdaReference Value Analysis
--

data FreeLambdaReferenceAnalysis = FreeLambdaReferenceAnalysis
    deriving (Typeable)


--
-- * the constraint generator
--

freeLambdaReferences :: NodeAddress -> TypedVar LambdaReferenceSet
freeLambdaReferences addr = case getNodeType addr of

        IR.LambdaExpr -> var
            where
                var = mkVariable varId [con] bot
                con = forward (freeLambdaReferences $ goDown 2 addr) var

        -- this one handles root definitions, to utilize node sharing
        IR.LambdaDefinition | isRoot addr -> var
            where
                var = mkVariable varId [con] bot
                con = createConstraint (\_ -> toVar <$> deps) val var

                bindings = getChildren addr
                definedRefs = (getNode . goDown 0) <$> bindings
                deps = (freeLambdaReferences . goDown 1) <$> bindings

                val a = Set.filter f $ join $ get a <$> deps
                    where
                        f n = notElem (getNode n) definedRefs


        -- this is the one utilizing shared data
        IR.LambdaDefinition -> var
            where
                var = mkVariable varId [con] bot
                con = createConstraint dep val var

                dep _ = [toVar freeLambdaRefVar]
                val a = Set.map (append addr) $ get a freeLambdaRefVar

                freeLambdaRefVar = freeLambdaReferences $ crop addr

        _ -> var

    where

        var = mkVariable varId [con] base
        con = createConstraint (\_ -> toVar <$> deps) val var

        base = Set.fromList $ foldAddressPrune collector prune addr
            where
                collector c l = case getNodeType c of
                    IR.LambdaReference -> c : l
                    _                  -> l

                prune a = IR.LambdaExpr == nodeType || isType a
                    where
                        nodeType = getNodeType a

        deps = foldAddressPrune collector prune addr
            where
                collector c l = case getNodeType c of
                    IR.LambdaDefinition -> freeLambdaReferences c : l
                    _                  -> l

                prune a = IR.LambdaBinding == nodeType || isType a
                    where
                        nodeType = getNodeType a

        val a = join $ get a <$> deps

        varId = mkIdentifierFromExpression analysis addr

        analysis = mkAnalysisIdentifier FreeLambdaReferenceAnalysis "FreeLambdaRefs"



{--
--
-- * A lattice for a abstracted version of the free lambda reference analysis
--

type FreeRefsMark = Bool

instance Lattice FreeRefsMark where
    bot = False
    merge = (||)



--
-- * FreeLambdaReference Value Analysis
--

data HasFreeLambdaReferenceAnalysis = HasFreeLambdaReferenceAnalysis
    deriving (Typeable)

--
-- * A lattice for a abstracted version of the free lambda reference analysis
--

hasFreeLambdaReferences :: NodeAddress -> TypedVar FreeRefsMark
hasFreeLambdaReferences addr = var
    where
        var = mkVariable varId [con] bot
        con = createConstraint dep val var

        freeRefVar = freeLambdaReferences addr
        freeRefVal a = get a freeRefVar

        dep _ = [toVar freeRefVar]
        val a = (not . Set.null) $ freeRefVal a

        varId = mkIdentifier analysis addr ""

        analysis = mkAnalysisIdentifier HasFreeLambdaReferenceAnalysis "HasFreeLambdaRefs"

-}
