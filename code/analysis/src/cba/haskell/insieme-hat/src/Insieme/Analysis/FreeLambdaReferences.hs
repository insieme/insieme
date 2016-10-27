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
