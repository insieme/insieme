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

module Insieme.Analysis.FreeLambdaReferences (

    LambdaReferenceSet(..),
    freeLambdaReferences,

{-
    FreeRefsMark,
    hasFreeLambdaReferences
-}

) where

import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import Insieme.Analysis.Solver

--
-- * the lattice of this analysis
--

newtype LambdaReferenceSet = LambdaReferenceSet { unLRS :: Set.Set NodeAddress }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Lattice LambdaReferenceSet where
    bot = LambdaReferenceSet Set.empty
    (LambdaReferenceSet x) `merge` (LambdaReferenceSet y) = LambdaReferenceSet $ Set.union x y



--
-- * FreeLambdaReference Value Analysis
--

data FreeLambdaReferenceAnalysis = FreeLambdaReferenceAnalysis
    deriving (Typeable)


--
-- * the constraint generator
--

freeLambdaReferences :: NodeAddress -> TypedVar LambdaReferenceSet
freeLambdaReferences addr =

      if I.isRoot addr then direct else redirect

    where

      redirect = mkVariable varId [fwd] bot
      fwd = createConstraint dep val redirect
        where
          dep _ = [ toVar rooted_var ]
          val a = LambdaReferenceSet $ Set.mapMonotonic (I.append addr) (unLRS $ get a rooted_var)
          rooted_var = freeLambdaReferences $ I.crop addr

      direct = case Q.getNodeType addr of

        I.LambdaExpr -> var
            where
                var = mkVariable varId [con] bot
                con = forward (freeLambdaReferences $ I.goDown 2 addr) var

        -- this one handles root definitions, to utilize node sharing
        I.LambdaDefinition | I.isRoot addr -> var
            where
                var = mkVariable varId [con] bot :: TypedVar LambdaReferenceSet
                con = createConstraint (\_ -> toVar <$> deps) val var

                bindings = I.children addr
                definedRefs = (I.getNode . I.goDown 0) <$> bindings
                deps = (freeLambdaReferences . I.goDown 1) <$> bindings

                val a = LambdaReferenceSet $ Set.filter f $ unLRS $ join $ get a <$> deps
                    where
                        f n = notElem (I.getNode n) definedRefs


        -- this is the one utilizing shared data
        I.LambdaDefinition -> var
            where
                var = mkVariable varId [con] bot
                con = createConstraint dep val var

                dep _ = [toVar freeLambdaRefVar]
                val a = LambdaReferenceSet $ Set.mapMonotonic (I.append addr) $ unLRS $ get a freeLambdaRefVar

                freeLambdaRefVar = freeLambdaReferences $ I.crop addr

        _ -> var

      var = mkVariable varId [con] base
      con = createConstraint (\_ -> toVar <$> deps) val var

      base = LambdaReferenceSet $ Set.fromList $ I.collectAllPrune filter prune addr
          where
              filter n = Q.getNodeType n == I.LambdaReference

              prune a = if I.LambdaExpr == nodeType || Q.isType a then I.PruneHere else I.NoPrune
                  where
                      nodeType = Q.getNodeType a

      deps = freeLambdaReferences <$> I.collectAllPrune filter prune addr
          where
              filter n = Q.getNodeType n == I.LambdaDefinition

              prune a = if I.LambdaBinding == nodeType || Q.isType a then I.PruneHere else I.NoPrune
                  where
                      nodeType = Q.getNodeType a

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
