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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Insieme.Inspire.Visit.NodeMap
    ( NodeMap
    , mkNodeMap
    , lookup
    , mapNodeMap
    , mapNodeFoldr
    ) where

import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Prelude hiding (map, lookup)

import qualified Insieme.Inspire.IR as IR

newtype NodeMap a = NodeMap (HashMap (IR.Tree) a)

mkNodeMap :: IR.Tree -> NodeMap IR.Tree
mkNodeMap node = NodeMap $ execState (buildMap node) $ HashMap.empty
  where
    buildMap :: IR.Tree -> State (HashMap IR.Tree IR.Tree) ()
    buildMap n = do
      whenM (not . HashMap.member n <$> get) $ do
           modify $ HashMap.insert n n
           mapM_ buildMap $ IR.getChildren n

    whenM :: Monad m => m Bool -> m () -> m ()
    whenM cond a = do
      b <- cond
      when b a

lookup :: IR.Tree -> NodeMap a -> Maybe a
lookup n (NodeMap m) = HashMap.lookup n m

instance Functor NodeMap where
    fmap = mapNodeMap

mapNodeMap :: (a -> b) -> NodeMap a -> NodeMap b
mapNodeMap f (NodeMap m) = NodeMap $ fmap f m

instance Foldable NodeMap where
    foldr = mapNodeFoldr

mapNodeFoldr :: (a -> b -> b) -> b -> NodeMap a -> b
mapNodeFoldr f a (NodeMap m) = foldr f a m
