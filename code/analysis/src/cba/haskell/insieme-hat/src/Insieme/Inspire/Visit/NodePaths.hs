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

{-# LANGUAGE ScopedTypeVariables #-}
module Insieme.Inspire.Visit.NodePaths where

import Data.Tree
-- import Data.DList (DList)
-- import qualified Data.DList as DL
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
-- import Data.Foldable

data NodePaths a = NodePaths Bool (Forest (Bool, a))
                 deriving (Eq, Show, Read)

mergeNodePath :: Bool -> [NodePaths Int] -> NodePaths Int
mergeNodePath b xs =
    NodePaths b $ zipWith (\i (NodePaths b x) -> Node (b, i) x) [0..] xs

flattenNodePaths :: NodePaths a -> [[a]]
flattenNodePaths (NodePaths b f) =
    map reverse $ treePathsReversed fst snd (Node (b, error msg) f)
  where
    msg = "flattenNodePaths'List: root node value should not be accessed"

treePathsReversed :: forall a i. (a -> Bool) -> (a -> i) -> Tree a -> [[i]]
treePathsReversed pred offset (Node c f) =
    (if pred c then ([]:) else id) $ goF [] [] f
  where
    go :: [([i], [Tree a])] -> [i] -> Tree a ->  [[i]]
    go q p (Node c f) = (if pred c then (cp:) else id) $ goF q cp f
      where
        cp = offset c : p

    goF :: [([i], [Tree a])] -> [i] -> [Tree a] ->  [[i]]
    goF q      p (n:ns) = go ((p,ns):q) p n
    goF (q:qs) _  []    = uncurry (goF qs) q
    goF  []    _  []    = []

-- flattenNodePaths'DList :: NodePaths a -> [[a]]
-- flattenNodePaths'DList (NodePaths b f) =
--     map DL.toList $ DL.toList $
--         (if b then (DL.empty `DL.cons`) else id) $ DL.concat $ map (go DL.empty) f
--   where
--     go :: Bool -> DList a -> (Bool, a) -> DList (DList a)
--     go b p (Node (b', c) (f:fs)) =
--         (if b then p `DL.cons` else id) $ go b' (p `DL.snoc` c) f
--     go b p (Node (b', c) [] ) =
--         _

--     go _ _ [] = DList.empty


-- -- -- has far more max memory usage than lists and dlists
-- flattenNodePaths'Seq :: NodePaths a -> [[a]]
-- flattenNodePaths'Seq (NodePaths b f) = map toList $ toList $ go b Seq.empty f
--   where
--     go :: Bool -> Seq a -> Forest (Bool, a) -> Seq (Seq a)
--     go b p f =
--         (if b then (p Seq.<|) else id) $ concatSeq $ flip fmap f $ \(Node (b', c) f) ->
--              go b' (p Seq.|> c) f

--     concatSeq :: Foldable t => t (Seq.Seq a) -> Seq.Seq a
--     concatSeq = foldr (Seq.><) Seq.empty

-- -- BEWARE: paths needs to be reversed
-- flattenNodePaths'List :: NodePaths a -> [[a]]
-- flattenNodePaths'List (NodePaths b f) = go b [] f
--   where
--     go :: Bool -> [a] -> Forest (Bool, a) -> [[a]]
--     go b p f =
--         concat $ flip map f $ \(Node (b', c) f) ->
--             (if b then (p:) else id) $ go b' (c : p) f
