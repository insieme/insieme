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

{-# LANGUAGE OverloadedStrings #-}

module Insieme.Inspire.BinaryDumper (dumpBinaryDump) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString.Builder
import Data.Char (ord)
import Data.IntMap.Strict (IntMap)
import Data.Maybe
import Data.Map.Strict (Map)
import Insieme.Inspire.NodeType

import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Insieme.Inspire as IR

dumpBinaryDump :: IR.Tree -> L.ByteString
dumpBinaryDump ir = toLazyByteString $ mconcat [ magicNr,
                                                 dumpConverters,
                                                 dumpNodes $ toDumpNodes ir]

-- * Dumping the header

dumpHeader :: Builder
dumpHeader = mappend magicNr dumpConverters

magicNr :: Builder
magicNr = word64LE 0x494e5350495245

dumpConverters :: Builder
dumpConverters = dumpList []

-- * Dumping the body

dumpNodes :: IntMap DumpNode -> Builder
dumpNodes ns = dumpList $ (dumpNode . snd) <$> IntMap.toAscList ns

dumpNode :: DumpNode -> Builder
dumpNode (DumpNode t cs) = mconcat [type_, value, children, annotations]
  where
    type_ = word16LE $ fromIntegral $ fromEnum $ toNodeType t

    value = case t of
        IR.BoolValue   v -> word8 $ if v then 1 else 0
        IR.CharValue   v -> word8 $ fromIntegral $ ord v
        IR.IntValue    v -> int32LE $ fromIntegral v
        IR.UIntValue   v -> word32LE $ fromIntegral v
        IR.StringValue v -> dumpString v
        _ -> mempty

    children = case t of
        IR.BoolValue   _ -> mempty
        IR.CharValue   _ -> mempty
        IR.IntValue    _ -> mempty
        IR.UIntValue   _ -> mempty
        IR.StringValue _ -> mempty
        _ -> dumpList $ (word32LE . fromIntegral) <$> cs

    annotations = dumpList []

-- * DumpNode utility structure

data DumpNode = DumpNode IR.NodeType [Int]
  deriving (Eq, Ord, Show)

toDumpNodes :: IR.Tree -> IntMap DumpNode
toDumpNodes n = IntMap.union baseMap $ IntMap.fromList $ Map.elems map
  where
    map = execState (toDumpNode n) Map.empty
    baseMap = IntMap.singleton 0 $ snd $ fromJust $ Map.lookup n map

toDumpNode :: IR.Tree -> State (Map IR.Tree (Int, DumpNode)) Int
toDumpNode n = do
    map <- get
    case Map.lookup n map of
        Just (i, _) -> return i
        Nothing     -> do
            children <- forM (IR.getChildren n) toDumpNode

            -- index 0 is reserved for the root node, therefore we start at 1
            -- and set the root afterwards
            let index = Map.size map + 1

            modify $ Map.insert n $ (index, DumpNode (IR.getNodeType n) children)
            return $ index

-- * Helper Functions

dumpList :: [Builder] -> Builder
dumpList bs = mconcat (l:bs)
  where
    l = word32LE $ fromIntegral $ length bs

dumpString :: String -> Builder
dumpString s = mappend (word32LE $ fromIntegral $ length s) (string8 s)
