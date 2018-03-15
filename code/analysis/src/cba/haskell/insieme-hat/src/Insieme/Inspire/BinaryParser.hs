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

{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ViewPatterns #-}

module Insieme.Inspire.BinaryParser (parseBinaryDump) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Char (chr)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Word
import Data.HashCons
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IntMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Prelude hiding (take)

import Insieme.Inspire.NodeReference

import qualified Insieme.Inspire.IR as IR
import qualified Insieme.Inspire.NodeAddress as Addr

-- | Parse INSPIRE binary dump. Builtins are attached to the end of the dump
-- and builtin nodes are marked as such.
parseBinaryDump :: BS.ByteString -> Either String IR.Tree
parseBinaryDump bs = (Addr.getRoot . head) <$> parseAddresses bs

parseAddresses :: BS.ByteString -> Either String [Addr.NodeAddress]
parseAddresses = parseOnly $ do
    -- parse components
    parseHeader
    dumpNodes    <- IntMap.fromList <$> zip [0..] <$> parseList parseDumpNode
    dumpBuiltins <- parseList parseBuiltin
    addresses    <- parseList parseNodePath

    -- connect components
    let root     = connectDumpNodes dumpNodes
    let builtins = resolve root <$> dumpBuiltins
    let ir       = markBuiltins root builtins

    return $ (\p -> Addr.mkNodeAddress p ir) <$> addresses

  where
      resolve :: IR.Tree -> (String, [Int]) -> (String, IR.Tree)
      resolve root (s, addr) = (s, resolveAddr root addr)

      resolveAddr :: IR.Tree -> [Int] -> IR.Tree
      resolveAddr node []     = node
      resolveAddr node (x:xs) = resolveAddr (children node !! x) xs

-- * Parsing the header

parseHeader :: Parser [String]
parseHeader = parseMagicNr *> parseList parseConverter

parseMagicNr :: Parser Word64
parseMagicNr =  word64le 0x494e5350495245 <?> "magic number"

parseConverter :: Parser String
parseConverter = parseString

-- * Parsing the body

data DumpNode = DumpNode IR.NodeType [Int]

parseDumpNode :: Parser DumpNode
parseDumpNode = do
    -- get node type
    t <- toEnum <$> anyInt16

    -- create corresponding node
    n <- case t of
        -- directly handle value nodes (leafs)
        IR.NT_BoolValue   -> DumpNode <$> IR.BoolValue <$> (/=0) <$> anyInt8     <*> pure []
        IR.NT_CharValue   -> DumpNode <$> IR.CharValue <$> chr   <$> anyInt8     <*> pure []
        IR.NT_IntValue    -> DumpNode <$> IR.IntValue            <$> anyInt32    <*> pure []
        IR.NT_UIntValue   -> DumpNode <$> IR.UIntValue           <$> anyInt32    <*> pure []
        IR.NT_StringValue -> DumpNode <$> IR.StringValue         <$> parseString <*> pure []

        -- intermediate nodes
        _ -> do
            is  <- parseList anyInt32
            return $ DumpNode (IR.fromNodeType t) (fromIntegral <$> is)

    -- skip annotations
    parseList anyWord64le

    return $ n

parseBuiltin :: Parser (String, [Int])
parseBuiltin = (,) <$> parseString <*> parseNodePath

parseNodePath :: Parser [Int]
parseNodePath = map read . tail . splitOn "-" <$> parseString

connectDumpNodes :: IntMap.IntMap DumpNode -> IR.Tree
connectDumpNodes dumpNodes = evalState (go 0) IntMap.empty
  where
    go :: Int -> State (IntMap.IntMap IR.Tree) IR.Tree
    go index = do
        memo <- get
        case IntMap.lookup index memo of
            Just n -> return n
            Nothing -> do
                let (DumpNode irnode is) = dumpNodes IntMap.! index
                children <- mapM go is
                let n = IR.mkNode irnode children []
                modify (IntMap.insert index n)
                return n


markBuiltins :: IR.Tree -> [(String, IR.Tree)] -> IR.Tree
markBuiltins root builtins = evalState (go root) HashMap.empty
  where
    go :: IR.Tree -> State (HashMap IR.Tree IR.Tree) IR.Tree
    go node0@(IR.Tree nt0 ch0 bt0) = do
        memo <- get
        case HashMap.lookup node0 memo of
            Just n -> return n
            Nothing -> do
                ch1 <- mapM go ch0
                let bt1  = bt0 ++ fromMaybe [] (HashMap.lookup node0 builtinIndex)
                    node1 = IR.mkNode nt0 ch1 bt1
                modify (HashMap.insert node0 node1)
                return node1

    builtinIndex = HashMap.fromListWith (++) $ (\(x,y) -> (y,[x])) <$> builtins

-- * Helper Functions

anyInt8 :: Parser Int
anyInt8 = fromIntegral <$> anyWord8

anyInt16 :: Parser Int
anyInt16 = fromIntegral <$> anyWord16le

anyInt32 :: Parser Int
anyInt32 = fromIntegral <$> anyWord32le

parseList :: Parser a -> Parser [a]
parseList p = anyInt32 >>= flip count p

parseString :: Parser String
parseString = liftM BS8.unpack $ anyInt32 >>= take
