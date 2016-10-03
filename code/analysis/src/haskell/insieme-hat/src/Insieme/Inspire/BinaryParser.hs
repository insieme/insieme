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

{-# LANGUAGE OverloadedStrings #-}

module Insieme.Inspire.BinaryParser (
    parseBinaryDump
) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Char (chr)
import Data.List.Split (splitOn)
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.NodeAddress as Addr

import Prelude hiding (take)

-- | Parse binary dump.
parseBinaryDump :: BS.ByteString -> Either String IR.Tree
parseBinaryDump bs = (Addr.getRoot . head) <$> parseAddresses bs

parseAddresses :: BS.ByteString -> Either String [Addr.NodeAddress]
parseAddresses = parseOnly $ do
    -- parse components
    parseHeader
    n            <- anyInt32
    dumpNodes    <- IntMap.fromList <$> zip [0..] <$> count n parseDumpNode
    m            <- anyInt32
    dumpBuiltins <- Map.fromList <$> count m parseBuiltin
    l            <- anyInt32
    addresses    <- count l parseNodePath

    -- connect components
    let root     = connectDumpNodes dumpNodes
    let builtins = resolve root <$> dumpBuiltins
    let ir       = markBuiltins root builtins 

    return $ (\p -> Addr.mkNodeAddress p ir) <$> addresses

  where
      resolve :: IR.Tree -> [Int] -> IR.Tree
      resolve node []     = node
      resolve node (x:xs) = resolve (IR.getChildren node !! x) xs


--
-- * Parsing the header
--

parseHeader :: Parser [String]
parseHeader = parseMagicNr *> parseConverters

parseMagicNr :: Parser Word64
parseMagicNr =  word64le 0x494e5350495245
            <|> fail "wrong magic number"

parseConverter :: Parser String
parseConverter = parseString

parseConverters :: Parser [String]
parseConverters = do
    n <- anyInt32
    count n parseConverter

--
-- * Parsing the body
--

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
        _              -> do
            c  <- anyInt32
            is <- count c anyInt32
            return $ DumpNode (IR.fromNodeType t) (fromIntegral <$> is)

    -- skip annotations
    a <- anyInt32
    count a anyWord64le

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
                let n = IR.mkNode index irnode children []
                modify (IntMap.insert index n)
                return n


markBuiltins :: IR.Tree -> Map.Map String IR.Tree -> IR.Tree
markBuiltins root builtins = evalState (go root) Map.empty
  where
    go :: IR.Tree -> State (Map.Map IR.Tree IR.Tree) IR.Tree
    go node = do
        memo <- get
        case Map.lookup node memo of
            Just n -> return n
            Nothing -> do
                let tags = builtinTags node
                children <- mapM go $ IR.getChildren node
                let n = IR.mkNode (IR.getID node) (IR.getNodeType node) children tags
                modify (Map.insert node n)
                return n
                
    builtinIndex = Map.fromList $ swap <$> Map.toList builtins
      where
        swap (n,t) = (t,n)
        
    builtinTags t = case Map.lookup t builtinIndex of
        Just n  -> [n]
        Nothing -> []  
                 

--
-- * Some helper functions to aid the parsing of values
--

anyInt8 :: Parser Int
anyInt8 = fromIntegral <$> anyWord8

anyInt16 :: Parser Int
anyInt16 = fromIntegral <$> anyWord16le

anyInt32 :: Parser Int
anyInt32 = fromIntegral <$> anyWord32le

parseString :: Parser String
parseString = liftM BS8.unpack $ take =<< anyInt32
