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
import Data.Tree
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Insieme.Inspire as IR

import Prelude hiding (take)

-- | Parse binary dump.
parseBinaryDump :: BS.ByteString -> Either String (IR.Inspire)
parseBinaryDump = parseOnly $ do
    -- parse components
    parseHeader
    n            <- anyInt32
    dumpNodes    <- IntMap.fromList <$> zip [0..] <$> count n parseDumpNode
    m            <- anyInt32
    dumpBuiltins <- Map.fromList <$> count m parseBuiltin

    -- connect components
    let nodes    = connectDumpNodes dumpNodes
    let builtins = resolve nodes <$> dumpBuiltins

    return $ IR.Inspire (connectDumpNodes dumpNodes) builtins

  where
      resolve :: Tree IR.NodeType -> [Int] -> Tree IR.NodeType
      resolve node []     = node
      resolve node (x:xs) = resolve (subForest node !! x) xs

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
parseBuiltin = do
    key <- parseString
    val <- tail . splitOn "-" <$> parseString
    return (key, read <$> val)

connectDumpNodes :: IntMap.IntMap DumpNode -> Tree IR.NodeType
connectDumpNodes dumpNodes = evalState (go 0) IntMap.empty
  where
    go :: Int -> State (IntMap.IntMap (Tree IR.NodeType)) (Tree IR.NodeType)
    go index = do
        memo <- get
        case IntMap.lookup index memo of
            Just n -> return n
            Nothing -> do
                let (DumpNode irnode is) = dumpNodes IntMap.! index
                children <- mapM go is
                let n = Node irnode children
                modify (IntMap.insert index n)
                return n

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
