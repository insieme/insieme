{-# LANGUAGE ForeignFunctionInterface #-}

module Insieme.Adapter where

import Data.Tree
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar



passIRdump :: CString -> CUInt -> IO (StablePtr (Tree IR.Inspire))
passIRdump dump_c length_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral length_c)
    let Right tree = BinPar.parseBinaryDump dump
    newStablePtr tree

foreign export ccall
    passIRdump :: CString -> CUInt -> IO (StablePtr (Tree IR.Inspire))



nodeCount :: StablePtr (Tree IR.Inspire) -> IO Int
nodeCount tree_c = length <$> deRefStablePtr tree_c

foreign export ccall
    nodeCount :: StablePtr (Tree IR.Inspire) -> IO Int



freeIRdump :: StablePtr (Tree IR.Inspire) -> IO ()
freeIRdump = freeStablePtr

foreign export ccall
    freeIRdump :: StablePtr (Tree IR.Inspire) -> IO ()
