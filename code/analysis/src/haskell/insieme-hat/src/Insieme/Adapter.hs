{-# LANGUAGE ForeignFunctionInterface #-}

module Insieme.Adapter where

import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Tree
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Insieme.Analysis.Boolean
import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Utils as IRUtils

--
-- * HSobject
--

foreign export ccall "hat_freeStablePtr"
    freeStablePtr :: StablePtr a -> IO ()

--
-- * Tree
--

-- | Get a stable C pointer to the Haskell Inspire representation of
-- the input (binary dump).
passIR :: CString -> CSize -> IO (StablePtr IR.Inspire)
passIR dump_c length_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral length_c)
    let Right ir = BinPar.parseBinaryDump dump
    newStablePtr ir

foreign export ccall "hat_passIR"
    passIR :: CString -> CSize -> IO (StablePtr IR.Inspire)

-- | Calculate the size of the buffer which contains the Haskell
-- representation of the Inspire tree.
treeLength :: StablePtr IR.Inspire -> IO CSize
treeLength ir_c = fromIntegral . length . IR.getTree <$> deRefStablePtr ir_c

foreign export ccall "hat_IR_length"
    treeLength :: StablePtr IR.Inspire -> IO CSize

-- | Print default representation of the given tree.
printTree :: StablePtr IR.Inspire -> IO ()
printTree ir_c = deRefStablePtr ir_c >>= (print . IR.getTree)

foreign export ccall "hat_IR_printTree"
    printTree :: StablePtr IR.Inspire -> IO ()

-- | 2-dimensional drawing of the Inspire subtree located at the given
-- address.
printNode :: StablePtr Addr.NodeAddress -> IO ()
printNode addr_c = do
    addr <- deRefStablePtr addr_c
    putStrLn . drawTree $ show <$> Addr.getNode addr

foreign export ccall "hat_IR_printNode"
    printNode :: StablePtr Addr.NodeAddress -> IO ()

--
-- * Address
--

-- | Return a stable C pointer to a Haskell vector containing the
-- given NodeAddress.
passAddress :: StablePtr IR.Inspire -> Ptr CSize -> CSize
            -> IO (StablePtr Addr.NodeAddress)
passAddress ir_c path_c length_c = do
    ir   <- deRefStablePtr ir_c
    path <- peekArray (fromIntegral length_c) path_c
    newStablePtr $ Addr.mkNodeAddress (fromIntegral <$> path) ir

foreign export ccall "hat_passAddress"
    passAddress :: StablePtr IR.Inspire -> Ptr CSize -> CSize
                -> IO (StablePtr Addr.NodeAddress)

-- | Return the size of the buffer representing the Haskell NodeAddress.
addrLength :: StablePtr Addr.NodeAddress -> IO CSize
addrLength addr_c = do
    addr <- deRefStablePtr addr_c
    return . fromIntegral . length . Addr.getAddress $ addr

foreign export ccall "hat_addr_length"
    addrLength :: StablePtr Addr.NodeAddress -> IO CSize

-- | Convert the address contained in the given buffer into a proper
-- C++ vector of type @vector<size_t>@.
addrToArray :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()
addrToArray addr_c dst = do
    addr <- deRefStablePtr addr_c
    pokeArray dst $ fromIntegral <$> Addr.getAddress addr

foreign export ccall "hat_addr_toArray"
    addrToArray :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()

--
-- * Parse IR
--

parseIR :: String -> IO IR.Inspire
parseIR ircode = do
    ptr_ir <- BS8.useAsCStringLen (BS8.pack ircode) parseIR_c'
    ir     <- deRefStablePtr ptr_ir
    freeStablePtr ptr_ir
    return ir
  where
    parseIR_c' (s,l) = parseIR_c s (fromIntegral l)

foreign import ccall "hat_parseIR"
    parseIR_c :: CString -> CSize -> IO (StablePtr IR.Inspire)

--
-- * Analysis
--

findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)
findDecl addr_c = do
    addr <- deRefStablePtr addr_c
    case IRUtils.findDecl addr of
        Nothing -> return $ castPtrToStablePtr nullPtr
        Just a  -> newStablePtr a

foreign export ccall "hat_findDecl"
    findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)

checkBoolean :: StablePtr Addr.NodeAddress -> StablePtr IR.Inspire
             -> IO (CInt)
checkBoolean addr_c ir_c = handleAll (return . fromIntegral . fromEnum $ Both) $ do
    addr <- deRefStablePtr addr_c
    ir   <- deRefStablePtr ir_c
    evaluate . fromIntegral . fromEnum . Solver.resolve . booleanValue $ addr

foreign export ccall "hat_checkBoolean"
    checkBoolean :: StablePtr Addr.NodeAddress -> StablePtr IR.Inspire
                 -> IO (CInt)

--
-- * Utilities
--

handleAll :: IO a -> IO a -> IO a
handleAll dummy action = catch action $ \e -> do
    putStrLn $ "Exception: " ++ show (e :: SomeException)
    dummy
