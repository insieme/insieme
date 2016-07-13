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
passTree :: CString -> CSize -> IO (StablePtr IR.TreePackage)
passTree dump_c length_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral length_c)
    let Right tree = BinPar.parseBinaryDump dump
    newStablePtr tree

foreign export ccall "hat_passTree"
    passTree :: CString -> CSize -> IO (StablePtr IR.TreePackage)

-- | Calculate the size of the buffer which contains the Haskell
-- representation of the Inspire tree.
treeLength :: StablePtr IR.TreePackage -> IO CSize
treeLength tree_c = fromIntegral . length . IR.getTree <$> deRefStablePtr tree_c

foreign export ccall "hat_tree_length"
    treeLength :: StablePtr IR.TreePackage -> IO CSize

-- | Print default representation of the given tree.
printTree :: StablePtr IR.TreePackage -> IO ()
printTree tree_c = deRefStablePtr tree_c >>= (print . IR.getTree)

foreign export ccall "hat_tree_print"
    printTree :: StablePtr IR.TreePackage -> IO ()

-- | 2-dimensional drawing of the Inspire subtree located at the given
-- address.
printNode :: StablePtr Addr.NodeAddress -> IO ()
printNode addr_c = do
    addr <- deRefStablePtr addr_c
    putStrLn . drawTree $ show <$> Addr.getNode addr

foreign export ccall "hat_tree_printNode"
    printNode :: StablePtr Addr.NodeAddress -> IO ()

--
-- * Address
--

-- | Return a stable C pointer to a Haskell vector containing the
-- given NodeAddress.
passAddress :: StablePtr IR.TreePackage -> Ptr CSize -> CSize
            -> IO (StablePtr Addr.NodeAddress)
passAddress tree_c path_c length_c = do
    tree <- deRefStablePtr tree_c
    path <- peekArray (fromIntegral length_c) path_c
    newStablePtr $ Addr.mkNodeAddress (fromIntegral <$> path) (IR.getTree tree)

foreign export ccall "hat_passAddress"
    passAddress :: StablePtr IR.TreePackage -> Ptr CSize -> CSize
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

checkBoolean :: StablePtr Addr.NodeAddress -> StablePtr IR.TreePackage
             -> IO (CInt)
checkBoolean addr_c tree_c = handleAll (return . fromIntegral . fromEnum $ Both) $ do
    addr <- deRefStablePtr addr_c
    tree <- deRefStablePtr tree_c
    evaluate . fromIntegral . fromEnum . Solver.resolve . booleanValue $ addr

foreign export ccall "hat_checkBoolean"
    checkBoolean :: StablePtr Addr.NodeAddress -> StablePtr IR.TreePackage
                 -> IO (CInt)

--
-- * Utilities
--

handleAll :: IO a -> IO a -> IO a
handleAll dummy action = catch action $ \e -> do
    putStrLn $ "Exception: " ++ show (e :: SomeException)
    dummy
