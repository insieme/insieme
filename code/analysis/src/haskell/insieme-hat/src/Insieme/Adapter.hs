{-# LANGUAGE ForeignFunctionInterface #-}

module Insieme.Adapter where

import Data.Foldable
import Data.Maybe
import Data.Tree
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Analysis.Examples as Anal
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr

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
passTree :: CString -> CSize -> IO (StablePtr (Tree IR.Inspire))
passTree dump_c length_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral length_c)
    let Right tree = BinPar.parseBinaryDump dump
    newStablePtr tree

foreign export ccall "hat_passTree"
    passTree :: CString -> CSize -> IO (StablePtr (Tree IR.Inspire))

-- | Calculate the size of the buffer which contains the Haskell
-- representation of the Inspire tree.
treeLength :: StablePtr (Tree IR.Inspire) -> IO CSize
treeLength tree_c = fromIntegral . length <$> deRefStablePtr tree_c

foreign export ccall "hat_tree_length"
    treeLength :: StablePtr (Tree IR.Inspire) -> IO CSize

-- | 2-dimensional drawing of the Inspire subtree located at the given
-- address.
printNode :: StablePtr (Tree IR.Inspire) -> StablePtr Addr.NodeAddress -> IO ()
printNode tree_c addr_c = do
    tree <- deRefStablePtr tree_c
    addr <- deRefStablePtr addr_c
    case Addr.resolve addr $ Addr.addressTree tree of
        Just t  -> putStrLn $ drawTree $ show <$> t
        Nothing -> putStrLn "Invalid NodeAddress for given tree"

foreign export ccall "hat_tree_printNode"
    printNode :: StablePtr (Tree IR.Inspire) -> StablePtr Addr.NodeAddress
              -> IO ()

--
-- * Address
--

-- | Return a stable C pointer to a Haskell vector containing the
-- given NodeAddress.
passAddress :: Ptr CSize -> CSize -> IO (StablePtr Addr.NodeAddress)
passAddress path_c length_c = do
    path <- peekArray (fromIntegral length_c) path_c
    let addr = Addr.fromList (fromIntegral <$> path)
    newStablePtr addr

foreign export ccall "hat_passAddress"
    passAddress :: Ptr CSize -> CSize -> IO (StablePtr Addr.NodeAddress)

-- | Return the size of the buffer representing the Haskell NodeAddress.
addrLength :: StablePtr Addr.NodeAddress -> IO CSize
addrLength addr_c = fromIntegral . Addr.length <$> deRefStablePtr addr_c

foreign export ccall "hat_addr_length"
    addrLength :: StablePtr Addr.NodeAddress -> IO CSize

-- | Convert the address contained in the given buffer into a proper
-- C++ vector of type @vector<size_t>@.
addrToArray :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()
addrToArray addr_c dst = do
    addr <- deRefStablePtr addr_c
    pokeArray dst $ fromIntegral <$> toList addr

foreign export ccall "hat_addr_toArray"
    addrToArray :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()

--
-- * Analysis
--

findDeclr :: StablePtr (Tree IR.Inspire) -> StablePtr Addr.NodeAddress
          -> IO (StablePtr Addr.NodeAddress)
findDeclr tree_c addr_c = do
    tree <- deRefStablePtr tree_c
    addr <- deRefStablePtr addr_c
    case Anal.findDeclr addr tree of
        Nothing -> return $ castPtrToStablePtr nullPtr
        Just a  -> newStablePtr a

foreign export ccall "hat_findDeclr"
    findDeclr :: StablePtr (Tree IR.Inspire) -> StablePtr Addr.NodeAddress
              -> IO (StablePtr Addr.NodeAddress)
