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
{-# LANGUAGE ForeignFunctionInterface #-}

{- | This module builds the bridge between the Haskell and C/C++ part of
    INSIEME.

All interaction between the two components happens over the foreign function
interface. The adapter exports and imports functions with C-calling-convention.
These functions are used to pass data back and forth and start analysis runs.

 -}

module Insieme.Adapter where

import Control.Exception
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Entities.FieldIndex as FieldIndex
import qualified Insieme.Analysis.Entities.SymbolicFormula as SymbolicFormula
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.BinaryDumper as BinDum
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Visit as Visit
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

-- 'freeStablePtr' is exported so it can be used directly within a C++ destructor.
foreign export ccall "hat_freeStablePtr"
  freeStablePtr :: StablePtr a -> IO ()

-- * Context

foreign import ccall "hat_update_context"
  updateContext :: Ctx.CContext -> StablePtr Ctx.Context -> IO ()

foreign export ccall "hat_initialize_context"
  initializeContext :: Ctx.CContext -> CString -> CSize -> IO (StablePtr Ctx.Context)
-- | Create a new 'Ctx.Context' by providing a reference to a 'Ctx.CContext'
-- and a binary dump (including size) of the relevant program.
initializeContext context_c dump_c size_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral size_c)
    let Right ir = BinPar.parseBinaryDump dump
    newStablePtr $ Ctx.mkContext context_c ir

foreign export ccall "hat_print_statistic"
  dumpStatistics :: StablePtr Ctx.Context -> IO ()
-- | Printer 'Solver.Solver' statistics.
dumpStatistics ctx_hs = do
    ctx <- deRefStablePtr ctx_hs
    putStrLn $ Solver.showSolverStatistic $ Ctx.getSolverState ctx

foreign export ccall "hat_dump_assignment"
  dumpAssignment :: StablePtr Ctx.Context -> IO ()
-- | Print current 'Solver.Solver' assignment.
dumpAssignment ctx_hs = do
    ctx <- deRefStablePtr ctx_hs
    putStrLn $ Solver.dumpSolverState True (Ctx.getSolverState ctx) "graph"

foreign export ccall "hat_mk_node_address"
  mkNodeAddress :: StablePtr Ctx.Context -> Ptr CSize -> CSize -> IO (StablePtr Addr.NodeAddress)
-- | Create a 'Addr.NodeAddress' from a dumped NodePath.
mkNodeAddress ctx_hs path_c length_c = do
    ctx  <- deRefStablePtr ctx_hs
    path <- peekArray (fromIntegral length_c) path_c
    newStablePtr $ Addr.mkNodeAddress (fromIntegral <$> path) (Ctx.getTree ctx)

foreign export ccall "hat_node_path_length"
  nodePathLength :: StablePtr Addr.NodeAddress -> IO CSize
-- | Returns the length of the given 'Addr.NodeAddress'
nodePathLength addr_hs = do
    addr <- deRefStablePtr addr_hs
    return $ fromIntegral $ Addr.depthAbsolute addr

foreign export ccall "hat_node_path_poke"
  nodePathPoke :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()
-- | Write the NodePath of the given 'Addr.NodeAddress' into the array pointed
-- to by @path_c@. Ensure the target array is big enough beforehand.
nodePathPoke addr_hs path_c = do
    addr <- deRefStablePtr addr_hs
    pokeArray path_c $ fromIntegral <$> Addr.getAbsolutePath addr

-- * Analysis

foreign export ccall "hat_find_declaration"
  findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)
-- | Run 'Visit.findDecl' visitor with the given 'Addr.NodeAddress' as input,
-- returns a 'Addr.NodeAddress' pointing to the found declaration or @null@.
findDecl var_hs = do
    var <- deRefStablePtr var_hs
    case Visit.findDecl var of
        Nothing -> return $ castPtrToStablePtr nullPtr
        Just a  -> newStablePtr a

foreign export ccall "hat_check_boolean"
  checkBoolean :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt
checkBoolean ctx_hs expr_hs = handleAll (return $ fromIntegral $ fromEnum AnBoolean.Both) $ do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) $ AnBoolean.booleanValue expr
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    evaluate $ fromIntegral $ fromEnum $ ComposedValue.toValue res

foreign export ccall "hat_check_alias"
  checkAlias :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> StablePtr Addr.NodeAddress -> IO CInt
checkAlias ctx_hs x_hs y_hs = handleAll (return . fromIntegral . fromEnum $ Alias.MayAlias) $ do
    ctx <- deRefStablePtr ctx_hs
    x <- deRefStablePtr x_hs
    y <- deRefStablePtr y_hs
    let (res,ns) = Alias.checkAlias (Ctx.getSolverState ctx) x y
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    evaluate $ fromIntegral $ fromEnum res

foreign export ccall "hat_arithmetic_value"
  arithValue :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticSet)
arithValue ctx_hs expr_hs = do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Arith.arithmeticValue expr)
    let results = ComposedValue.toValue res
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    passFormulaSet ctx_c $ BSet.map (fmap SymbolicFormula.getAddr) results

-- ** NodeAddresses

type CNodeAddress = ()
type CNodeAddressSet = ()

foreign import ccall "hat_mk_c_node_address"
  mkCNodeAddress :: Ctx.CContext -> Ptr CSize -> CSize -> IO (Ptr CNodeAddress)

foreign import ccall "hat_mk_c_node_address_set"
  mkCNodeAddressSet :: Ptr (Ptr CNodeAddress) -> CLLong -> IO (Ptr CNodeAddressSet)

passNodeAddress :: StablePtr Ctx.Context -> Addr.NodeAddress -> IO (Ptr CNodeAddress)
passNodeAddress ctx_hs addr = do
    ctx <- deRefStablePtr ctx_hs
    withArrayUnsignedLen (fromIntegral <$> Addr.getAbsolutePath addr) (mkCNodeAddress $ Ctx.getCContext ctx)

passNodeAddressSet :: BSet.IsBound bb => StablePtr Ctx.Context -> BSet.BoundSet bb Addr.NodeAddress -> IO (Ptr CNodeAddressSet)
passNodeAddressSet _ BSet.Universe = mkCNodeAddressSet nullPtr (-1)
passNodeAddressSet ctx_hs addrs = do
    addrs_c <- mapM (passNodeAddress ctx_hs) (BSet.toList addrs)
    withArraySignedLen addrs_c mkCNodeAddressSet

-- ** References

foreign export ccall "hat_check_null"
  checkForNull :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt
checkForNull = checkForReference Ref.NullReference

foreign export ccall "hat_check_extern"
  checkForExtern :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt
checkForExtern = checkForReference Ref.UninitializedReference

checkForReference :: Ref.Reference FieldIndex.SimpleFieldIndex -> StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt
checkForReference ref ctx_hs expr_hs = handleAll (return maybe) $ do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Ref.referenceValue expr)
    let results = (ComposedValue.toValue res) :: Ref.ReferenceSet FieldIndex.SimpleFieldIndex
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    evaluate $ case () of
        _ |  BSet.member ref results -> case () of
                _ | not (BSet.isUniverse results) && BSet.size results == 1 -> yes
                  | otherwise              -> maybe
          |  otherwise -> no
  where
    yes   = 0
    maybe = 1
    no    = 2

-- ** Memory Locations

type CMemoryLocation = ()
type CMemoryLocationSet = ()

foreign export ccall "hat_memory_locations"
  memoryLocations :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (Ptr CMemoryLocationSet)
memoryLocations ctx_hs expr_hs = do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Ref.referenceValue expr)
    let results = (ComposedValue.toValue res) :: Ref.ReferenceSet FieldIndex.SimpleFieldIndex
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    if BSet.member Ref.UninitializedReference results
        then passNodeAddressSet ctx_nhs (BSet.Universe :: BSet.UnboundSet Ref.Location)
        else passNodeAddressSet ctx_nhs $ BSet.map Ref.creationPoint $ BSet.filter f results
      where
        f (Ref.Reference _ _ ) = True
        f _ = False

-- ** Arithmetic

type CArithmeticSet = ()
type CArithmeticFormula = ()
type CArithmeticTerm = ()
type CArithmeticProduct = ()
type CArithmeticFactor = ()
type CArithmeticValue = ()

foreign import ccall "hat_mk_arithmetic_set"
  arithmeticSet :: Ptr (Ptr CArithmeticFormula) -> CLLong -> IO (Ptr CArithmeticSet)

foreign import ccall "hat_mk_arithmetic_formula"
  arithmeticFormula :: Ptr (Ptr CArithmeticTerm) -> CSize -> IO (Ptr CArithmeticFormula)

foreign import ccall "hat_mk_arithmetic_product"
  arithmeticProduct :: Ptr (Ptr CArithmeticFactor) -> CSize -> IO (Ptr CArithmeticProduct)

foreign import ccall "hat_mk_arithmetic_term"
  arithmeticTerm :: Ptr CArithmeticProduct -> CLong -> IO (Ptr CArithmeticTerm)

foreign import ccall "hat_mk_arithemtic_factor"
  arithemticFactor :: Ptr CArithmeticValue -> CInt -> IO (Ptr CArithmeticFactor)

foreign import ccall "hat_mk_arithmetic_value"
  arithmeticValue :: Ctx.CContext -> Ptr CSize -> CSize -> IO (Ptr CArithmeticValue)

passFormula :: Integral c => Ctx.CContext -> Ar.Formula c Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
passFormula ctx_c formula_hs = do
    terms_c <- forM (Ar.terms formula_hs) passTerm
    withArrayUnsignedLen terms_c arithmeticFormula
  where
    passTerm :: Integral c => Ar.Term c Addr.NodeAddress -> IO (Ptr CArithmeticTerm)
    passTerm term_hs = do
        product_c <- passProduct (Ar.product term_hs)
        arithmeticTerm product_c (fromIntegral $ Ar.coeff term_hs)

    passProduct :: Integral c => Ar.Product c Addr.NodeAddress -> IO (Ptr CArithmeticProduct)
    passProduct product_hs = do
        factors_c <- forM (Ar.factors product_hs) passFactor
        withArrayUnsignedLen factors_c arithmeticProduct

    passFactor :: Integral c => Ar.Factor c Addr.NodeAddress -> IO (Ptr CArithmeticFactor)
    passFactor factor_hs = do
        value_c <- passValue (Ar.base factor_hs)
        arithemticFactor value_c (fromIntegral $ Ar.exponent factor_hs)

    passValue :: Addr.NodeAddress -> IO (Ptr CArithmeticValue)
    passValue addr_hs = withArrayUnsignedLen (fromIntegral <$> Addr.getAbsolutePath addr_hs) (arithmeticValue ctx_c)

passFormulaSet :: Integral c => Ctx.CContext -> BSet.BoundSet bb (Ar.Formula c Addr.NodeAddress) -> IO (Ptr CArithmeticSet)
passFormulaSet _ BSet.Universe = arithmeticSet nullPtr (-1)
passFormulaSet ctx_c bs = do
    formulas <- mapM (passFormula ctx_c) (BSet.toList bs)
    withArraySignedLen formulas arithmeticSet

-- * Utilities

handleAll :: IO a -> IO a -> IO a
handleAll dummy action = catch action $ \e -> do
    putStrLn $ "Exception: " ++ show (e :: SomeException)
    dummy

withArraySignedLen :: Storable a => [a] -> (Ptr a -> CLLong -> IO b) -> IO b
withArraySignedLen xs f = withArrayLen xs (\s a -> f a (fromIntegral s))

withArrayUnsignedLen :: Storable a => [a] -> (Ptr a -> CSize -> IO b) -> IO b
withArrayUnsignedLen xs f = withArrayLen xs (\s a -> f a (fromIntegral s))

-- ** Arithmetic Tests

foreign export ccall "hat_test_formulaZero"
  testFormulaZero :: IO (Ptr CArithmeticFormula)
testFormulaZero = passFormula nullPtr Ar.zero

foreign export ccall "hat_test_formulaOne"
  testFormulaOne :: IO (Ptr CArithmeticFormula)
testFormulaOne = passFormula nullPtr Ar.one

foreign export ccall "hat_test_formulaExample1"
  testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
testFormulaExample1 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula nullPtr $ Ar.Formula [Ar.Term 2 (Ar.Product [Ar.Factor addr 2])]

foreign export ccall "hat_test_formulaExample2"
  testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
testFormulaExample2 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula nullPtr $ Ar.Formula [ Ar.Term 1 (Ar.Product []), Ar.Term 2 (Ar.Product [Ar.Factor addr 2, Ar.Factor addr 4]) ]

-- ** BinaryDumper Tests

foreign export ccall "hat_hs_test_binary_dumper_mirror"
  testBinaryDumperMirror :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()

testBinaryDumperMirror dump_c size_c dump_hs_ptr size_hs_ptr = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral size_c)
    let Right ir = BinPar.parseBinaryDump dump
    let dump' = BinDum.dumpBinaryDump ir
    let size  = BS8.length dump'
    dump_hs <- mallocBytes size :: IO CString
    pokeArray dump_hs (castCharToCChar <$> BS8.unpack dump')
    poke dump_hs_ptr dump_hs
    poke size_hs_ptr $ fromIntegral size
