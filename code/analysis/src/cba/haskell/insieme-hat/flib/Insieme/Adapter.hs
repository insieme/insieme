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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module builds the bridge between the Haskell and C/C++ part of
    INSIEME.

All interaction between the two components happens over the foreign function
interface. The adapter exports and imports functions with C-calling-convention.
These functions are used to pass data back and forth and start analysis runs.

 -}

module Insieme.Adapter where

import Control.Exception
import Control.Exception.Base (evaluate)
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.CStorable
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Entities.FieldIndex as FieldIndex
import qualified Insieme.Analysis.Entities.SymbolicFormula as SymbolicFormula
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Analysis.SymbolicValue as SV
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryDumper as BinDum
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Visit as Visit
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

-- * General FFI

-- | C representation of 'a'
data CRep a

-- | C representation of 'BSet.BoundSet bb a'
data CSet a

-- Convenience alises
type CRepPtr a = Ptr (CRep a)
type CSetPtr a = Ptr (CSet a)
type CRepArr a = Ptr (Ptr (CRep a))

-- * Exports

foreign export ccall "hat_freeStablePtr"
  freeStablePtr :: StablePtr a -> IO ()

foreign export ccall "hat_initialize_context"
  initializeContext :: Ctx.CContext -> CString -> CSize -> IO (StablePtr Ctx.Context)

foreign export ccall "hat_print_statistic"
  dumpStatistics :: StablePtr Ctx.Context -> IO ()

foreign export ccall "hat_dump_assignment"
  dumpAssignment :: StablePtr Ctx.Context -> IO ()

foreign export ccall "hat_mk_node_address"
  mkNodeAddress :: StablePtr Ctx.Context -> Ptr CSize -> CSize -> IO (StablePtr Addr.NodeAddress)

foreign export ccall "hat_node_path_length"
  nodePathLength :: StablePtr Addr.NodeAddress -> IO CSize

foreign export ccall "hat_node_path_poke"
  nodePathPoke :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()

foreign export ccall "hat_find_declaration"
  findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)

foreign export ccall "hat_check_boolean"
  checkBoolean :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)

foreign export ccall "hat_check_alias"
  checkAlias :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)

foreign export ccall "hat_arithmetic_value"
  arithValue :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr (CSetPtr ArithmeticFormula))

foreign export ccall "hat_check_null"
  checkForNull :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)

foreign export ccall "hat_check_extern"
  checkForExtern :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)

foreign export ccall "hat_memory_locations"
  memoryLocations :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr (CSetPtr MemoryLocation))

foreign export ccall "hat_test_formulaZero"
  testFormulaZero :: IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_test_formulaOne"
  testFormulaOne :: IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_test_formulaExample1"
  testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_test_formulaExample2"
  testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_hs_test_binary_dumper_mirror"
  testBinaryDumperMirror :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()

foreign export ccall "hat_hs_symbolic_values"
  symbolicValues :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr (CSetPtr SV.SymbolicValue))

-- * Context

initializeContext :: Ctx.CContext -> CString -> CSize -> IO (StablePtr Ctx.Context)
-- | Create a new 'Ctx.Context' by providing a reference to a 'Ctx.CContext'
-- and a binary dump (including size) of the relevant program.
initializeContext context_c dump_c size_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral size_c)
    let Right ir = BinPar.parseBinaryDump dump
    newStablePtr $ Ctx.mkContext context_c ir

dumpStatistics :: StablePtr Ctx.Context -> IO ()
-- | Printer 'Solver.Solver' statistics.
dumpStatistics ctx_hs = do
    ctx <- deRefStablePtr ctx_hs
    putStrLn $ Solver.showSolverStatistic $ Ctx.getSolverState ctx

-- | Print current 'Solver.Solver' assignment.
dumpAssignment :: StablePtr Ctx.Context -> IO ()
dumpAssignment ctx_hs = do
    ctx <- deRefStablePtr ctx_hs
    putStrLn $ Solver.dumpSolverState True (Ctx.getSolverState ctx) "graph"

-- | Create a 'Addr.NodeAddress' from a dumped NodePath.
mkNodeAddress :: StablePtr Ctx.Context -> Ptr CSize -> CSize -> IO (StablePtr Addr.NodeAddress)
mkNodeAddress ctx_hs path_c length_c = do
    ctx  <- deRefStablePtr ctx_hs
    path <- peekArray (fromIntegral length_c) path_c
    newStablePtr $ Addr.mkNodeAddress (fromIntegral <$> path) (Ctx.getTree ctx)

-- | Returns the length of the given 'Addr.NodeAddress'
nodePathLength :: StablePtr Addr.NodeAddress -> IO CSize
nodePathLength addr_hs = do
    addr <- deRefStablePtr addr_hs
    return $ fromIntegral $ Addr.depthAbsolute addr

-- | Write the NodePath of the given 'Addr.NodeAddress' into the array pointed
-- to by @path_c@. Ensure the target array is big enough beforehand.
nodePathPoke :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()
nodePathPoke addr_hs path_c = do
    addr <- deRefStablePtr addr_hs
    pokeArray path_c $ fromIntegral <$> Addr.getAbsolutePath addr

foreign import ccall "hat_c_get_timelimit"
  getTimelimit :: Ctx.CContext -> IO CLLong

-- * Analysis Result

-- Cannot use 'StablePtr Ctx.Context' as it is no instance of 'Generic'
data AnalysisResult a = AnalysisResult (Ptr ()) Bool a
  deriving (Generic)

type AnalysisResultPtr a = Ptr (AnalysisResult a)

instance CStorable a => CStorable (AnalysisResult a)
instance CStorable a => Storable (AnalysisResult a) where
    sizeOf = cSizeOf
    alignment = cAlignment
    peek = cPeek
    poke = cPoke

allocAnalysisResult :: forall a. CStorable a
                    => StablePtr Ctx.Context
                    -> Bool
                    -> a
                    -> IO (Ptr (AnalysisResult a))
allocAnalysisResult ctx t v = do
    res <- malloc :: IO (Ptr (AnalysisResult a))
    poke res $ AnalysisResult (castStablePtrToPtr ctx) t v
    return res

-- * Analysis

-- | Run 'Visit.findDecl' visitor with the given 'Addr.NodeAddress' as input,
-- returns a 'Addr.NodeAddress' pointing to the found declaration or @null@.
findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)
findDecl var_hs = do
    var <- deRefStablePtr var_hs
    case Visit.findDecl var of
        Nothing -> return $ castPtrToStablePtr nullPtr
        Just a  -> newStablePtr a

checkBoolean :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)
checkBoolean ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) $ AnBoolean.booleanValue expr
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ serialize res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True =<< serialize Solver.top
  where
    serialize = evaluate . convertResult . ComposedValue.toValue

    convertResult AnBoolean.AlwaysTrue  = 0
    convertResult AnBoolean.AlwaysFalse = 1
    convertResult AnBoolean.Both        = 2
    convertResult AnBoolean.Neither     = 3

checkAlias :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)
checkAlias ctx_hs x_hs y_hs = do
    ctx <- deRefStablePtr ctx_hs
    x <- deRefStablePtr x_hs
    y <- deRefStablePtr y_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let (res,ns) = Alias.checkAlias (Ctx.getSolverState ctx) x y
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ serialize res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True $ convertResult Alias.MayAlias
  where
    serialize = evaluate . convertResult

    convertResult Alias.AreAlias = 0
    convertResult Alias.MayAlias = 1
    convertResult Alias.NotAlias = 2

arithValue :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr (CSetPtr ArithmeticFormula))
arithValue ctx_hs expr_hs = do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let ctx_c = Ctx.getCContext ctx
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) $ Arith.arithmeticValue expr
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ serialize ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True =<< serialize ctx_c Solver.top
  where
    serialize :: Ctx.CContext -> Arith.ArithResult -> IO (CSetPtr ArithmeticFormula)
    serialize ctx_c r = passBoundSet (passFormula ctx_c) arithmeticSet
                      $ BSet.map Ar.convert
                      $ BSet.map (fmap SymbolicFormula.getAddr)
                      $ Arith.unSFS
                      $ ComposedValue.toValue r

-- ** NodeAddresses

foreign import ccall "hat_mk_c_node_address"
  mkCNodeAddress :: Ctx.CContext -> Ptr CSize -> CSize -> IO (CRepPtr Addr.NodeAddress)

foreign import ccall "hat_del_c_node_address"
  delCNodeAddress :: CRepPtr Addr.NodeAddress -> IO ()

foreign import ccall "hat_mk_c_node_address_set"
  mkCNodeAddressSet :: CRepArr Addr.NodeAddress -> CLLong -> IO (CSetPtr Addr.NodeAddress)

passNodeAddress :: Ctx.CContext -> Addr.NodeAddress -> IO (CRepPtr Addr.NodeAddress)
passNodeAddress ctx_c addr = do
    withArrayUnsignedLen (fromIntegral <$> Addr.getAbsolutePath addr) (mkCNodeAddress ctx_c)

-- ** References

checkForNull :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)
checkForNull = checkForReference Ref.NullReference

checkForExtern :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)
checkForExtern = checkForReference Ref.UninitializedReference

checkForReference :: Ref.Reference FieldIndex.SimpleFieldIndex -> StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (AnalysisResultPtr CInt)
checkForReference ref ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)

    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Ref.referenceValue expr)
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }

    let results = Ref.unRS $ ComposedValue.toValue res :: BSet.UnboundSet (Ref.Reference FieldIndex.SimpleFieldIndex)
    let result = case () of
                   _ |  BSet.member ref results -> case () of
                           _ | not (BSet.isUniverse results) && BSet.size results == 1 -> yes
                             | otherwise -> maybe
                     |  otherwise -> no

    result' <- timeout timelimit (evaluate result)
    case result' of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True maybe
  where
    yes   = 0
    maybe = 1
    no    = 2

-- ** Memory Locations

type MemoryLocation = Addr.NodeAddress

memoryLocations :: StablePtr Ctx.Context
                -> StablePtr Addr.NodeAddress
                -> IO (AnalysisResultPtr (CSetPtr MemoryLocation))
memoryLocations ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let ctx_c = Ctx.getCContext ctx

    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Ref.referenceValue expr)
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }

    --let results = Ref.unRS $ ComposedValue.toValue res :: BSet.UnboundSet (Ref.Reference FieldIndex.SimpleFieldIndex)
    result <- timeout timelimit $ serialize ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True =<< serialize ctx_c Solver.top
  where
    serialize ctx_c results = passBoundSet (passNodeAddress ctx_c) mkCNodeAddressSet
                            $ if BSet.member Ref.UninitializedReference (unwrap results)
                                then (BSet.Universe :: BSet.UnboundSet Ref.Location)
                                else  BSet.map Ref.creationPoint $ BSet.filter f (unwrap results)

    unwrap :: (ValueTree.Tree FieldIndex.SimpleFieldIndex (Ref.ReferenceSet FieldIndex.SimpleFieldIndex))
           -> BSet.UnboundSet (Ref.Reference FieldIndex.SimpleFieldIndex)
    unwrap = Ref.unRS . ComposedValue.toValue

    f (Ref.Reference _ _ ) = True
    f _ = False

-- ** Arithmetic

type ArithmeticFormula = Ar.Formula CLong ArithmeticValue
type ArithmeticTerm = Ar.Term CLong ArithmeticValue
type ArithmeticProduct = Ar.Product CInt ArithmeticValue
type ArithmeticFactor = Ar.Factor CInt ArithmeticValue

-- Arithmetic values are actually wrapped in C, yet we do not care about this here.
type ArithmeticValue = Addr.NodeAddress

foreign import ccall "hat_mk_arithmetic_set"
  arithmeticSet :: CRepArr ArithmeticFormula -> CLLong -> IO (CSetPtr ArithmeticFormula)

foreign import ccall "hat_mk_arithmetic_formula"
  arithmeticFormula :: CRepArr ArithmeticTerm -> CSize -> IO (CRepPtr ArithmeticFormula)

foreign import ccall "hat_mk_arithmetic_term"
  arithmeticTerm ::CRepPtr ArithmeticProduct -> CLong -> IO (CRepPtr ArithmeticTerm)

foreign import ccall "hat_mk_arithmetic_product"
  arithmeticProduct :: CRepArr ArithmeticFactor -> CSize -> IO (CRepPtr ArithmeticProduct)

foreign import ccall "hat_mk_arithemtic_factor"
  arithemticFactor :: CRepPtr ArithmeticValue -> CInt -> IO (CRepPtr ArithmeticFactor)

foreign import ccall "hat_mk_arithmetic_value"
  arithmeticValue :: Ctx.CContext -> Ptr CSize -> CSize -> IO (CRepPtr ArithmeticValue)

foreign import ccall "hat_del_arithmetic_value"
  delArithmeticValue :: CRepPtr ArithmeticValue -> IO ()

foreign import ccall "hat_del_arithmetic_factor"
  delArithmeticFactor :: CRepPtr ArithmeticFactor -> IO ()

foreign import ccall "hat_del_arithmetic_product"
  delArithmeticProduct :: CRepPtr ArithmeticProduct -> IO ()

foreign import ccall "hat_del_arithmetic_term"
  delArithmeticTerm :: CRepPtr ArithmeticTerm -> IO ()

passFormula :: Integral c => Ctx.CContext -> Ar.Formula c Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)
passFormula ctx_c formula_hs = bracket
    (forM (Ar.terms formula_hs) passTerm)
    (mapM_ delArithmeticTerm)
    (\terms_c -> withArrayUnsignedLen terms_c arithmeticFormula)
  where
    passTerm :: Integral c => Ar.Term c Addr.NodeAddress -> IO (CRepPtr ArithmeticTerm)
    passTerm term_hs = bracket
        (passProduct (Ar.product term_hs))
        (delArithmeticProduct)
        (\product_c -> arithmeticTerm product_c (fromIntegral $ Ar.coeff term_hs))

    passProduct :: Integral c => Ar.Product c Addr.NodeAddress -> IO (CRepPtr ArithmeticProduct)
    passProduct product_hs = bracket
        (forM (Ar.factors product_hs) passFactor)
        (mapM_ delArithmeticFactor)
        (\factors_c -> withArrayUnsignedLen factors_c arithmeticProduct)

    passFactor :: Integral c => Ar.Factor c Addr.NodeAddress -> IO (CRepPtr ArithmeticFactor)
    passFactor factor_hs = bracket
        (passValue (Ar.base factor_hs))
        (delArithmeticValue)
        (\value_c -> arithemticFactor value_c (fromIntegral $ Ar.exponent factor_hs))

    passValue :: Addr.NodeAddress -> IO (CRepPtr ArithmeticValue)
    passValue addr_hs = withArrayUnsignedLen (fromIntegral <$> Addr.getAbsolutePath addr_hs) (arithmeticValue ctx_c)

-- * Symbolic Value

foreign import ccall "hat_c_mk_symbolic_value_set"
  mkCSymbolicValueSet :: CRepArr SV.SymbolicValue -> CLLong -> IO (CSetPtr SV.SymbolicValue)

symbolicValues :: StablePtr Ctx.Context
               -> StablePtr Addr.NodeAddress
               -> IO (AnalysisResultPtr (CSetPtr SV.SymbolicValue))
symbolicValues ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let ctx_c =  Ctx.getCContext ctx
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (SV.symbolicValue stmt)
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ serialize ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True =<< serialize ctx_c Solver.top
  where
    serialize :: Ctx.CContext -> SV.SymbolicValueLattice -> IO (CSetPtr SV.SymbolicValue)
    serialize ctx_c = passSymbolicValueSet ctx_c . SV.unSVS . ComposedValue.toValue

passSymbolicValueSet :: Ctx.CContext
                     -> BSet.BoundSet bb SV.SymbolicValue
                     -> IO (CSetPtr SV.SymbolicValue)
passSymbolicValueSet ctx s = passBoundSet (dumpIrTree ctx) mkCSymbolicValueSet s

-- * Utilities

foreign import ccall "hat_c_mk_ir_tree"
  mkCIrTree :: Ctx.CContext -> CString -> CSize -> IO (CRepPtr IR.Tree)

dumpIrTree :: Ctx.CContext -> IR.Tree -> IO (CRepPtr IR.Tree)
dumpIrTree ctx irtree = BS8.useAsCStringLen (BinDum.dumpBinaryDump irtree)
                      $ \(sz,l) -> mkCIrTree ctx sz (fromIntegral l)

foreign import ccall "hat_c_del_ir_tree"
  delCIrTree :: CRepPtr IR.Tree -> IO ()

passBoundSet :: (a -> IO (CRepPtr a))                   -- ^ Element constructor
             -> (CRepArr a -> CLLong -> IO (CSetPtr a)) -- ^ Set constructor (destroys elements)
             -> BSet.BoundSet bb a                      -- ^ input set
             -> IO (CSetPtr a)
passBoundSet _ mkSet BSet.Universe = mkSet nullPtr (-1)
passBoundSet mkElem mkSet s = do
    elems <- forM (BSet.toList s) mkElem
    withArraySignedLen elems mkSet

withArraySignedLen :: Storable a => [a] -> (Ptr a -> CLLong -> IO b) -> IO b
withArraySignedLen xs f = withArrayLen xs (\s a -> f a (fromIntegral s))

withArrayUnsignedLen :: Storable a => [a] -> (Ptr a -> CSize -> IO b) -> IO b
withArrayUnsignedLen xs f = withArrayLen xs (\s a -> f a (fromIntegral s))

-- ** Arithmetic Tests

testFormulaZero :: IO (CRepPtr ArithmeticFormula)
testFormulaZero = passFormula nullPtr Ar.zero

testFormulaOne :: IO (CRepPtr ArithmeticFormula)
testFormulaOne = passFormula nullPtr Ar.one

testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)
testFormulaExample1 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula nullPtr $ Ar.Formula [Ar.Term 2 (Ar.Product [Ar.Factor addr 2])]

testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)
testFormulaExample2 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula nullPtr $ Ar.Formula [ Ar.Term 1 (Ar.Product []), Ar.Term 2 (Ar.Product [Ar.Factor addr 2, Ar.Factor addr 4]) ]

-- ** BinaryDumper Tests

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
