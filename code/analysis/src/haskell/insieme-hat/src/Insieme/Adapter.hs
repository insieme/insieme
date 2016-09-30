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

{-# LANGUAGE ForeignFunctionInterface #-}

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
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Visit as Visit
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

--
-- * HSobject
--

foreign export ccall "hat_freeStablePtr"
    freeStablePtr :: StablePtr a -> IO ()

--
-- * Context
--

initializeContext :: Ctx.CContext -> CString -> CSize -> IO (StablePtr Ctx.Context)
initializeContext context_c dump_c size_c = do
    dump <- BS8.packCStringLen (dump_c, fromIntegral size_c)
    let Right ir = BinPar.parseBinaryDump dump
    newStablePtr $ Ctx.mkContext context_c ir

foreign export ccall "hat_initialize_context"
    initializeContext :: Ctx.CContext -> CString -> CSize
                      -> IO (StablePtr Ctx.Context)


dumpStatistics :: StablePtr Ctx.Context -> IO ()
dumpStatistics ctx_hs = do
    ctx <- deRefStablePtr ctx_hs
    putStrLn $ Solver.showSolverStatistic $ Ctx.getSolverState ctx

foreign export ccall "hat_print_statistic"
    dumpStatistics :: StablePtr Ctx.Context -> IO ()


dumpAssignment :: StablePtr Ctx.Context -> IO ()
dumpAssignment ctx_hs = do
    ctx <- deRefStablePtr ctx_hs
    putStrLn $ Solver.dumpSolverState True (Ctx.getSolverState ctx) "graph"

foreign export ccall "hat_dump_assignment"
    dumpAssignment :: StablePtr Ctx.Context -> IO ()


mkNodeAddress :: StablePtr Ctx.Context -> Ptr CSize -> CSize
              -> IO (StablePtr Addr.NodeAddress)
mkNodeAddress ctx_hs path_c length_c = do
    ctx  <- deRefStablePtr ctx_hs
    path <- peekArray (fromIntegral length_c) path_c
    newStablePtr $ Addr.mkNodeAddress (fromIntegral <$> path) (Ctx.getInspire ctx)

foreign export ccall "hat_mk_node_address"
    mkNodeAddress :: StablePtr Ctx.Context -> Ptr CSize -> CSize
                  -> IO (StablePtr Addr.NodeAddress)

nodePathLength :: StablePtr Addr.NodeAddress -> IO CSize
nodePathLength addr_hs = do
    addr <- deRefStablePtr addr_hs
    return $ fromIntegral $ length $ Addr.getAbsolutePath addr

foreign export ccall "hat_node_path_length"
    nodePathLength :: StablePtr Addr.NodeAddress -> IO CSize

nodePathPoke :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()
nodePathPoke addr_hs path_c = do
    addr <- deRefStablePtr addr_hs
    pokeArray path_c $ fromIntegral <$> Addr.getAbsolutePath addr

foreign export ccall "hat_node_path_poke"
    nodePathPoke :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()

foreign import ccall "hat_update_context"
    updateContext :: Ctx.CContext -> StablePtr Ctx.Context -> IO ()


--
-- * Analysis
--

findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)
findDecl var_hs = do
    var <- deRefStablePtr var_hs
    case Visit.findDecl var of
        Nothing -> return $ castPtrToStablePtr nullPtr
        Just a  -> newStablePtr a

foreign export ccall "hat_find_declaration"
    findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)

checkBoolean :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (CInt)
checkBoolean ctx_hs expr_hs = handleAll (return $ fromIntegral $ fromEnum AnBoolean.Both) $ do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) $ AnBoolean.booleanValue expr
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    evaluate $ fromIntegral $ fromEnum $ ComposedValue.toValue res
            

foreign export ccall "hat_check_boolean"
    checkBoolean :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (CInt)

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

foreign export ccall "hat_check_alias"
    checkAlias :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> StablePtr Addr.NodeAddress -> IO CInt

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



--
-- * Memory Locations
--

type CMemoryLocation = ()
type CMemoryLocationSet = ()

foreign import ccall "hat_mk_memory_location"
    memoryLocation :: Ctx.CContext -> Ptr CSize -> CSize -> IO (Ptr CMemoryLocation)

foreign import ccall "hat_mk_memory_location_set"
    memoryLocationSet :: Ptr (Ptr CMemoryLocation) -> CInt -> IO (Ptr CMemoryLocationSet)

foreign export ccall "hat_memory_locations"
    memoryLocations :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress 
                    -> IO (Ptr CMemoryLocationSet)


memoryLocations :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (Ptr CMemoryLocationSet)
memoryLocations ctx_hs expr_hs = do
    ctx <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Ref.referenceValue expr)
    let results = (ComposedValue.toValue res) :: Ref.ReferenceSet FieldIndex.SimpleFieldIndex
    let ctx_c = Ctx.getCContext ctx
    let hasUnknownSources = BSet.member Ref.NullReference results || BSet.member Ref.UninitializedReference results  
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    passMemoryLocationSet ctx_c $ if hasUnknownSources then BSet.Universe else BSet.map Ref.creationPoint results

passMemoryLocation :: Ctx.CContext -> Ref.Location -> IO (Ptr CMemoryLocation)
passMemoryLocation ctx_c location_hs = do
    passLoc location_hs
  where
    passLoc :: Ref.Location -> IO (Ptr CMemoryLocation)
    passLoc loc_hs = withArrayLen' (fromIntegral <$> Addr.getAbsolutePath loc_hs) (memoryLocation ctx_c)
    withArrayLen' :: Storable a => [a] -> (Ptr a -> CSize -> IO b) -> IO b
    withArrayLen' xs f = withArrayLen xs (\s a -> f a (fromIntegral s))


passMemoryLocationSet :: Ctx.CContext -> BSet.UnboundSet Ref.Location
               -> IO (Ptr CMemoryLocationSet)
passMemoryLocationSet _ BSet.Universe = memoryLocationSet nullPtr (-1)
passMemoryLocationSet ctx_c bs = do
    locations <- mapM (passMemoryLocation ctx_c) (BSet.toList bs)
    withArrayLen' locations memoryLocationSet
  where
    withArrayLen' :: Storable a => [a] -> (Ptr a -> CInt -> IO b) -> IO b
    withArrayLen' xs f = withArrayLen xs (\s a -> f a (fromIntegral s))


--
-- * Arithemtic
--

type CArithmeticValue = ()
type CArithmeticFactor = ()
type CArithmeticProduct = ()
type CArithmeticTerm = ()
type CArithmeticFormula = ()
type CArithmeticSet = ()

foreign import ccall "hat_mk_arithmetic_value"
    arithmeticValue :: Ctx.CContext -> Ptr CSize -> CSize -> IO (Ptr CArithmeticValue)

foreign import ccall "hat_mk_arithemtic_factor"
    arithemticFactor :: Ptr CArithmeticValue -> CInt -> IO (Ptr CArithmeticFactor)

foreign import ccall "hat_mk_arithmetic_product"
    arithmeticProduct :: Ptr (Ptr CArithmeticFactor) -> CSize -> IO (Ptr CArithmeticProduct)

foreign import ccall "hat_mk_arithmetic_term"
    arithmeticTerm :: Ptr CArithmeticProduct -> CULong -> IO (Ptr CArithmeticTerm)

foreign import ccall "hat_mk_arithmetic_formula"
    arithmeticFormula :: Ptr (Ptr CArithmeticTerm) -> CSize -> IO (Ptr CArithmeticFormula)

foreign import ccall "hat_mk_arithmetic_set"
    arithmeticSet :: Ptr (Ptr CArithmeticFormula) -> CInt -> IO (Ptr CArithmeticSet)

foreign export ccall "hat_arithmetic_value"
    arithValue :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress
               -> IO (Ptr CArithmeticSet)

passFormula :: Integral c => Ctx.CContext -> Ar.Formula c Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
passFormula ctx_c formula_hs = do
    terms_c <- forM (Ar.terms formula_hs) passTerm
    withArrayLen' terms_c arithmeticFormula
  where
    passTerm :: Integral c => Ar.Term c Addr.NodeAddress -> IO (Ptr CArithmeticTerm)
    passTerm term_hs = do
        product_c <- passProduct (Ar.product term_hs)
        arithmeticTerm product_c (fromIntegral $ Ar.coeff term_hs)

    passProduct :: Integral c => Ar.Product c Addr.NodeAddress -> IO (Ptr CArithmeticProduct)
    passProduct product_hs = do
        factors_c <- forM (Ar.factors product_hs) passFactor
        withArrayLen' factors_c arithmeticProduct

    passFactor :: Integral c => Ar.Factor c Addr.NodeAddress -> IO (Ptr CArithmeticFactor)
    passFactor factor_hs = do
        value_c <- passValue (Ar.base factor_hs)
        arithemticFactor value_c (fromIntegral $ Ar.exponent factor_hs)

    passValue :: Addr.NodeAddress -> IO (Ptr CArithmeticValue)
    passValue addr_hs = withArrayLen' (fromIntegral <$> Addr.getAbsolutePath addr_hs) (arithmeticValue ctx_c)

    withArrayLen' :: Storable a => [a] -> (Ptr a -> CSize -> IO b) -> IO b
    withArrayLen' xs f = withArrayLen xs (\s a -> f a (fromIntegral s))


passFormulaSet :: Integral c => Ctx.CContext -> BSet.BoundSet bb (Ar.Formula c Addr.NodeAddress)
               -> IO (Ptr CArithmeticSet)
passFormulaSet _ BSet.Universe = arithmeticSet nullPtr (-1)
passFormulaSet ctx_c bs = do
    formulas <- mapM (passFormula ctx_c) (BSet.toList bs)
    withArrayLen' formulas arithmeticSet
  where
    withArrayLen' :: Storable a => [a] -> (Ptr a -> CInt -> IO b) -> IO b
    withArrayLen' xs f = withArrayLen xs (\s a -> f a (fromIntegral s))


--
-- * Arithmetic Tests
--

testFormulaZero :: IO (Ptr CArithmeticFormula)
testFormulaZero = passFormula nullPtr Ar.zero

foreign export ccall "hat_test_formulaZero"
    testFormulaZero :: IO (Ptr CArithmeticFormula)


testFormulaOne :: IO (Ptr CArithmeticFormula)
testFormulaOne = passFormula nullPtr Ar.one

foreign export ccall "hat_test_formulaOne"
    testFormulaOne :: IO (Ptr CArithmeticFormula)


testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
testFormulaExample1 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula nullPtr $ Ar.Formula [Ar.Term 2 (Ar.Product [Ar.Factor addr 2])]

foreign export ccall "hat_test_formulaExample1"
    testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)


testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
testFormulaExample2 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula nullPtr $ Ar.Formula [ Ar.Term 1 (Ar.Product []), Ar.Term 2 (Ar.Product [Ar.Factor addr 2, Ar.Factor addr 4]) ]

foreign export ccall "hat_test_formulaExample2"
    testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)


--
-- * Utilities
--

handleAll :: IO a -> IO a -> IO a
handleAll dummy action = catch action $ \e -> do
    putStrLn $ "Exception: " ++ show (e :: SomeException)
    dummy
