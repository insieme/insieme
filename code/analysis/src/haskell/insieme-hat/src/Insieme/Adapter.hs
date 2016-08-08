{-# LANGUAGE ForeignFunctionInterface #-}

module Insieme.Adapter where

import Control.Monad
import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Tree
import Debug.Trace
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Arithmetic as AnArith
import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Arithmetic as Arith
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Utils as IRUtils
import qualified Insieme.Utils.BoundSet as BSet

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

checkBoolean :: StablePtr Addr.NodeAddress -> IO (CInt)
checkBoolean addr_c = handleAll (return . fromIntegral . fromEnum $ AnBoolean.Both) $ do
    addr <- deRefStablePtr addr_c
    evaluate . fromIntegral . fromEnum . Solver.resolve . AnBoolean.booleanValue $ addr

foreign export ccall "hat_checkBoolean"
    checkBoolean :: StablePtr Addr.NodeAddress -> IO (CInt)

arithValue :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticSet)
arithValue addr_c = do
    addr <- deRefStablePtr addr_c
    let results = Solver.resolve (AnArith.arithmeticValue addr)
    passFormulaSet $ BSet.map (fmap AnArith.getAddr) results

foreign export ccall "hat_arithmeticValue"
    arithValue :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticSet)


--
-- * Arithemtic
--

type CArithmeticValue = ()
type CArithmeticFactor = ()
type CArithmeticProduct = ()
type CArithmeticTerm = ()
type CArithmeticFormula = ()
type CArithmeticSet = ()

foreign import ccall "hat_arithmetic_value"
    arithmeticValue :: Ptr CSize -> CSize -> IO (Ptr CArithmeticValue)

foreign import ccall "hat_arithemtic_factor"
    arithemticFactor :: Ptr CArithmeticValue -> CInt -> IO (Ptr CArithmeticFactor)

foreign import ccall "hat_arithmetic_product"
    arithmeticProduct :: Ptr (Ptr CArithmeticFactor) -> CSize -> IO (Ptr CArithmeticProduct)

foreign import ccall "hat_arithmetic_term"
    arithmeticTerm :: Ptr CArithmeticProduct -> CULong -> IO (Ptr CArithmeticTerm)

foreign import ccall "hat_arithmetic_formula"
    arithmeticFormula :: Ptr (Ptr CArithmeticTerm) -> CSize -> IO (Ptr CArithmeticFormula)

foreign import ccall "hat_arithmetic_set"
    arithmeticSet :: Ptr (Ptr CArithmeticFormula) -> CInt -> IO (Ptr CArithmeticSet)


passFormula :: Integral c => Arith.Formula c Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
passFormula formula = do
    terms <- forM (Arith.terms formula) passTerm
    withArrayLen' terms arithmeticFormula
  where
    passTerm :: Integral c => Arith.Term c Addr.NodeAddress -> IO (Ptr CArithmeticTerm)
    passTerm term = do
        product <- passProduct (Arith.product term)
        arithmeticTerm product (fromIntegral $ Arith.coeff term)

    passProduct :: Integral c => Arith.Product c Addr.NodeAddress -> IO (Ptr CArithmeticProduct)
    passProduct product = do
        factors <- forM (Arith.factors product) passFactor
        withArrayLen' factors arithmeticProduct

    passFactor :: Integral c => Arith.Factor c Addr.NodeAddress -> IO (Ptr CArithmeticFactor)
    passFactor factor = do
        value <- passValue (Arith.base factor)
        arithemticFactor value (fromIntegral $ Arith.exponent factor)

    passValue :: Addr.NodeAddress -> IO (Ptr CArithmeticValue)
    passValue addr = withArrayLen' (fromIntegral <$> Addr.getAddress addr) arithmeticValue

    withArrayLen' :: Storable a => [a] -> (Ptr a -> CSize -> IO b) -> IO b
    withArrayLen' xs f = withArrayLen xs (\s a -> f a (fromIntegral s))


passFormulaSet :: Integral c => BSet.BoundSet bb (Arith.Formula c Addr.NodeAddress)
               -> IO (Ptr CArithmeticSet)
passFormulaSet BSet.Universe = arithmeticSet nullPtr (-1)
passFormulaSet bs = do
    formulas <- mapM passFormula (BSet.toList bs)
    withArrayLen' formulas arithmeticSet
  where
    withArrayLen' :: Storable a => [a] -> (Ptr a -> CInt -> IO b) -> IO b
    withArrayLen' xs f = withArrayLen xs (\s a -> f a (fromIntegral s))


--
-- * Arithmetic Tests
--

testFormulaZero :: IO (Ptr CArithmeticFormula)
testFormulaZero = passFormula Arith.zero

foreign export ccall "hat_test_formulaZero"
    testFormulaZero :: IO (Ptr CArithmeticFormula)


testFormulaOne :: IO (Ptr CArithmeticFormula)
testFormulaOne = passFormula Arith.one

foreign export ccall "hat_test_formulaOne"
    testFormulaOne :: IO (Ptr CArithmeticFormula)


testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
testFormulaExample1 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula $ Arith.Formula [Arith.Term 2 (Arith.Product [Arith.Factor addr 2])]

foreign export ccall "hat_test_formulaExample1"
    testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)


testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)
testFormulaExample2 addr_c = do
    addr <- deRefStablePtr addr_c
    passFormula $ Arith.Formula [ Arith.Term 1 (Arith.Product []), Arith.Term 2 (Arith.Product [Arith.Factor addr 2, Arith.Factor addr 4]) ]

foreign export ccall "hat_test_formulaExample2"
    testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (Ptr CArithmeticFormula)


--
-- * Utilities
--

handleAll :: IO a -> IO a -> IO a
handleAll dummy action = catch action $ \e -> do
    putStrLn $ "Exception: " ++ show (e :: SomeException)
    dummy
