module Insieme.Utils.ParseInt where

import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Numeric
import Unsafe.Coerce

data CInt = CInt   Int32
          | CLong  Int64
          | CUInt  Word32
          | CULong Word64
  deriving (Eq, Ord, Show)

instance Num CInt where
    (CInt   x) + (CInt   y) = CInt   $ x + y
    (CLong  x) + (CLong  y) = CLong  $ x + y
    (CUInt  x) + (CUInt  y) = CUInt  $ x + y
    (CULong x) + (CULong y) = CULong $ x + y
    _          + _          = error "Cannot mix types"

    (CInt   x) * (CInt   y) = CInt   $ x * y
    (CLong  x) * (CLong  y) = CLong  $ x * y
    (CUInt  x) * (CUInt  y) = CUInt  $ x * y
    (CULong x) * (CULong y) = CULong $ x * y
    _          * _          = error "Cannot mix types"

    abs (CInt   x) = CInt   $ abs x
    abs (CLong  x) = CLong  $ abs x
    abs (CUInt  x) = CUInt  $ abs x
    abs (CULong x) = CULong $ abs x

    signum (CInt   x) = CInt   $ signum x
    signum (CLong  x) = CLong  $ signum x
    signum (CUInt  x) = CUInt  $ signum x
    signum (CULong x) = CULong $ signum x

    fromInteger x = CInt (fromInteger x)

    negate (CInt   x) = CInt   $ negate x
    negate (CLong  x) = CLong  $ negate x
    negate (CUInt  x) = CUInt  $ negate x
    negate (CULong x) = CULong $ negate x

instance Enum CInt where
    fromEnum (CInt x) = fromIntegral x
    fromEnum _        = error "fromEnum only supported for CInt"

    toEnum = CInt . fromIntegral

instance Real CInt where
    toRational (CInt   x) = toRational x
    toRational (CLong  x) = toRational x
    toRational (CUInt  x) = toRational x
    toRational (CULong x) = toRational x

instance Integral CInt where
    quotRem (CInt   x) (CInt   y) = (\(x, y) -> (CInt   x, CInt   y)) (quotRem x y)
    quotRem (CLong  x) (CLong  y) = (\(x, y) -> (CLong  x, CLong  y)) (quotRem x y)
    quotRem (CUInt  x) (CUInt  y) = (\(x, y) -> (CUInt  x, CUInt  y)) (quotRem x y)
    quotRem (CULong x) (CULong y) = (\(x, y) -> (CULong x, CULong y)) (quotRem x y)
    quotRem _          _          = error "Cannot mix types"

    toInteger (CInt   x) = toInteger x
    toInteger (CLong  x) = toInteger x
    toInteger (CUInt  x) = toInteger x
    toInteger (CULong x) = toInteger x

parseInt :: String -> Maybe CInt
parseInt input = (toCInt . normalizeSuffix) <$> (listToMaybe . readInt $ input)
  where
    readInt input =
        case input of
            ('+':'0':'x':xs) -> readHex xs
            ('+':'0':[])     -> readDec "0"
            ('+':'0':xs)     -> readOct xs
            ('+':xs)         -> readDec xs
            ('-':'0':'x':xs) -> fmap invert (readHex xs)
            ('-':'0':[])     -> readDec "0"
            ('-':'0':xs)     -> fmap invert (readOct xs)
            ('-':xs)         -> fmap invert (readDec xs)
            ('0':'x':xs)     -> readHex xs
            ('0':[])         -> readDec "0"
            ('0':xs)         -> readOct xs
            _                -> readDec input

    toCInt pair = case pair of
        (num, "u")   -> CUInt  (fromIntegral num :: Word32)
        (num, "l")   -> CLong  (fromIntegral num :: Int64)
        (num, "lu")  -> CULong (fromIntegral num :: Word64)
        (num, "ll")  -> CLong  (fromIntegral num :: Int64)
        (num, "llu") -> CULong (fromIntegral num :: Word64)
        (num, _)     -> CInt   (fromIntegral num :: Int32)

    invert (num, suffix) = (-num, suffix)

    normalizeSuffix (num, suffix) = (num, sort $ map toLower suffix)
