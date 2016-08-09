module Insieme.Utils.ParseInt where

import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Numeric

data CInt = CInt32  Int32
          | CInt64  Int64
          | CUInt32 Word32
          | CUInt64 Word64
  deriving (Eq, Ord)

instance Show CInt where
    show (CInt32  x) = show x
    show (CInt64  x) = show x
    show (CUInt32 x) = show x
    show (CUInt64 x) = show x

instance Num CInt where
    (CInt32  x) + (CInt32  y) = CInt32  $ x + y
    (CInt64  x) + (CInt64  y) = CInt64  $ x + y
    (CUInt32 x) + (CUInt32 y) = CUInt32 $ x + y
    (CUInt64 x) + (CUInt64 y) = CUInt64 $ x + y
    _           + _           = error "Cannot mix types"

    (CInt32  x) * (CInt32  y) = CInt32  $ x * y
    (CInt64  x) * (CInt64  y) = CInt64  $ x * y
    (CUInt32 x) * (CUInt32 y) = CUInt32 $ x * y
    (CUInt64 x) * (CUInt64 y) = CUInt64 $ x * y
    _           * _           = error "Cannot mix types"

    abs (CInt32  x) = CInt32  $ abs x
    abs (CInt64  x) = CInt64  $ abs x
    abs (CUInt32 x) = CUInt32 $ abs x
    abs (CUInt64 x) = CUInt64 $ abs x

    signum (CInt32  x) = CInt32  $ signum x
    signum (CInt64  x) = CInt64  $ signum x
    signum (CUInt32 x) = CUInt32 $ signum x
    signum (CUInt64 x) = CUInt64 $ signum x

    fromInteger x = CInt32 (fromInteger x)

    negate (CInt32  x) = CInt32  $ negate x
    negate (CInt64  x) = CInt64  $ negate x
    negate (CUInt32 x) = CUInt32 $ negate x
    negate (CUInt64 x) = CUInt64 $ negate x

instance Enum CInt where
    fromEnum (CInt32 x) = fromIntegral x
    fromEnum _          = error "fromEnum only supported for CInt"

    toEnum = CInt32 . fromIntegral

instance Real CInt where
    toRational (CInt32  x) = toRational x
    toRational (CInt64  x) = toRational x
    toRational (CUInt32 x) = toRational x
    toRational (CUInt64 x) = toRational x

instance Integral CInt where
    quotRem (CInt32  x) (CInt32  y) = (\(x, y) -> (CInt32  x, CInt32  y)) (quotRem x y)
    quotRem (CInt64  x) (CInt64  y) = (\(x, y) -> (CInt64  x, CInt64  y)) (quotRem x y)
    quotRem (CUInt32 x) (CUInt32 y) = (\(x, y) -> (CUInt32 x, CUInt32 y)) (quotRem x y)
    quotRem (CUInt64 x) (CUInt64 y) = (\(x, y) -> (CUInt64 x, CUInt64 y)) (quotRem x y)
    quotRem _           _           = error "Cannot mix types"

    toInteger (CInt32  x) = toInteger x
    toInteger (CInt64  x) = toInteger x
    toInteger (CUInt32 x) = toInteger x
    toInteger (CUInt64 x) = toInteger x

parseInt :: String -> Maybe CInt
parseInt input = (toCInt . normalizeSuffix) <$> (listToMaybe . readInt $ input)
  where
    readInt input =
        case input of
            ('+':'0':'x':xs) -> readHex xs
            ('+':'0':xs)     -> readOct ("0" ++ xs)
            ('+':xs)         -> readDec xs
            ('-':'0':'x':xs) -> invert <$> readHex xs
            ('-':'0':xs)     -> invert <$> readOct ("0" ++ xs)
            ('-':xs)         -> invert <$> readDec xs
            ('0':'x':xs)     -> readHex xs
            ('0':xs)         -> readOct ("0" ++ xs)
            _                -> readDec input

    toCInt pair = case pair of
        (num, "u")   -> CUInt32 (fromIntegral num :: Word32)
        (num, "l")   -> CInt64  (fromIntegral num :: Int64)
        (num, "lu")  -> CUInt64 (fromIntegral num :: Word64)
        (num, "ll")  -> CInt64  (fromIntegral num :: Int64)
        (num, "llu") -> CUInt64 (fromIntegral num :: Word64)
        (num, _)     -> CInt32  (fromIntegral num :: Int32)

    invert (num, suffix) = (-num, suffix)

    normalizeSuffix (num, suffix) = (num, sort $ map toLower suffix)
