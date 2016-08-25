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

module Insieme.Utils.ParseInt where

import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Numeric (readOct, readDec, readHex)

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
    quotRem (CInt32  x) (CInt32  y) = (\(u, v) -> (CInt32  u, CInt32  v)) (quotRem x y)
    quotRem (CInt64  x) (CInt64  y) = (\(u, v) -> (CInt64  u, CInt64  v)) (quotRem x y)
    quotRem (CUInt32 x) (CUInt32 y) = (\(u, v) -> (CUInt32 u, CUInt32 v)) (quotRem x y)
    quotRem (CUInt64 x) (CUInt64 y) = (\(u, v) -> (CUInt64 u, CUInt64 v)) (quotRem x y)
    quotRem _           _           = error "Cannot mix types"

    toInteger (CInt32  x) = toInteger x
    toInteger (CInt64  x) = toInteger x
    toInteger (CUInt32 x) = toInteger x
    toInteger (CUInt64 x) = toInteger x

parseInt :: String -> Maybe CInt
parseInt input = (toCInt . normalizeSuffix) <$> (listToMaybe $ readInt input)
  where
    readInt :: String -> [(Integer, String)]
    readInt int =
        case int of
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
