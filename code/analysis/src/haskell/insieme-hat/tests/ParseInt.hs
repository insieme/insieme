module ParseInt (parseIntTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Insieme.Utils.ParseInt

parseIntTests = testGroup "ParseInt" [decs, octs, hexs]

decs = testGroup "Decimals"
    [ check "0"      $ Just (CInt32  0)
    , check "0l"     $ Just (CInt64  0)
    , check "0u"     $ Just (CUInt32 0)
    , check "0ul"    $ Just (CUInt64 0)
    , check "+0"     $ Just (CInt32  0)
    , check "+0l"    $ Just (CInt64  0)
    , check "+0u"    $ Just (CUInt32 0)
    , check "+0ull"  $ Just (CUInt64 0)
    , check "-0"     $ Just (CInt32  0)
    , check "-0l"    $ Just (CInt64  0)
    , check "-0u"    $ Just (CUInt32 0)
    , check "-0ull"  $ Just (CUInt64 0)

    , check "123"    $ Just (CInt32  123)
    , check "123L"   $ Just (CInt64  123)
    , check "123U"   $ Just (CUInt32 123)
    , check "123UL"  $ Just (CUInt64 123)
    , check "+123"   $ Just (CInt32  123)
    , check "+123LL" $ Just (CInt64  123)
    , check "+123U"  $ Just (CUInt32 123)
    , check "+123UL" $ Just (CUInt64 123)
    , check "-123"   $ Just (CInt32  (-123))
    , check "-123Ll" $ Just (CInt64  (-123))
    , check "-123U"  $ Just (CUInt32 (-123))
    , check "-123Ul" $ Just (CUInt64 (-123))
    ]

octs = testGroup "Octals"
    [ check "00"      $ Just (CInt32  0)
    , check "00l"     $ Just (CInt64  0)
    , check "00u"     $ Just (CUInt32 0)
    , check "00ul"    $ Just (CUInt64 0)
    , check "+00"     $ Just (CInt32  0)
    , check "+00l"    $ Just (CInt64  0)
    , check "+00u"    $ Just (CUInt32 0)
    , check "+00ul"   $ Just (CUInt64 0)
    , check "-00"     $ Just (CInt32  0)
    , check "-00l"    $ Just (CInt64  0)
    , check "-00u"    $ Just (CUInt32 0)
    , check "-00ul"   $ Just (CUInt64 0)

    , check "0123"    $ Just (CInt32  83)
    , check "0123l"   $ Just (CInt64  83)
    , check "0123u"   $ Just (CUInt32 83)
    , check "0123ul"  $ Just (CUInt64 83)
    , check "+0123"   $ Just (CInt32  83)
    , check "+0123l"  $ Just (CInt64  83)
    , check "+0123u"  $ Just (CUInt32 83)
    , check "+0123ul" $ Just (CUInt64 83)
    , check "-0123"   $ Just (CInt32  (-83))
    , check "-0123l"  $ Just (CInt64  (-83))
    , check "-0123u"  $ Just (CUInt32 (-83))
    , check "-0123ul" $ Just (CUInt64 (-83))
    ]

hexs = testGroup "Hexadecimals"
    [ check "0x0"      $ Just (CInt32  0)
    , check "0x0l"     $ Just (CInt64  0)
    , check "0x0u"     $ Just (CUInt32 0)
    , check "0x0ul"    $ Just (CUInt64 0)
    , check "+0x0"     $ Just (CInt32  0)
    , check "+0x0l"    $ Just (CInt64  0)
    , check "+0x0u"    $ Just (CUInt32 0)
    , check "+0x0ul"   $ Just (CUInt64 0)
    , check "-0x0"     $ Just (CInt32  0)
    , check "-0x0l"    $ Just (CInt64  0)
    , check "-0x0u"    $ Just (CUInt32 0)
    , check "-0x0ul"   $ Just (CUInt64 0)

    , check "0x123"    $ Just (CInt32  291)
    , check "0x123l"   $ Just (CInt64  291)
    , check "0x123u"   $ Just (CUInt32 291)
    , check "0x123ul"  $ Just (CUInt64 291)
    , check "+0x123"   $ Just (CInt32  291)
    , check "+0x123l"  $ Just (CInt64  291)
    , check "+0x123u"  $ Just (CUInt32 291)
    , check "+0x123ul" $ Just (CUInt64 291)
    , check "-0x123"   $ Just (CInt32  (-291))
    , check "-0x123l"  $ Just (CInt64  (-291))
    , check "-0x123u"  $ Just (CUInt32 (-291))
    , check "-0x123ul" $ Just (CUInt64 (-291))

    , check "0x1b3"    $ Just (CInt32  435)
    , check "0x1B3l"   $ Just (CInt64  435)
    , check "0x1b3u"   $ Just (CUInt32 435)
    , check "0x1B3ul"  $ Just (CUInt64 435)
    , check "+0x1b3"   $ Just (CInt32  435)
    , check "+0x1B3l"  $ Just (CInt64  435)
    , check "+0x1b3u"  $ Just (CUInt32 435)
    , check "+0x1B3ul" $ Just (CUInt64 435)
    , check "-0x1b3"   $ Just (CInt32  (-435))
    , check "-0x1B3l"  $ Just (CInt64  (-435))
    , check "-0x1b3u"  $ Just (CUInt32 (-435))
    , check "-0x1B3ul" $ Just (CUInt64 (-435))
    ]

check input expect = testCase input $ parseInt input @?= expect
