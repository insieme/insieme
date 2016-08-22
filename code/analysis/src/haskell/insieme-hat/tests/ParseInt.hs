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
