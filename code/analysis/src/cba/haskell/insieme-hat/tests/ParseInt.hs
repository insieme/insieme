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
