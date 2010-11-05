/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include <gtest/gtest.h>

#include <iostream>
#include "insieme/utils/enum.h"

using std::string;
using std::vector;

/* The following enums need to be defined outside of any function.
*/

//Normal, 'long' one
ENUM(testCard, EINS, ZWEI, DREI, VIER, FUENF, SECHS, SIEBEN, ACHT, NEUN, ZEHN, BUBE, DAME, KOENIG, AS)

//Two elements
ENUM(testInt, ZERO, ONE)

//One element
//ENUM(testItalianInt, UNO)

//Long word and underscore
ENUM(testLongElement, VERDORRE_IN_DIE_KISTEUNDZUGENAEHT_nochmal,     _KILLEM)



TEST(Enums, LongString) {
    testCard eins = EINS;
    testCard zwei = ZWEI;
    testCard koenig = KOENIG;
    testCard as = AS;

    //The strings should be the identifier and the ord should be counting
    //Note: Though tested here, it could be considered as implementation detail
    EXPECT_EQ("EINS", name(eins));
    EXPECT_EQ(static_cast<unsigned>(0), ord(eins));

    EXPECT_EQ("ZWEI", name(zwei));
    EXPECT_EQ(static_cast<unsigned>(1), ord(zwei));

    EXPECT_EQ("KOENIG", name(koenig));
    EXPECT_EQ(static_cast<unsigned>(12), ord(koenig));

    EXPECT_EQ("AS", name(as));
    EXPECT_EQ(static_cast<unsigned>(13), ord(as));

    //Those values should be seperate!
    EXPECT_NE(eins, zwei);
    EXPECT_NE(eins, koenig);
    EXPECT_NE(eins, as);
    EXPECT_NE(zwei, koenig);
    EXPECT_NE(zwei, as);
    EXPECT_NE(koenig, as);
}

//Tests wether a two-elements enum has two different elements and the string-things are working.
TEST(Enums, TwoElements) {
    testInt z = ZERO;
    testInt o = ONE;

    EXPECT_EQ("ZERO", name(z));
    EXPECT_EQ(static_cast<unsigned>(0), ord(z));

    EXPECT_EQ("ONE", name(o));
    EXPECT_EQ(static_cast<unsigned>(1), ord(o));

    EXPECT_NE(z, o);
}

//Would test for an one-element enum.
/*TEST(Enums, OneElement) {

    testItalianInt u1 = UNO;
    testItalianInt u2 = UNO;

    EXPECT_EQ("UNO", name(u1));
    EXPECT_EQ(0, ord(u1));

    EXPECT_EQ("UNO", name(u2));
    EXPECT_EQ(0, ord(u2));

    EXPECT_EQ(u1, u2);
}*/

//Tests a long enum word. Just for being sure. :-)
TEST(Enums, LongWord) {
    testLongElement t = (testLongElement) 0;
    EXPECT_EQ("VERDORRE_IN_DIE_KISTEUNDZUGENAEHT_nochmal", name(t));
}

//Tests wether the first character may be underscore and may have prefix whitespaces
TEST(Enums, UnderScore) {
    testLongElement t = _KILLEM;
    EXPECT_EQ("_KILLEM", name(t));
}

//Tests the counting
TEST(Enums, Counting) {
    testCard tc;
    EXPECT_EQ(static_cast<unsigned>(14), count(tc));
}

//Tests the tc++
TEST(Enums, ForLoopPP) {
    std::size_t j = 0;
    for(testCard tc = min(tc); tc != max(tc); tc++) {
        j++;
    }
    EXPECT_EQ(static_cast<unsigned>(14), j);
}

//Tests the ++tc
TEST(Enums, PPForLoop) {
    std::size_t j = 0;
    for(testCard tc = min(tc); tc != max(tc); ++tc) {
        j++;
    }
    EXPECT_EQ(static_cast<unsigned>(14), j);
}

//Tests the tc--
TEST(Enums, ForLoopMM) {
    std::size_t j = 0;
    for(testCard tc = max(tc); tc-- != min(tc); ) {
        j++;
    }
    EXPECT_EQ(static_cast<unsigned>(14), j);
}

//Tests the --tc
TEST(Enums, MMForLoop) {
    std::size_t j = 0;
    for(testCard tc = max(tc); --tc >= min(tc); ) {
        j++;
    }
    EXPECT_EQ(static_cast<unsigned>(14), j);
}

//Tests the function fromName
TEST(Enums, FromName) {
	//Should find
    testCard tc1 = fromName<testCard>("KOENIG");
    EXPECT_EQ(KOENIG, tc1);

    //Should not find
    //TODO: Should it better throw an exception?
    testCard tc2 = fromName<testCard>("KNIGGE");
    EXPECT_EQ(max(tc2), tc2);

    testInt z = fromName<testInt>("KOENIG");
    testInt o = fromName<testInt>("ONE");
    EXPECT_NE(z, o);
    //EXPECT_EQ(z, testIntMIN); //WTF: '0' breaked the gTest Suite!
    EXPECT_EQ(z, max(z));
    EXPECT_EQ(static_cast<unsigned>(1), o);
}
