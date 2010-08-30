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
#include "enums.h"

using std::string;
using std::vector;

/* The following enums need to be defined outside of some function.
*/

//Normal, 'long' one
ENUM(testCard, EINS, ZWEI, DREI, VIER, FUENF, SECHS, SIEBEN, ACHT, NEUN, ZEHN, BUBE, DAME, KOENIG, AS)

//Two elements
ENUM(testInt, ZERO, ONE)

//One element
//ENUM(testItalianInt, UNO)

//Long word and underscore
ENUM(testLongElement,    VERDORRE_IN_DIE_KISTEUNDZUGENAEHT_nochmal,     _KILLEM)



TEST(Enums, LongString) {
    testCard eins = EINS;
    testCard zwei = ZWEI;
    testCard koenig = KOENIG;
    testCard as = AS;

    //The strings should be the identifier and the ord should be counting
    //Note: Though tested here, it could be considered as implementation detail
    EXPECT_EQ(name(eins), "EINS");
    EXPECT_EQ(ord(eins),  0);

    EXPECT_EQ(name(zwei), "ZWEI");
    EXPECT_EQ(ord(zwei),  1);

    EXPECT_EQ(name(koenig), "KOENIG");
    EXPECT_EQ(ord(koenig),  12);

    EXPECT_EQ(name(as), "AS");
    EXPECT_EQ(ord(as),  13);

    //Those values should be seperate!
    EXPECT_NE(eins, zwei);
    EXPECT_NE(eins, koenig);
    EXPECT_NE(eins, as);
    EXPECT_NE(zwei, koenig);
    EXPECT_NE(zwei, as);
    EXPECT_NE(koenig, as);
}

TEST(Enums, TwoElements) {
    testInt z = ZERO;
    testInt o = ONE;

    EXPECT_EQ(name(z), "ZERO");
    EXPECT_EQ(ord(z),  0);

    EXPECT_EQ(name(o), "ONE");
    EXPECT_EQ(ord(o),  1);

    EXPECT_NE(z, o);
}

/*TEST(Enums, OneElement) {

    testItalianInt u1 = UNO;
    testItalianInt u2 = UNO;

    EXPECT_EQ(name(u1), "UNO");
    EXPECT_EQ(ord(u1),  0);

    EXPECT_EQ(name(u2), "UNO");
    EXPECT_EQ(ord(u2),  0);

    EXPECT_EQ(u1, u2);"_KILLEM"
}*/

TEST(Enums, LongWord) {
    testLongElement t = (testLongElement) 0;
    EXPECT_EQ("VERDORRE_IN_DIE_KISTEUNDZUGENAEHT_nochmal", name(t));
}

TEST(Enums, UnderScore) {
    testLongElement t = _KILLEM;
    EXPECT_EQ("_KILLEM", name(t));
}

TEST(Enums, ForLoopPP) {
    std::size_t j = 0;
    for(testCard tc = min(tc); tc != max(tc); tc++) {
        j++;
    }
    EXPECT_EQ(14, j);
}

TEST(Enums, PPForLoop) {
    std::size_t j = 0;
    for(testCard tc = min(tc); tc != max(tc); ++tc) {
        j++;
    }
    EXPECT_EQ(14, j);
}

/* Das wird wohl eher nicht funktionieren (max(tc) - 1)
TEST(Enums, ForLoopMM) {
    std::size_t j = 0;
    for(testCard tc = max(tc) - 1; tc-- != min(tc); ) {
        j++;
    }
    EXPECT_EQ(14, j);
}*/

TEST(Enums, MMForLoop) {
    std::size_t j = 0;
    for(testCard tc = max(tc); --tc >= min(tc); ) {
        j++;
    }
    EXPECT_EQ(14, j);
}

