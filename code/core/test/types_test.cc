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
#include "types.h"


TEST(TypesTest, IntTypeParam) {
#ifndef WIN32
	// test size limitation
	EXPECT_LE (sizeof(IntTypeParam), 4);
#endif

	// test toString format
	IntTypeParam p12 = IntTypeParam::getConcreteIntParam(12);
	EXPECT_EQ (p12.toString(), "12");

	IntTypeParam inf = IntTypeParam::getInfiniteIntParam();
	EXPECT_EQ (inf.toString(), "Inf");

	IntTypeParam pvp = IntTypeParam::getVariableIntParam('p');
	EXPECT_EQ (pvp.toString(), "p");

	// test isConcrete()
	EXPECT_EQ (p12.isConcrete(), true);
	EXPECT_EQ (inf.isConcrete(), true);
	EXPECT_EQ (pvp.isConcrete(), false);

	// test == operator
	IntTypeParam params[] = {p12, inf, pvp};
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {
			EXPECT_EQ(params[i]==params[j], i==j);
		}
	}

	IntTypeParam p12b = IntTypeParam::getConcreteIntParam(12);
	EXPECT_TRUE (p12 == p12b);

	IntTypeParam pvpb = IntTypeParam::getVariableIntParam('p');
	EXPECT_TRUE (pvp == pvpb);

	IntTypeParam infb = IntTypeParam::getInfiniteIntParam();
	EXPECT_TRUE (inf == infb);
}

TEST(TypesTest, AbstractType) {
	AbstractTypePtr ref = AbstractType::getInstance();

	// testing name ...
	EXPECT_EQ (ref->getName(), "abstract");
	EXPECT_EQ (ref->toString(), "abstract");

	// testing singleton property
	AbstractTypePtr ref2 = AbstractType::getInstance();
	EXPECT_EQ (ref, ref2);

	// check basic properties
	EXPECT_TRUE (ref->isConcrete());
	EXPECT_FALSE (ref->isFunctionType());
}

