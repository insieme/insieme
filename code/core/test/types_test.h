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

#include <cxxtest/TestSuite.h>
#include "types.h"

class TypesTestSuite : public CxxTest::TestSuite  {
public:
	void testIntTypeParam( ) {
		// test size limitation
		TS_ASSERT_LESS_THAN_EQUALS (sizeof(IntTypeParam), 4);

		// test toString format
		IntTypeParam p12 = IntTypeParam::getConcreteIntParam(12);
		TS_ASSERT_EQUALS (p12.toString(), "12");

		IntTypeParam inf = IntTypeParam::getInfiniteIntParam();
		TS_ASSERT_EQUALS (inf.toString(), "Inf");

		IntTypeParam pvp = IntTypeParam::getVariableIntParam('p');
		TS_ASSERT_EQUALS (pvp.toString(), "p");

		// test isConcrete()
		TS_ASSERT_EQUALS (p12.isConcrete(), true);
		TS_ASSERT_EQUALS (inf.isConcrete(), true);
		TS_ASSERT_EQUALS (pvp.isConcrete(), false);

		// test == operator
		IntTypeParam params[] = {p12, inf, pvp};
		for (int i=0; i<3; i++) {
			for (int j=0; j<3; j++) {
				TS_ASSERT_EQUALS(params[i]==params[j], i==j);
			}
		}

		IntTypeParam p12b = IntTypeParam::getConcreteIntParam(12);
		TS_ASSERT (p12 == p12b);

		IntTypeParam pvpb = IntTypeParam::getVariableIntParam('p');
		TS_ASSERT (pvp == pvpb);

		IntTypeParam infb = IntTypeParam::getInfiniteIntParam();
		TS_ASSERT (inf == infb);
	}

	void testAbstractType() {

		AbstractTypePtr ref = AbstractType::getInstance();

		// testing name ...
		TS_ASSERT_EQUALS (ref->getName(), "abstract");
		TS_ASSERT_EQUALS (ref->toString(), "abstract");

		// testing singleton property
		AbstractTypePtr ref2 = AbstractType::getInstance();
		TS_ASSERT_EQUALS (ref, ref2);

		// check basic properties
		TS_ASSERT (ref->isConcrete());
		TS_ASSERT (!ref->isFunctionType());
	}

	// TODO: add further tests
};
