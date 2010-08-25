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

#include <string>
#include <boost/functional/hash.hpp>

#include "identifiers.h"
#include "container_utils.h"


TEST(TypeTest, DuplicateTest) {

	// check names
	Identifier identA("A");
	EXPECT_EQ ("A" , identA.getName());

	Identifier identB("B");
	EXPECT_EQ ("B" , identB.getName());

	Identifier identA2("A");
	EXPECT_EQ ("A" , identA2.getName());

	// check equality operator
	EXPECT_NE ( identA, identB );
	EXPECT_EQ ( identA, identA2 );
	EXPECT_NE ( identB, identA2 );

	EXPECT_NE ( identB, identA );
	EXPECT_EQ ( identA2, identA );
	EXPECT_NE ( identA2, identB );

	// check hash
	boost::hash<Identifier> hasher;
	Identifier all[] = {identA, identB, identA2};
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {
			Identifier a = all[i];
			Identifier b = all[j];
			EXPECT_EQ ( a == b, hasher(a) == hasher(b));
		}
	}



	// Tests whether hash function for identifiers is properly working

	// create list of identifiers
	vector<Identifier> identifier;
	identifier.push_back(Identifier("A"));
	identifier.push_back(Identifier("B"));
	EXPECT_FALSE ( hasDuplicates(identifier) );

	identifier.push_back(Identifier("A"));
	EXPECT_TRUE ( hasDuplicates(identifier) );

}

TEST(TypeTest, HashCodeTest) {

	Identifier identA("a");
	Identifier identB(std::string("a"));

	EXPECT_EQ (identA.hash(), identB.hash());

}
