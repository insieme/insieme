/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/haskell_interface.h"

#include "../common/alias_analysis_test.inc"

namespace insieme {
namespace analysis {

	using namespace core;

	/**
	 * Run the alias analysis tests using the haskell backend.
	 */
	INSTANTIATE_TYPED_TEST_CASE_P(Haskell, AliasAnalysis, HaskellEngine);


	bool areAlias(const StatementAddress& a, const StatementAddress& b) {
		return areAlias<HaskellEngine>(a.as<ExpressionAddress>(), b.as<ExpressionAddress>());
	}

	bool mayAlias(const StatementAddress& a, const StatementAddress& b) {
		return mayAlias<HaskellEngine>(a.as<ExpressionAddress>(), b.as<ExpressionAddress>());
	}

	bool notAlias(const StatementAddress& a, const StatementAddress& b) {
		return notAlias<HaskellEngine>(a.as<ExpressionAddress>(), b.as<ExpressionAddress>());
	}


	TEST(AdvancedAliasAnalysis, ComponentReferences) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"def struct A {"
				"	x : int<4>;"
				"	y : bool;"
				"	z : array<struct { a : int<4>; b : int<4>; },12>;"
				"};"
				""
				"{"
				"	var ref<A> a = ref_new(type_lit(A));"
				"	var ref<A> b = ref_new(type_lit(A));"
				"	var ref<A> c = a;"
				"	var ref<ref<A>> p = a;"
				"	"
				"	a;"		// first block  (l=4-8)
				"	b;"
				"   c;"
				"   p;"
				"	*p;"
				"	"
				"	a.x;"	// second block (l=9-12)
				"	b.x;"
				"	c.x;"
				"	(*p).x;"
				"	"
				"	var ref<array<A,10>> l = ref_new(type_lit(array<A,10>));"
				"	var ref<array<A,10>> k = ref_new(type_lit(array<A,10>));"
				"	var ref<array<A,10>> m = l;"
				"	"
				"	l;"				// third block (l=16-18)
				"	k;"
				"	m;"
				"	"
				"	l[0];"			// 4. block (l=19-24)
				"	l[1];"
				"	k[0];"
				"	k[1];"
				"	m[0];"
				"	m[1];"
				"	"
				"	a.z;"			// 5. block (l=25-33)
				"	b.z;"
				"	c.z;"
				"	a.z[0];"
				"	a.z[1];"
				"	a.z[0].a;"
				"	a.z[0].b;"
				"	a.z[1].a;"
				"	(*p).z[1].a;"
				"	"
				"	var ref<array<A>> q = ref_scalar_to_ref_array(a);"
				"	a;"												// 6. block (l=35-37)
				"	q;"                                             // testing expanding data paths
				"	q[0];"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		// first block
		EXPECT_TRUE(areAlias(comp[4], comp[4]));
		EXPECT_TRUE(notAlias(comp[4], comp[5]));
		EXPECT_TRUE(areAlias(comp[4], comp[6]));
		EXPECT_TRUE(notAlias(comp[4], comp[7]));
		EXPECT_TRUE(areAlias(comp[4], comp[8]));
		EXPECT_TRUE(areAlias(comp[6], comp[8]));

		// second block
		EXPECT_TRUE(areAlias(comp[9], comp[ 9]));
		EXPECT_TRUE(notAlias(comp[9], comp[10]));
		EXPECT_TRUE(areAlias(comp[9], comp[11]));
		EXPECT_TRUE(areAlias(comp[9], comp[12]));

		// third block
		EXPECT_TRUE(areAlias(comp[16], comp[16]));
		EXPECT_TRUE(notAlias(comp[16], comp[17]));
		EXPECT_TRUE(areAlias(comp[16], comp[18]));

		// 4. block
		EXPECT_TRUE(areAlias(comp[19], comp[19]));
		EXPECT_TRUE(notAlias(comp[19], comp[20]));

		EXPECT_TRUE(areAlias(comp[21], comp[21]));
		EXPECT_TRUE(notAlias(comp[21], comp[22]));

		EXPECT_TRUE(areAlias(comp[23], comp[23]));
		EXPECT_TRUE(notAlias(comp[23], comp[24]));

		EXPECT_TRUE(notAlias(comp[19], comp[21]));
		EXPECT_TRUE(notAlias(comp[19], comp[22]));
		EXPECT_TRUE(areAlias(comp[19], comp[23]));
		EXPECT_TRUE(notAlias(comp[19], comp[24]));

		// 5. block
		EXPECT_TRUE(areAlias(comp[25], comp[25]));
		EXPECT_TRUE(notAlias(comp[25], comp[26]));
		EXPECT_TRUE(areAlias(comp[25], comp[27]));

		EXPECT_TRUE(areAlias(comp[28], comp[28]));
		EXPECT_TRUE(notAlias(comp[28], comp[29]));
		EXPECT_TRUE(notAlias(comp[28], comp[30]));

		EXPECT_TRUE(areAlias(comp[30], comp[30]));
		EXPECT_TRUE(notAlias(comp[30], comp[31]));
		EXPECT_TRUE(notAlias(comp[30], comp[32]));

		EXPECT_TRUE(areAlias(comp[32], comp[33]));

		// 6. block
		EXPECT_TRUE(areAlias(comp[35], comp[35]));
		EXPECT_TRUE(notAlias(comp[35], comp[36]));
		EXPECT_TRUE(areAlias(comp[35], comp[37]));
	}

} // end namespace analysis
} // end namespace insieme

