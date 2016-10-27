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

#include "insieme/analysis/cba/haskell/interface.h"

#include "../common/alias_analysis_test.inc"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;


	bool isTrue(const StatementAddress& a) {
		return isTrue<HaskellEngine>(a.as<ExpressionAddress>());
	}

	bool isFalse(const StatementAddress& a) {
		return isFalse<HaskellEngine>(a.as<ExpressionAddress>());
	}

	bool isUndefined(const StatementAddress& a) {
		auto e = a.as<ExpressionAddress>();
		return mayBeTrue<HaskellEngine>(e) && mayBeFalse<HaskellEngine>(e);
	}

	TEST(MemoryStateAnalysis, ScalarReadWrite) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<bool> a = ref_new(type_lit(bool));"
				"	"
				"	*a;"				// should be undefined
				"	a = true;"
				"	*a;"				// should be true now
				"	a = false;"
				"	*a;"				// should be false now
				"	a = true;"
				"	*a;"				// should be true now
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(isUndefined(comp[1]));

		EXPECT_TRUE(isTrue(comp[3]));
		EXPECT_TRUE(isFalse(comp[5]));
		EXPECT_TRUE(isTrue(comp[7]));
	}


	TEST(MemoryStateAnalysis, CompoundReadWrite) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"def struct A {"
				"	x : bool;"
				"	y : bool;"
				"	z : array<struct { a : bool; b : bool; },12>;"
				"};"
				""
				"{"
				"	var ref<A> a = ref_new(type_lit(A));"
				"	"
				"	*(a.x);"				// should be undefined
				"	*(a.y);"				// should be undefined
				"	a.x = true;"
				"	a.y = false;"
				"	*(a.x);"				// should be true now
				"	*(a.y);"				// should be false now
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(isUndefined(comp[1]));
		EXPECT_TRUE(isUndefined(comp[2]));

		EXPECT_TRUE(isTrue(comp[5]));
		EXPECT_TRUE(isFalse(comp[6]));

	}

	TEST(MemoryStateAnalysis, TupleReadWrite) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<(bool,bool)> a = ref_new(type_lit((bool,bool)));"
				"	"
				"	*(a.0);"				// should be undefined
				"	*(a.1);"				// should be undefined
				"	a.0 = true;"
				"	a.1 = false;"
				"	*(a.0);"				// should be true now
				"	*(a.1);"				// should be false now
				"	a = (false,true);"
				"	*(a.0);"				// should be false now
				"	*(a.1);"				// should be true now
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(isUndefined(comp[1]));
		EXPECT_TRUE(isUndefined(comp[2]));

		EXPECT_TRUE(isTrue(comp[5]));
		EXPECT_TRUE(isFalse(comp[6]));

		EXPECT_TRUE(isFalse(comp[8]));
		EXPECT_TRUE(isTrue(comp[9]));
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

