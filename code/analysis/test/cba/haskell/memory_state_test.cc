/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

