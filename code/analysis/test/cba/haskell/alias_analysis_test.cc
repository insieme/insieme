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
 */

#include <gtest/gtest.h>

#include "insieme/analysis/cba/haskell/interface.h"

#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/dump/json_dump.h"

#include "../common/alias_analysis_test.inc"

namespace insieme {
namespace analysis {
namespace cba {

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

	bool areAlias(haskell::Context& ctxt, const StatementAddress& a, const StatementAddress& b) {
		return areAlias<HaskellEngine>(ctxt, a.as<ExpressionAddress>(), b.as<ExpressionAddress>());
	}

	bool mayAlias(haskell::Context& ctxt, const StatementAddress& a, const StatementAddress& b) {
		return mayAlias<HaskellEngine>(ctxt, a.as<ExpressionAddress>(), b.as<ExpressionAddress>());
	}

	bool notAlias(haskell::Context& ctxt, const StatementAddress& a, const StatementAddress& b) {
		return notAlias<HaskellEngine>(ctxt, a.as<ExpressionAddress>(), b.as<ExpressionAddress>());
	}


	TEST(AdvancedAliasAnalysis, ComponentReferences) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"def struct A {"
				"	x : int<4>;"
				"	y : bool;"
				"	z : array<struct { a : int<4>; b : int<4>; },12u>;"
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
				"	var ref<array<A,10u>> l = ref_new(type_lit(array<A,10u>));"
				"	var ref<array<A,10u>> k = ref_new(type_lit(array<A,10u>));"
				"	var ref<array<A,10u>> m = l;"
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
				"	q[1];"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);
		core::dump::binary::haskell::dumpIR("comprefs.binir", stmt);
		core::dump::json::dumpIR("comprefs.json", stmt);

		auto comp = CompoundStmtAddress(stmt);

		haskell::Context ctxt;

		// first block
		EXPECT_TRUE(areAlias(ctxt, comp[4], comp[4]));
		EXPECT_TRUE(notAlias(ctxt, comp[4], comp[5]));
		EXPECT_TRUE(areAlias(ctxt, comp[4], comp[6]));
		EXPECT_TRUE(notAlias(ctxt, comp[4], comp[7]));
		EXPECT_TRUE(areAlias(ctxt, comp[4], comp[8]));
		EXPECT_TRUE(areAlias(ctxt, comp[6], comp[8]));

		// second block
		EXPECT_TRUE(areAlias(ctxt, comp[9], comp[ 9]));
		EXPECT_TRUE(notAlias(ctxt, comp[9], comp[10]));
		EXPECT_TRUE(areAlias(ctxt, comp[9], comp[11]));
		EXPECT_TRUE(areAlias(ctxt, comp[9], comp[12]));

		// third block
		EXPECT_TRUE(areAlias(ctxt, comp[16], comp[16]));
		EXPECT_TRUE(notAlias(ctxt, comp[16], comp[17]));
		EXPECT_TRUE(areAlias(ctxt, comp[16], comp[18]));

		// 4. block
		EXPECT_TRUE(areAlias(ctxt, comp[19], comp[19]));
		EXPECT_TRUE(notAlias(ctxt, comp[19], comp[20]));

		EXPECT_TRUE(areAlias(ctxt, comp[21], comp[21]));
		EXPECT_TRUE(notAlias(ctxt, comp[21], comp[22]));

		EXPECT_TRUE(areAlias(ctxt, comp[23], comp[23]));
		EXPECT_TRUE(notAlias(ctxt, comp[23], comp[24]));

		EXPECT_TRUE(notAlias(ctxt, comp[19], comp[21]));
		EXPECT_TRUE(notAlias(ctxt, comp[19], comp[22]));
		EXPECT_TRUE(areAlias(ctxt, comp[19], comp[23]));
		EXPECT_TRUE(notAlias(ctxt, comp[19], comp[24]));

		// 5. block
		EXPECT_TRUE(areAlias(ctxt, comp[25], comp[25]));
		EXPECT_TRUE(notAlias(ctxt, comp[25], comp[26]));
		EXPECT_TRUE(areAlias(ctxt, comp[25], comp[27]));

		EXPECT_TRUE(areAlias(ctxt, comp[28], comp[28]));
		EXPECT_TRUE(notAlias(ctxt, comp[28], comp[29]));
		EXPECT_TRUE(notAlias(ctxt, comp[28], comp[30]));

		EXPECT_TRUE(areAlias(ctxt, comp[30], comp[30]));
		EXPECT_TRUE(notAlias(ctxt, comp[30], comp[31]));
		EXPECT_TRUE(notAlias(ctxt, comp[30], comp[32]));

		EXPECT_TRUE(areAlias(ctxt, comp[32], comp[33]));

		// 6. block
		EXPECT_TRUE(areAlias(ctxt, comp[35], comp[35]));
		EXPECT_TRUE(notAlias(ctxt, comp[35], comp[36]));
		EXPECT_TRUE(areAlias(ctxt, comp[35], comp[37]));
		EXPECT_TRUE(notAlias(ctxt, comp[35], comp[38]));
	}

	TEST(AdvancedAliasAnalysis, PointerRefConversion) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<A> a = ref_new(type_lit(A)); "
				"	var ptr<A> p = ptr_from_ref(a); "
				"	"
				"	ptr_to_ref(p);"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(areAlias(comp[2], comp[2]));
	}


	TEST(AdvancedAliasAnalysis, PointerFromRef) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<A> a = ref_new(type_lit(A)); "
				"	var ref<A> b = ref_new(type_lit(A)); "
				"	var ref<ptr<A>> p = ptr_from_ref(a); "
				"	"
				"	ptr_to_ref(*p);"
				"	p = ptr_from_ref(b);"
				"	ptr_to_ref(*p);"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(areAlias(comp[3], comp[3]));
		EXPECT_TRUE(areAlias(comp[5], comp[5]));

		EXPECT_TRUE(notAlias(comp[3], comp[5]));
	}


	TEST(AdvancedAliasAnalysis, RefKindCastPlainTest) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<A> a = ref_new(type_lit(A)); "
				"	"
				"	a;"
				"	ref_kind_cast(a,type_lit(plain));"
				"	ref_kind_cast(a,type_lit(cpp_ref));"
				"	ref_kind_cast(a,type_lit(cpp_rref));"
				""
				"	ref_const_cast(a,type_lit(t));"
				"	ref_const_cast(a,type_lit(f));"
				""
				"	ref_volatile_cast(a,type_lit(t));"
				"	ref_volatile_cast(a,type_lit(f));"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(areAlias(comp[1], comp[1]));
		EXPECT_TRUE(areAlias(comp[1], comp[2]));
		EXPECT_TRUE(areAlias(comp[1], comp[3]));
		EXPECT_TRUE(areAlias(comp[1], comp[4]));
		EXPECT_TRUE(areAlias(comp[1], comp[5]));
		EXPECT_TRUE(areAlias(comp[1], comp[6]));
		EXPECT_TRUE(areAlias(comp[1], comp[7]));
		EXPECT_TRUE(areAlias(comp[1], comp[8]));

	}


	TEST(AdvancedAliasAnalysis, RefKindCastCppRefTest) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<A,f,f,cpp_ref> a = ref_kind_cast(ref_new(type_lit(A)),type_lit(cpp_ref)); "
				"	"
				"	a;"
				"	ref_kind_cast(a,type_lit(plain));"
				"	ref_kind_cast(a,type_lit(cpp_ref));"
				"	ref_kind_cast(a,type_lit(cpp_rref));"
				""
				"	ref_const_cast(a,type_lit(t));"
				"	ref_const_cast(a,type_lit(f));"
				""
				"	ref_volatile_cast(a,type_lit(t));"
				"	ref_volatile_cast(a,type_lit(f));"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(areAlias(comp[1], comp[1]));
		EXPECT_TRUE(areAlias(comp[1], comp[2]));
		EXPECT_TRUE(areAlias(comp[1], comp[3]));
		EXPECT_TRUE(areAlias(comp[1], comp[4]));
		EXPECT_TRUE(areAlias(comp[1], comp[5]));
		EXPECT_TRUE(areAlias(comp[1], comp[6]));
		EXPECT_TRUE(areAlias(comp[1], comp[7]));
		EXPECT_TRUE(areAlias(comp[1], comp[8]));

	}


	TEST(AdvancedAliasAnalysis, RefKindCastCppRRefTest) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<A,f,f,cpp_rref> a = ref_kind_cast(ref_new(type_lit(A)),type_lit(cpp_rref)); "
				"	"
				"	a;"
				"	ref_kind_cast(a,type_lit(plain));"
				"	ref_kind_cast(a,type_lit(cpp_ref));"
				"	ref_kind_cast(a,type_lit(cpp_rref));"
				""
				"	ref_const_cast(a,type_lit(t));"
				"	ref_const_cast(a,type_lit(f));"
				""
				"	ref_volatile_cast(a,type_lit(t));"
				"	ref_volatile_cast(a,type_lit(f));"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(areAlias(comp[1], comp[1]));
		EXPECT_TRUE(areAlias(comp[1], comp[2]));
		EXPECT_TRUE(areAlias(comp[1], comp[3]));
		EXPECT_TRUE(areAlias(comp[1], comp[4]));
		EXPECT_TRUE(areAlias(comp[1], comp[5]));
		EXPECT_TRUE(areAlias(comp[1], comp[6]));
		EXPECT_TRUE(areAlias(comp[1], comp[7]));
		EXPECT_TRUE(areAlias(comp[1], comp[8]));

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

