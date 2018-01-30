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

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	using SymbolTable = std::map<std::string,core::NodePtr>;

	/**
	 * A generic test case for alias analysis.
	 */
	template <typename Backend>
	class AliasAnalysis : public testing::Test {

		typename Backend::context_type context;

	protected:

		bool areAlias(const core::StatementAddress& a, const core::StatementAddress& b) {
			return insieme::analysis::cba::areAlias<Backend>(context,a.as<ExpressionAddress>(),b.as<ExpressionAddress>());
		}

		bool mayAlias(const core::StatementAddress& a, const core::StatementAddress& b) {
			return insieme::analysis::cba::mayAlias<Backend>(context,a.as<ExpressionAddress>(),b.as<ExpressionAddress>());
		}

		bool notAlias(const core::StatementAddress& a, const core::StatementAddress& b) {
			return insieme::analysis::cba::notAlias<Backend>(context,a.as<ExpressionAddress>(),b.as<ExpressionAddress>());
		}

		void dumpSolution() {
			context.dumpSolution();
		}

	};

	TYPED_TEST_CASE_P(AliasAnalysis);

	TYPED_TEST_P(AliasAnalysis, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<bool> x = ref_new(type_lit(bool));"
				"	x;"
				"	x;"
				"}"
		).as<CompoundStmtPtr>();

		auto compound = CompoundStmtAddress(stmt);
		auto x1 = compound[0].as<DeclarationStmtAddress>()->getVariable();
		auto x2 = compound[1].as<VariableAddress>();
		auto x3 = compound[2].as<VariableAddress>();

		EXPECT_TRUE(this->areAlias(x1,x1));
		EXPECT_TRUE(this->mayAlias(x1,x1));

		EXPECT_TRUE(this->areAlias(x2,x2));
		EXPECT_TRUE(this->mayAlias(x2,x2));

		EXPECT_TRUE(this->areAlias(x3,x3));
		EXPECT_TRUE(this->mayAlias(x3,x3));

		EXPECT_TRUE(this->areAlias(x1,x2));
		EXPECT_TRUE(this->mayAlias(x1,x2));

		EXPECT_TRUE(this->areAlias(x1,x3));
		EXPECT_TRUE(this->mayAlias(x1,x3));

		EXPECT_TRUE(this->areAlias(x2,x3));
		EXPECT_TRUE(this->mayAlias(x2,x3));
	}

	TYPED_TEST_P(AliasAnalysis, Forwarding) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"{"
				"	var ref<A> a = ref_new(type_lit(A));"
				"	"
				"	a;"
				"	( x : ref<A>) -> ref<A> { return x; } (a);"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(stmt);

		auto comp = CompoundStmtAddress(stmt);

		// check that the given statements are aliases
		EXPECT_TRUE(this->areAlias(comp[1], comp[1]));
		EXPECT_TRUE(this->areAlias(comp[1], comp[2]));

	}

	TYPED_TEST_P(AliasAnalysis, MultipleReferences) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
				"{"
				"	var ref<bool> x = ref_new(type_lit(bool));"
				"	var ref<bool> y = ref_new(type_lit(bool));"
				"	var ref<bool> z = x;"
				"	$x$;"
				"	$y$;"
				"	$z$;"
				"}"
		);

		EXPECT_EQ(3,addresses.size());

		auto x = addresses[0].as<VariableAddress>();
		auto y = addresses[1].as<VariableAddress>();
		auto z = addresses[2].as<VariableAddress>();

		EXPECT_FALSE(this->areAlias(x,y));
		EXPECT_FALSE(this->mayAlias(x,y));

		EXPECT_TRUE(this->areAlias(x,z));
		EXPECT_TRUE(this->mayAlias(x,z));

		EXPECT_FALSE(this->areAlias(y,z));
		EXPECT_FALSE(this->mayAlias(y,z));

	}

	TYPED_TEST_P(AliasAnalysis, MaterializingCall) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
				"def id = function ( x : ref<int<4>,f,f,plain> ) -> int<4> {"
				"	$x$;"
				"	return *x;"
				"};"
				""
				"{"
				"	var ref<int<4>,f,f,plain> a = 12;"
				"	var ref<int<4>,f,f,plain> b = id(*a);"
				"	var ref<int<4>,f,f,plain> c = $id(*a) materialize$;"
				"	$a$;"
				"	$b$;"
				"	$c$;"
				"}"
		);

		// assert correct number of statements
		ASSERT_EQ(6,addresses.size());

		// get the variables
		auto a = addresses[3].as<VariableAddress>();
		auto b = addresses[4].as<VariableAddress>();
		auto c = addresses[5].as<VariableAddress>();

		// get the id calls
		auto i = addresses[1].as<CallExprAddress>();

		// get the x instances
		auto x1 = addresses[0].as<VariableAddress>();
		auto x2 = addresses[2].as<VariableAddress>();

		// assert error free code
		assert_correct_ir(a.getRootNode());

		// ensure that calls are materializing and not materializing
		EXPECT_TRUE(core::analysis::isMaterializingCall(i));

		EXPECT_NE(a,b);
		EXPECT_NE(a,c);
		EXPECT_NE(a,i);
		EXPECT_NE(a,x1);
		EXPECT_NE(a,x2);

		EXPECT_NE(b,c);
		EXPECT_NE(b,i);
		EXPECT_NE(b,x1);
		EXPECT_NE(b,x2);

		EXPECT_NE(c,i);
		EXPECT_NE(c,x1);
		EXPECT_NE(c,x2);

		EXPECT_NE(i,x1);
		EXPECT_NE(i,x2);

		EXPECT_NE(x1,x2);


		// compare the mutal alias relations
		EXPECT_TRUE(this->areAlias(a,a));
		EXPECT_TRUE(this->notAlias(a,b));
		EXPECT_TRUE(this->notAlias(a,c));
		EXPECT_TRUE(this->notAlias(a,i));
		EXPECT_TRUE(this->notAlias(a,x1));
		EXPECT_TRUE(this->notAlias(a,x2));

		EXPECT_TRUE(this->areAlias(b,b));
		EXPECT_TRUE(this->notAlias(b,c));
		EXPECT_TRUE(this->notAlias(b,i));
		EXPECT_TRUE(this->notAlias(b,x1));
		EXPECT_TRUE(this->notAlias(b,x2));

		EXPECT_TRUE(this->areAlias(c,c));
		EXPECT_TRUE(this->areAlias(c,i));
		EXPECT_TRUE(this->notAlias(c,x1));
		EXPECT_TRUE(this->notAlias(c,x2));

		EXPECT_TRUE(this->areAlias(i,i));
		EXPECT_TRUE(this->notAlias(i,x1));
		EXPECT_TRUE(this->notAlias(i,x2));

		EXPECT_TRUE(this->areAlias(x1,x1));
		EXPECT_TRUE(this->notAlias(x1,x2));

		EXPECT_TRUE(this->areAlias(x2,x2));

	}

	// TODO: test global variables

	// TODO: test free variables


	REGISTER_TYPED_TEST_CASE_P(AliasAnalysis,
			Basic,
			Forwarding,
			MultipleReferences,
			MaterializingCall
	);

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

