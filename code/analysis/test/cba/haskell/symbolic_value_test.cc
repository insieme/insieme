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

#include "insieme/analysis/cba/haskell/symbolic_value_analysis.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	using namespace insieme::core;

	namespace {

		using symbolic_value_list = std::vector<std::string>;


		NodePtr groundFreeVariables(const NodePtr& node) {
			IRBuilder builder(node.getNodeManager());

			// get free variables
			auto freeVars = core::analysis::getFreeVariables(node);

			// compute replacements
			unsigned i = 0;
			core::NodeMap replacements;
			for(const auto& var : freeVars) {
				replacements[var] = builder.variable(var.getType(),i++);
			}

			// apply replacements
			return core::transform::replaceAll(node.getNodeManager(),node,replacements);

		}

		symbolic_value_list toList(const SymbolicValueSet& values) {
			symbolic_value_list res;

			// handle universal sets
			if (values.isUniversal()) {
				res.push_back("-all-");
				return res;
			}

			// convert values
			for(const auto& cur : values) {
				// check that they do not contain semantic errors
				assert_correct_ir(cur);
				// add print-out to result values
				res.push_back(toString(dumpOneLine(groundFreeVariables(cur))));
			}

			// sort values
			std::sort(res.begin(),res.end());

			// done
			return res;
		}


		symbolic_value_list getValues(NodeManager& mgr, const std::string& code, bool debug = false) {
			IRBuilder builder(mgr);

			// parse the value set
			auto expr = builder.parseExpr(code);
			assert_correct_ir(expr);

			// compute the symbolic values
			Context ctxt;
			auto res = getSymbolicValue(ctxt,core::ExpressionAddress(expr));

			if (debug) {
				insieme::core::dump::json::dumpIR("ir.json",expr);
				ctxt.dumpStatistics();
				ctxt.dumpSolution();
			}

			// convert set to list of values
			return toList(res);

		}

	}

	TEST(SymbolicValues, Literals) {
		NodeManager mgr;

		// test literals of differnt types
		EXPECT_EQ("[12]",toString(getValues(mgr,"12")));
		EXPECT_EQ("['c']",toString(getValues(mgr,"'c'")));
		EXPECT_EQ("[true]",toString(getValues(mgr,"true")));
		EXPECT_EQ("[fun]",toString(getValues(mgr,"lit(\"fun\":(int<4>)->int<4>)")));

	}

	TEST(SymbolicValues, Operators) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ("[1+2]",toString(getValues(mgr,"1 + 2")));

		// more complex values
		EXPECT_EQ("[1+2*3]",toString(getValues(mgr,"1 + 2 * 3")));

	}

	TEST(SymbolicValues, Forwarding) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[12]",
			toString(getValues(mgr,
				"def id = ( x : 'a ) -> 'a {"
				"	return x;"
				"};"
				""
				"id(12)"
			))
		);

	}

	TEST(SymbolicValues, Increment) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[3+1]",
			toString(getValues(mgr,
				R"(
					def id = ( x : int<4> ) -> int<4> {
						return x + 1;
					};
					
					id(3)
				)"
			))
		);

	}

	TEST(SymbolicValues, Computation) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[3%(3*2+1)]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = x;
						tmp = *tmp * 2;
						tmp = *tmp + 1;
						tmp = 3 % (*tmp);
						return *tmp;
					};
					
					fun(3)
				)"
			))
		);

	}


	TEST(SymbolicValues, ConditionalComputation) {
		NodeManager mgr;

		// test composed values
		EXPECT_EQ(
			"[3%(3*2+1),3%(3+1)]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = x;
						if (*tmp < *lit("X":ref<int<4>>)) {
							tmp = *tmp * 2;
						}
						tmp = *tmp + 1;
						tmp = 3 % (*tmp);
						return *tmp;
					};
					
					fun(3)
				)"
			))
		);

	}

	TEST(SymbolicValues, Globals) {
		NodeManager mgr;

		// test reading from a global value
		EXPECT_EQ(
			"[*x]",
			toString(getValues(mgr,
				R"(
					*lit("x":ref<int<4>>)
				)"
			))
		);

		// test composing this global value
		EXPECT_EQ(
			"[*x+2]",
			toString(getValues(mgr,
				R"(
					*lit("x":ref<int<4>>) + 2
				)"
			))
		);


		// test some more complex computation
		EXPECT_EQ(
			"[3%(*x*2+*k),3%(*x+*k)]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = x;
						if (*tmp < *lit("X":ref<int<4>>)) {
							tmp = *tmp * 2;
						}
						tmp = *tmp + *lit("k":ref<int<4>>);
						tmp = 3 % (*tmp);
						return *tmp;
					};
					
					fun(*lit("x":ref<int<4>>))
				)"
			))
		);

		// mutating global references
		EXPECT_EQ(
			"[*x+12]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : ref<int<4>> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = *x;
						x = 12;
						tmp = *tmp + *x;
						return *tmp;
					};
					
					fun(lit("x":ref<int<4>>))
				)"
			))
		);

	}

	TEST(SymbolicValues, FreeVariable) {
		NodeManager mgr;

		// test retrieving a simple free variable
		EXPECT_EQ(
			"[v0]",
			toString(getValues(mgr,
				R"(
					free_var(int<4>)
				)"
			))
		);

		// test reading from a free variable
		EXPECT_EQ(
			"[*v0]",
			toString(getValues(mgr,
				R"(
					*free_var(ref<int<4>>)
				)"
			))
		);

		// test composing the free variable with a value
		EXPECT_EQ(
			"[v0+2]",
			toString(getValues(mgr,
				R"(
					free_var(int<4>) + 2
				)"
			))
		);

		// test composing the free variable with a value
		EXPECT_EQ(
			"[*v0+2]",
			toString(getValues(mgr,
				R"(
					*free_var(ref<int<4>>) + 2
				)"
			))
		);

		// test with free variables of value types
		EXPECT_EQ(
			"[3%(v0*2+1),3%(v0+1)]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = x;
						if (*tmp < *lit("X":ref<int<4>>)) {
							tmp = *tmp * 2;
						}
						tmp = *tmp + 1;
						tmp = 3 % (*tmp);
						return *tmp;
					};

					fun(free_var(int<4>))
				)"
			))
		);


		// test with free variables of reference types
		EXPECT_EQ(
			"[*v0+12]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : ref<int<4>> ) -> int<4> {
						var ref<int<4>> tmp;
						tmp = *x;
						x = 12;
						tmp = *tmp + *x;
						return *tmp;
					};

					fun(free_var(ref<int<4>>))
				)"
			))
		);

	}

	TEST(SymbolicValues, PassAndReturn) {
		NodeManager mgr;

		// test with free variables of reference types
		EXPECT_EQ(
			"[4]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> sum = x;
						return *sum;
					};

					fun(4)
				)"
			))
		);
	}


	TEST(SymbolicValues, ForLoop) {
		NodeManager mgr;

		// make sure that for-loop variables remain unknown (and not fixed to lower or upper bound)
		EXPECT_EQ(
			"[-all-]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> tmp = 0;
						for(int<4> i = 1 .. x) {
							tmp = i;
						}
						return *tmp;
					};

					fun(4)
				)"
			))
		);

		// make sure that for-loop variables remain unknown (and not fixed to lower or upper bound)
		EXPECT_EQ(
			"[-all-]",
			toString(getValues(mgr,
				R"(
					def fun = ( x : int<4> ) -> int<4> {
						var ref<int<4>> sum = 0;
						for(int<4> i = 1 .. x) {
							sum = *sum + i;
						}
						return *sum;
					};

					fun(4)
				)"
			))
		);
	}


	TEST(SymbolicValues, StructMembers) {
		NodeManager mgr;

		// see whether a simple member access does work
		EXPECT_EQ(
			"[S.x]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					lit("S":params).x
				)"
			))
		);

		EXPECT_EQ(
			"[S.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					lit("S":params).y
				)"
			))
		);

		// see whether a access to a ref member does work
		EXPECT_EQ(
			"[S.x]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					lit("S":ref<params>).x
				)"
			))
		);

		EXPECT_EQ(
			"[(*S).x]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					(*lit("S":ref<params>)).x
				)"
			))
		);

		EXPECT_EQ(
			"[*S.x]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					*(lit("S":ref<params>).x)
				)"
			))
		);

		EXPECT_EQ(
			"[S.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					lit("S":ref<params>).y
				)"
			))
		);

		// also test this in conjunction with function calls
		EXPECT_EQ(
			"[S.x]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};
					def fun = ( x : params ) -> int<4> {
						return x.x;
					};

					fun(lit("S":params))
				)"
			))
		);

		EXPECT_EQ(
			"[S.x+S.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};
					def fun = ( x : params ) -> int<4> {
						return x.x + x.y;
					};

					fun(lit("S":params))
				)"
			))
		);

		// and as reference - passed by reference
		EXPECT_EQ(
			"[*S.x+*S.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};
					def fun = ( x : ref<params> ) -> int<4> {
						return *x.x + *x.y;
					};

					fun(lit("S":ref<params>))
				)"
			))
		);

		// and as reference - passed by value
		EXPECT_EQ(
			"[(*S).x+(*S).y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};
					def fun = ( x : params ) -> int<4> {
						return x.x + x.y;
					};

					fun(lit("S":ref<params>))
				)"
			))
		);

		// nested struct values
		EXPECT_EQ(
			"[S.a.x+S.b.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					def struct ext_params {
						a : params;
						b : params;
					};

					lit("S":ext_params).a.x + lit("S":ext_params).b.y 
				)"
			))
		);

		// also test nested structs, passed by reference
		EXPECT_EQ(
			"[*S.b.x+*S.a.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					def struct ext_params {
						a : params;
						b : params;
					};

					def fun = ( x : ref<ext_params> ) -> int<4> {
						return *x.b.x + *x.a.y;
					};

					fun(lit("S":ref<ext_params>))
				)"
			))
		);

		// nested structs, passed by value
		EXPECT_EQ(
			"[(*S).b.x+(*S).a.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					def struct ext_params {
						a : params;
						b : params;
					};

					def fun = ( x : ext_params ) -> int<4> {
						return x.b.x + x.a.y;
					};

					fun(lit("S":ref<ext_params>))
				)"
			))
		);

		// three-times nested
		EXPECT_EQ(
			"[(*S).l.b.x+(*S).k.a.y]",
			toString(getValues(mgr,
				R"(
					def struct params {
						x : int<4>;
						y : int<4>;
					};

					def struct ext_params {
						a : params;
						b : params;
					};

					def struct extra_ext_params {
						k : ext_params;
						l : ext_params;
					};

					def fun = ( x : extra_ext_params ) -> int<4> {
						return x.l.b.x + x.k.a.y;
					};

					fun(lit("S":ref<extra_ext_params>))
				)"
			))
		);

	}


	TEST(SymbolicValues, Constructors) {
		NodeManager mgr;

		// in case of constructors, the result value should only be the initialized value
		EXPECT_EQ(
			"[A(ref_temp(type_lit(A)))]",
			toString(getValues(mgr,
				R"(
					lit("A": A::() )(ref_temp(type_lit(A)))
				)"
			))
		);
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

