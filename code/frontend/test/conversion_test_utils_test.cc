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

#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"


namespace insieme {
namespace frontend {
namespace utils {

	TEST(ConversionUtils, AnyTypeAndAnyExpr) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// __any_expr__
		{
			auto expected = builder.parseStmt(R"(
				{
					var ref<int<4>> v1 = 1;
					var ref<int<4>> v2 = 2;
					var ref<int<4>> v3 = lit("__any_expr__" : (int<4>) -> int<4>)(1);
				}
			)");
			auto actual = builder.parseStmt(R"(
				def foo = (a: int<4>) -> int<4> { return a; };
				{
					var ref<int<4>> v1 = 1;
					var ref<int<4>> v2 = 2;
					var ref<int<4>> v3 = foo(1);
				}
			)");
			auto res = detail::replaceAnyTypeAndAnyExprNodes(expected, actual);

			ASSERT_TRUE(checks::check(expected).empty()) << checks::check(expected);
			ASSERT_TRUE(checks::check(actual).empty()) << checks::check(actual);
			ASSERT_TRUE(checks::check(res).empty()) << checks::check(res);

			EXPECT_EQ(builder.normalize(expected), builder.normalize(res));
		}

		// __any_type__
		{
			auto expected = builder.parseStmt(R"(
				{
					var ref<__any_type__> v1 = ref_decl(type_lit(ref<__any_type__>));
				}
			)");
			auto actual = builder.parseStmt(R"(
				def struct A {};
				{
					var ref<A> v1 = ref_decl(type_lit(ref<A>));
				}
			)");
			auto res = detail::replaceAnyTypeAndAnyExprNodes(expected, actual);

			ASSERT_TRUE(checks::check(expected).empty()) << checks::check(expected);
			ASSERT_TRUE(checks::check(actual).empty()) << checks::check(actual);
			ASSERT_TRUE(checks::check(res).empty()) << checks::check(res);

			EXPECT_EQ(builder.normalize(expected), builder.normalize(res));
		}
	}

	TEST(ConversionUtils, AnyString) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto expected = builder.parseStmt(R"(
			def struct __any_string__1 {};
			def struct __any_string__2 {
				ctor function () {}
				function IMP_foo = () -> unit {}
			};
			{
				var ref<__any_string__1> v1;
				var ref<__any_string__2> v2 = __any_string_2::(ref_decl(type_lit(ref<__any_string__2>)));
				v2.IMP_foo();
			}
		)");
		auto actual = builder.parseStmt(R"(
			def struct A {};
			def struct B {
				ctor function () {}
				function IMP_foo = () -> unit {}
			};
			{
				var ref<A> v1;
				var ref<B> v2 = B::(ref_decl(type_lit(ref<B>)));
				v2.IMP_foo();
			}
		)");
		auto res = detail::replaceAnyStringsInActual(expected, actual);

		ASSERT_TRUE(checks::check(expected).empty()) << checks::check(expected);
		ASSERT_TRUE(checks::check(actual).empty()) << checks::check(actual);
		ASSERT_TRUE(checks::check(res).empty()) << checks::check(res);

		EXPECT_EQ(builder.normalize(expected), builder.normalize(res));
	}

} // utils namespace
} // fe namespace
} // insieme namespace
