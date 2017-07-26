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


#include "insieme/core/encoder/tuples.h"
#include "insieme/core/encoder/pairs.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {

	TEST(Tuples, TypeUtilities) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// check the create_tuple_type
		EXPECT_EQ("()", toString(*detail::create_tuple_type<>()(mgr)));
		EXPECT_EQ("(int<4>)", toString(*detail::create_tuple_type<int>()(mgr)));
		EXPECT_EQ("(int<4>,real<8>)", toString(*detail::create_tuple_type<int, double>()(mgr)));

		// check the encoding
		EXPECT_TRUE(detail::is_tuple<>()(builder.tupleExpr()));
		EXPECT_TRUE(detail::is_tuple<int>()(builder.tupleExpr(toIR(mgr, 8))));
		EXPECT_TRUE((detail::is_tuple<int, double>()(builder.tupleExpr(toIR(mgr, 8), toIR(mgr, 8.0)))));

		EXPECT_FALSE(detail::is_tuple<int>()(builder.tupleExpr(toIR(mgr, 8), toIR(mgr, 8.0))));
		EXPECT_FALSE((detail::is_tuple<int, double>()(builder.tupleExpr(toIR(mgr, 8.0), toIR(mgr, 8.0)))));
	}


	TEST(Tuples, TupleConversion) {
		NodeManager manager;

		// create a tuple

		auto value = std::make_tuple(1, 1.3, string("hello"));
		core::ExpressionPtr ir = toIR(manager, value);
		auto back = toValue<decltype(value)>(ir);

		EXPECT_EQ("(1,1.3,hello)", toString(value));
		EXPECT_EQ("tuple(1,1.3,hello)", toString(*ir));

		EXPECT_TRUE((isEncodingOf<decltype(value)>(ir)));
		EXPECT_EQ(value, back);

		EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));


		// check something more complex
		auto value2 = std::make_tuple(1, 2, std::make_pair(string("hello"), true), std::make_tuple(4, 3, 2));
		core::ExpressionPtr ir2 = toIR(manager, value2);
		auto back2 = toValue<decltype(value2)>(ir2);

		EXPECT_TRUE((isEncodingOf<decltype(value2)>(ir2)));
		EXPECT_EQ(value2, back2);

		EXPECT_EQ("[]", toString(check(ir2, checks::getFullCheck())));
	}


} // end namespace lists
} // end namespace core
} // end namespace insieme
