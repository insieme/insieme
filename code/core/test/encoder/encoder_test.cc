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

#include <boost/lexical_cast.hpp>

#include "insieme/core/encoder/encoder.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {

	TEST(PrimitiveTypes, Base) {
		NodeManager manager;
		IRBuilder builder(manager);
		const auto& basic = manager.getLangBasic();

		core::ExpressionPtr expr = toIR(manager, 12);

		EXPECT_EQ(basic.getInt4(), expr->getType());
		EXPECT_EQ("12", toString(*expr));
		EXPECT_EQ(12, toValue<int>(expr));
		EXPECT_TRUE(isEncodingOf<int>(expr));
		EXPECT_FALSE(isEncodingOf<short>(expr));

		expr = toIR(manager, false);
		EXPECT_EQ(basic.getBool(), expr->getType());
		EXPECT_TRUE(basic.isFalse(expr));
		EXPECT_EQ("false", toString(*expr));
		EXPECT_FALSE(toValue<bool>(expr));

		expr = toIR(manager, true);
		EXPECT_EQ(basic.getBool(), expr->getType());
		EXPECT_TRUE(basic.isTrue(expr));
		EXPECT_EQ("true", toString(*expr));
		EXPECT_TRUE(toValue<bool>(expr));

		EXPECT_TRUE(isEncodingOf<bool>(expr));
		EXPECT_FALSE(isEncodingOf<int>(expr));
		EXPECT_FALSE(isEncodingOf<short>(expr));

		EXPECT_TRUE(isEncodingOf<bool>(toIR(manager, true)));
		EXPECT_TRUE(isEncodingOf<bool>(toIR(manager, false)));


		expr = toIR(manager, (short)4);
		EXPECT_EQ("int<2>", toString(*expr->getType()));
		EXPECT_EQ("4", toString(*expr));
		EXPECT_TRUE(checks::check(expr).empty()) << checks::check(expr);

		expr = toIR(manager, (unsigned)14);
		EXPECT_EQ("uint<4>", toString(*expr->getType()));
		EXPECT_EQ("14u", toString(*expr));
		EXPECT_TRUE(checks::check(expr).empty()) << checks::check(expr);

		float y = 1.5f;
		expr = toIR(manager, y);
		EXPECT_EQ("real<4>", toString(*expr->getType()));
		EXPECT_EQ("1.5f", toString(*expr));
		EXPECT_TRUE(checks::check(expr).empty()) << checks::check(expr);

		double x = 123.123;
		expr = toIR(manager, x);
		EXPECT_EQ("real<8>", toString(*expr->getType()));
		EXPECT_EQ("123.123", toString(*expr));
		EXPECT_TRUE(checks::check(expr).empty()) << checks::check(expr);


		// check strings
		string test = "Hello";
		expr = toIR(manager, test);
		EXPECT_EQ("ref<array<char,inf>,f,f,plain>", toString(*expr->getType()));
		EXPECT_EQ("Hello", toString(*expr));
		EXPECT_EQ(test, toValue<string>(expr));
		EXPECT_TRUE(checks::check(expr).empty()) << checks::check(expr);

		// check exceptions
		expr = builder.variable(basic.getInt4());
		EXPECT_THROW(toValue<double>(expr), InvalidExpression); // wrong type
		EXPECT_THROW(toValue<int>(expr), InvalidExpression);    // wrong node type
		EXPECT_TRUE(checks::check(expr).empty()) << checks::check(expr);


		// check types
		EXPECT_EQ(basic.getInt4(), getTypeFor<int>(manager));
		EXPECT_EQ(basic.getDouble(), getTypeFor<double>(manager));
	}

	TEST(PrimitiveTypes, SubTypeSupport) {
		NodeManager manager;
		IRBuilder builder(manager);
		const auto& basic = manager.getLangBasic();

		core::TypePtr uint4 = basic.getUInt4();
		core::TypePtr uint8 = basic.getUInt8();

		core::ExpressionPtr exprA = builder.literal(uint4, "12");
		core::ExpressionPtr exprB = builder.literal(uint8, "14");

		EXPECT_EQ(uint4, getTypeFor<uint32_t>(manager));
		EXPECT_EQ(uint8, getTypeFor<uint64_t>(manager));

		EXPECT_TRUE(isEncodingOf<uint32_t>(exprA));
		EXPECT_FALSE(isEncodingOf<uint32_t>(exprB));
		EXPECT_TRUE(isEncodingOf<uint64_t>(exprA));
		EXPECT_TRUE(isEncodingOf<uint64_t>(exprB));

		EXPECT_EQ(12u, toValue<uint32_t>(exprA));
		EXPECT_EQ(12u, toValue<uint64_t>(exprA));
		EXPECT_EQ(14u, toValue<uint64_t>(exprB));
	}


	TEST(Expressions, Basic) {
		// tests the encoding of expressions
		NodeManager manager;
		IRBuilder builder(manager);

		ExpressionPtr exp = builder.boolLit(true);

		EXPECT_TRUE(exp);


		EXPECT_EQ("AP(wrap_ExpressionPtr(true))", toString(toIR(manager, exp)));
		EXPECT_EQ(exp, toValue<ExpressionPtr>(toIR(manager, exp)));
	}

	TEST(Expressions, NullPointer) {
		// test the encoding of null pointer
		NodeManager mgr;

		ExpressionPtr nothing;

		EXPECT_FALSE(nothing);
		EXPECT_EQ("AP(null_ExpressionPtr())", toString(toIR(mgr, nothing)));
		EXPECT_EQ(nothing, toValue<ExpressionPtr>(toIR(mgr, nothing)));
	}


	struct Info : public encodable {

		int x;

		static bool isEncoding(const ExpressionPtr& expr) {
			return encoder::isEncodingOf<int>(expr);
		}

		static TypePtr getEncodedType(NodeManager& mgr) {
			return encoder::getTypeFor<int>(mgr);
		}

		ExpressionPtr toIR(NodeManager& mgr) const {
			return encoder::toIR(mgr,x);
		}

		static Info fromIR(const ExpressionPtr& expr) {
			Info res;
			res.x = encoder::toValue<int>(expr);
			return res;
		}

	};


	TEST(Encodable, SimpleEncodable) {

		NodeManager mgr;

		Info i;
		i.x = 12;

		EXPECT_EQ("int<4>",toString(*getTypeFor<Info>(mgr)));
		EXPECT_EQ("12",toString(*toIR(mgr,i)));

		EXPECT_EQ(12, toValue<Info>(toIR(mgr,i)).x);
	}

} // end namespace lists
} // end namespace core
} // end namespace insieme
