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

#include "insieme/core/lang/basic.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/lang/reference.h"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(LangBasic, Generation) {
	NodeManager nm;

	EXPECT_TRUE(nm.getLangBasic().isInt4(nm.getLangBasic().getInt4()));
}

TEST(LangBasic, BoolChecks) {
	NodeManager nm;

	EXPECT_TRUE(lang::isBuiltIn(nm.getLangBasic().getInt4()));
	EXPECT_TRUE(lang::isBuiltIn(nm.getLangBasic().getSignedIntLShift()));
	EXPECT_FALSE(lang::isBuiltIn((GenericType::get(nm, "surelyNotBuiltInISincerelyHope__"))));
}

TEST(LangBasic, StringGet) {
	NodeManager nm;

	EXPECT_EQ(nm.getLangBasic().getBoolEq(), nm.getLangBasic().getBuiltIn("bool_eq"));
}

TEST(LangBasic, Grouping) {
	NodeManager nm;

	EXPECT_TRUE(nm.getLangBasic().isUnsignedInt(nm.getLangBasic().getUInt4()));
	EXPECT_TRUE(nm.getLangBasic().isInt(nm.getLangBasic().getUInt4()));
	EXPECT_FALSE(nm.getLangBasic().isReal(nm.getLangBasic().getUInt4()));
}

TEST(LangBasic, GroupingEnumeration) {
	NodeManager nm;
	auto& basic = nm.getLangBasic();

	EXPECT_EQ(7u, basic.getUnsignedIntGroup().size());
	EXPECT_EQ(5u, basic.getRealGroup().size());
}

TEST(LangBasic, OperatorGet) {
	NodeManager nm;

	LiteralPtr op = dynamic_pointer_cast<const Literal>(nm.getLangBasic().getOperator(nm.getLangBasic().getUInt4(), nm.getLangBasic().Add));
	EXPECT_TRUE(op);
	EXPECT_EQ(*op, *nm.getLangBasic().getUnsignedIntAdd());
	EXPECT_NE(*op, *nm.getLangBasic().getSignedIntAdd());

	LiteralPtr op2 = dynamic_pointer_cast<const Literal>(nm.getLangBasic().getOperator(nm.getLangBasic().getInt2(), nm.getLangBasic().Mul));
	EXPECT_TRUE(op2);
	EXPECT_EQ(*op2, *nm.getLangBasic().getSignedIntMul());
	EXPECT_NE(*op2, *nm.getLangBasic().getUnsignedIntMul());

	LiteralPtr op3 = dynamic_pointer_cast<const Literal>(nm.getLangBasic().getOperator(nm.getLangBasic().getBool(), nm.getLangBasic().Eq));
	EXPECT_TRUE(op3);
	EXPECT_EQ(*op3, *nm.getLangBasic().getBoolEq());
	EXPECT_NE(*op3, *nm.getLangBasic().getBoolNe());
}

TEST(LangBasic, Derived) {
	NodeManager nm;

	// get a derived literal
	EXPECT_EQ("rec bool_and.{bool_and=fun(ref<bool,f,f,plain> v0, ref<(()=>bool),f,f,plain> v1) {if(ref_deref(v0)) {return ref_deref(v1)();} else {}; return "
	          "false;}}",
	          toString(*nm.getLangBasic().getBoolLAnd()));
}

TEST(LangBasic, DerivedMembership) {
	NodeManager nm;
	const BasicGenerator& gen = nm.getLangBasic();

	EXPECT_TRUE(gen.isArithOp(gen.getSignedIntAdd()));
}


// test all type and literal definitions
#define CHECK(id)                                                                                                                                              \
	EXPECT_TRUE(gen.get##id());                                                                                                                                \
	EXPECT_TRUE(gen.is##id(gen.get##id()));                                                                                                                    \
	EXPECT_TRUE(checks::check(gen.get##id()).empty()) << checks::check(gen.get##id());


#define TYPE(_id, _spec)                                                                                                                                       \
	TEST(LangBasic_types, _id) {                                                                                                                               \
		NodeManager nm;                                                                                                                                        \
		const BasicGenerator& gen = nm.getLangBasic();                                                                                                         \
		CHECK(_id);                                                                                                                                            \
		EXPECT_FALSE(isDerived(gen.get##_id()));                                                                                                               \
	}

#define LITERAL(_id, _name, _spec)                                                                                                                             \
	TEST(LangBasic_literals, _id) {                                                                                                                            \
		NodeManager nm;                                                                                                                                        \
		const BasicGenerator& gen = nm.getLangBasic();                                                                                                         \
		CHECK(_id);                                                                                                                                            \
		EXPECT_FALSE(isDerived(gen.get##_id()));                                                                                                               \
	}

#define DERIVED(_id, _name, _spec)                                                                                                                             \
	TEST(LangBasic_derived, _id) {                                                                                                                             \
		NodeManager nm;                                                                                                                                        \
		const BasicGenerator& gen = nm.getLangBasic();                                                                                                         \
		CHECK(_id);                                                                                                                                            \
		EXPECT_TRUE(isDerived(gen.get##_id()));                                                                                                                \
	}

#include "insieme/core/lang/inspire_api/lang.def"

#undef DERIVED
#undef LITERAL
#undef TYPE
#undef CHECK
