/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/core/lang/basic.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/checks/ir_checks.h"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(LangBasic, Generation) {
	NodeManager nm;

	EXPECT_TRUE(nm.getLangBasic().isInt4(nm.getLangBasic().getInt4()));
}

TEST(LangBasic, BoolChecks) {
	NodeManager nm;

	EXPECT_TRUE(nm.getLangBasic().isBuiltIn(nm.getLangBasic().getInt4()));
	EXPECT_TRUE(nm.getLangBasic().isBuiltIn(nm.getLangBasic().getSignedIntLShift()));
	// EXPECT_FALSE(nm.getLangBasic().isBuiltIn(GenericType::get(nm, "surelyNotBuiltInISincerelyHope__")));
}

TEST(LangBasic, StringGet) {
	NodeManager nm;

	EXPECT_EQ(nm.getLangBasic().getRedistribute(), nm.getLangBasic().getLiteral("redistribute"));
	// EXPECT_EQ(LiteralPtr(), nm.getLangBasic().getLiteral("surelyNotBuiltInISincerelyHope__"));
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
	EXPECT_EQ(4u, basic.getRealGroup().size());
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
	EXPECT_EQ("rec v0.{v0=fun(ref<ref<array<'elem,1>>> v1) {ref<array<'elem,1>> v2 = ref.deref(v1); ref.assign(v1, array.view(ref.deref(v1), 1)); return v2;}}",
			toString(*nm.getLangBasic().getArrayViewPostInc()));

}

TEST(LangBasic, DefinitionTest) {

	NodeManager nm;
	const BasicGenerator& gen = nm.getLangBasic();

	// test all type and literal definitions
#define CHECK(id) \
	std::cout << "Checking " #id "\n"; \
	EXPECT_TRUE(gen.get##id ()); \
	EXPECT_TRUE(gen.is##id(gen.get##id())); \
	EXPECT_TRUE(checks::check(gen.get##id()).empty()) << checks::check(gen.get##id());

#define TYPE(_id, _spec) CHECK(_id)
#define LITERAL(_id, _name, _spec) CHECK(_id)

#include "insieme/core/lang/lang.def"

#undef LITERAL
#undef TYPE
#undef CHECK

}
