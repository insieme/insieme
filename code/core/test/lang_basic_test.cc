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

#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(LangBasic, Generation) {
	NodeManager nm;

	EXPECT_TRUE(nm.basic.isInt4(nm.basic.getInt4()));
}

TEST(LangBasic, BoolChecks) {
	NodeManager nm;

	EXPECT_TRUE(nm.basic.isBuiltIn(nm.basic.getInt4()));
	EXPECT_TRUE(nm.basic.isBuiltIn(nm.basic.getSignedIntLShift()));
	// EXPECT_FALSE(nm.basic.isBuiltIn(GenericType::get(nm, "surelyNotBuiltInISincerelyHope__")));
}

TEST(LangBasic, StringGet) {
	NodeManager nm;

	EXPECT_EQ(nm.basic.getBarrier(), nm.basic.getLiteral("barrier"));
	// EXPECT_EQ(LiteralPtr(), nm.basic.getLiteral("surelyNotBuiltInISincerelyHope__"));
}

TEST(LangBasic, Grouping) {
	NodeManager nm;

	EXPECT_TRUE(nm.basic.isUnsignedInt(nm.basic.getUInt4()));
	EXPECT_TRUE(nm.basic.isInt(nm.basic.getUInt4()));
	EXPECT_FALSE(nm.basic.isReal(nm.basic.getUInt4()));
}

TEST(LangBasic, OperatorGet) {
	NodeManager nm;

	LiteralPtr op = nm.basic.getOperator(nm.basic.getUInt4(), nm.basic.Add);
	EXPECT_TRUE(op);
	EXPECT_EQ(*op, *nm.basic.getUnsignedIntAdd());
	EXPECT_NE(*op, *nm.basic.getSignedIntAdd());

	LiteralPtr op2 = nm.basic.getOperator(nm.basic.getInt2(), nm.basic.Mul);
	EXPECT_TRUE(op2);
	EXPECT_EQ(*op2, *nm.basic.getSignedIntMul());
	EXPECT_NE(*op2, *nm.basic.getUnsignedIntMul());

	LiteralPtr op3 = nm.basic.getOperator(nm.basic.getBool(), nm.basic.Eq);
	EXPECT_TRUE(op3);
	EXPECT_EQ(*op3, *nm.basic.getBoolEq());
	EXPECT_NE(*op3, *nm.basic.getBoolNe());
}

TEST(LangBasic, DefinitionTest) {

	NodeManager nm;
	const BasicGenerator& gen = nm.basic;

	// test all type and literal definitions
#define CHECK(id) \
	EXPECT_TRUE(gen.get##id ()); \
	EXPECT_TRUE(gen.is##id(gen.get##id())); \

#define TYPE(_id, _spec) CHECK(_id)
#define LITERAL(_id, _name, _spec) CHECK(_id)

#include "insieme/core/lang/lang.def"

#undef LITERAL
#undef TYPE
#undef CHECK

}
