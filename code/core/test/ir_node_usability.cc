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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

namespace insieme {
namespace core {


TEST(Node, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);

	IntValuePtr node = builder.intValue(14);

	EXPECT_TRUE(node->isValue());


	TypeList list;

	TupleTypePtr tuple1 = builder.tupleType(list);

	list.push_back(tuple1);
	TupleTypePtr tuple2 = builder.tupleType(list);

	EXPECT_EQ(0u, tuple1.getChildList().size());
	EXPECT_EQ(1u, tuple2.getChildList().size());

	EXPECT_EQ(NT_TupleType, tuple1.getNodeType());


	LiteralPtr lit = builder.literal(manager.getLangBasic().getBool(), "true");
	EXPECT_EQ(NT_Literal, lit->getNodeType());

	IfStmtPtr stmt = builder.ifStmt(lit, lit, lit);
	EXPECT_EQ(NT_IfStmt, stmt->getNodeType());
	EXPECT_EQ(3u, stmt->getChildList().size());

	EXPECT_EQ(lit, stmt->getChildList()[0]);
	EXPECT_EQ(builder.compoundStmt(lit), stmt->getChildList()[1]);
	EXPECT_EQ(builder.compoundStmt(lit), stmt->getChildList()[2]);

	EXPECT_EQ(lit, stmt->getChildNodeReference<0>());
	EXPECT_EQ(builder.compoundStmt(lit), stmt->getChildNodeReference<1>());
	EXPECT_EQ(builder.compoundStmt(lit), stmt->getChildNodeReference<2>());

	EXPECT_TRUE(typeid(stmt->getChildNodeReference<0>()) == typeid(ExpressionPtr));
	EXPECT_TRUE(typeid(stmt->getChildNodeReference<1>()) == typeid(CompoundStmtPtr));
	EXPECT_TRUE(typeid(stmt->getChildNodeReference<2>()) == typeid(CompoundStmtPtr));

	EXPECT_TRUE(typeid(&*stmt->getChildNodeReference<0>()) == typeid(const Expression*));
	EXPECT_TRUE(typeid(&*stmt->getChildNodeReference<1>()) == typeid(const CompoundStmt*));
	EXPECT_TRUE(typeid(&*stmt->getChildNodeReference<2>()) == typeid(const CompoundStmt*));

	EXPECT_EQ(&*lit, &*stmt->getChildNodeReference<0>());
	EXPECT_EQ(&*builder.compoundStmt(lit), &*stmt->getChildNodeReference<1>());
	EXPECT_EQ(&*builder.compoundStmt(lit), &*stmt->getChildNodeReference<2>());
}

TEST(Node, MemberTypeTraits) {

	EXPECT_TRUE(typeid(node_child_type<IfStmt,0>::type) == typeid(Expression));
	EXPECT_TRUE(typeid(node_child_type<IfStmt,1>::type) == typeid(CompoundStmt));

	EXPECT_TRUE(typeid(node_child_type<TupleType,1>::type) == typeid(Type));
	EXPECT_TRUE(typeid(node_child_type<TupleType,100>::type) == typeid(Type));
}


TEST(Node, MemberAccess) {
	NodeManager manager;
	IRBuilder builder(manager);

	LiteralPtr literal = builder.intLit(1);
	ExpressionPtr lit(literal);

	IfStmtPtr stmt = builder.ifStmt(lit, lit, lit);

	IfStmtPtr ptr(stmt);
	IfStmtAddress adr(stmt);

	ptr->getChildNodeReference<0>();
	adr->getChildNodeReference<0>();

	ExpressionPtr stmtPtr = ptr->getCondition();
	ExpressionAddress stmtAdr = adr->getCondition();

	ptr->getThenBody();
	ptr->getElseBody();

	adr->getThenBody();
	adr->getElseBody();

}

} // end namespace core
} // end namespace insieme

