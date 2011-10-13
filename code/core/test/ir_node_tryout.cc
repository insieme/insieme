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

#include "insieme/core/ir_node_tryout.h"

namespace insieme {
namespace core {


TEST(Node, Basic) {

	Node node(14);

	EXPECT_TRUE(node.isValue());


	TypeList list;

	TupleType tuple1(list);

	list.push_back(Ptr<Type>(&tuple1));
	TupleType tuple2(list);

	EXPECT_EQ(0, tuple1.getChildList().size());
	EXPECT_EQ(1, tuple2.getChildList().size());

	EXPECT_EQ(NT_TupleType, tuple1.getNodeType());


	Literal literal;
	EXPECT_EQ(NT_Literal, literal.getNodeType());

	Ptr<Literal> lit(&literal);

	If ifStmt(lit, lit, lit);
	EXPECT_EQ(NT_If, ifStmt.getNodeType());
	EXPECT_EQ(3, ifStmt.getChildList().size());

	EXPECT_EQ(lit, ifStmt.getChildList()[0]);
	EXPECT_EQ(lit, ifStmt.getChildList()[1]);
	EXPECT_EQ(lit, ifStmt.getChildList()[2]);

	EXPECT_EQ(lit, ifStmt.get<0>());
	EXPECT_EQ(lit, ifStmt.get<1>());
	EXPECT_EQ(lit, ifStmt.get<2>());

	EXPECT_TRUE(typeid(ifStmt.get<0>()) == typeid(ExpressionPtr));
	EXPECT_TRUE(typeid(ifStmt.get<1>()) == typeid(StatementPtr));
	EXPECT_TRUE(typeid(ifStmt.get<2>()) == typeid(StatementPtr));

	EXPECT_TRUE(typeid(&*ifStmt.get<0>()) == typeid(Expression*));
	EXPECT_TRUE(typeid(&*ifStmt.get<1>()) == typeid(Statement*));
	EXPECT_TRUE(typeid(&*ifStmt.get<2>()) == typeid(Statement*));

	EXPECT_EQ(&*lit, &*ifStmt.get<0>());
	EXPECT_EQ(&*lit, &*ifStmt.get<1>());
	EXPECT_EQ(&*lit, &*ifStmt.get<2>());
}

TEST(Node, MemberTypeTraits) {

	EXPECT_TRUE(typeid(node_child_type<If,0>::type) == typeid(Expression));
	EXPECT_TRUE(typeid(node_child_type<If,1>::type) == typeid(Statement));

	EXPECT_TRUE(typeid(node_child_type<TupleType,1>::type) == typeid(Type));
	EXPECT_TRUE(typeid(node_child_type<TupleType,100>::type) == typeid(Type));
}

TEST(Node, PointerTest) {

	Literal literal;

	// with real pointers

	Literal* p1 = &literal;
	Node* p2 = p1;

	EXPECT_EQ(*p1, *p2);
	EXPECT_EQ(&*p1, &*p2);

	p2 = p1;

	EXPECT_EQ(*p1, *p2);
	EXPECT_EQ(&*p1, &*p2);

	// with own pointers

	Ptr<Literal> q1(&literal);
	Ptr<Node> q2(&literal);

	EXPECT_EQ(*q1, *q2);
	EXPECT_EQ(&*q1, &*q2);

	q2 = q1;

	EXPECT_EQ(&*q1, &*q2);
	EXPECT_TRUE(&*q1 == &*q2);
	EXPECT_EQ(*q1, *q2);

//	EXPECT_EQ("", toString(*q1));
//	EXPECT_EQ("", toString(*q2));

}

TEST(Node, MemberAccess) {

	Literal literal;
	ExpressionPtr lit(&literal);

	If ifStmt(lit, lit, lit);

	NodePtr node(&ifStmt);

	IfPtr ptr(&ifStmt);
	IfAdr adr(ptr);

//	StatementPtr stmtPtr = If::getCondition(ptr);
//	StatementAdr stmtAdr = If::getCondition(adr);

	ptr->get<0>();

	ExpressionPtr stmtPtr = ptr->getCondition();
	ExpressionAdr stmtAdr = adr->getCondition();


//	adr.get<0>();
}

} // end namespace core
} // end namespace insieme

