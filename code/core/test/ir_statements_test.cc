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

#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "ir_node_test.inc"

namespace insieme {
namespace core {

	TEST(StatementsTest, Management) {
		NodeManager manager;
		NodeManager manager2;

		BreakStmtPtr bS = BreakStmt::get(manager);
		StatementPtr nS = CompoundStmt::get(manager);

		StatementPtr bSC = CompoundStmt::get(manager, bS);
		StatementPtr nSC = CompoundStmt::get(manager, nS);

		vector<StatementPtr> stmtVec;
		stmtVec.push_back(bS);
		stmtVec.push_back(nSC);
		stmtVec.push_back(nS);
		stmtVec.push_back(bSC);
		CompoundStmtPtr bSCVec = CompoundStmt::get(manager, stmtVec);

		EXPECT_EQ(5u, manager.size());
		EXPECT_EQ(0u, manager2.size());

		CompoundStmtPtr bSCVec2 = CompoundStmt::get(manager2, stmtVec);

		EXPECT_EQ(5u, manager.size());
		EXPECT_EQ(5u, manager2.size());

		// TODO:
		//	DepthFirstVisitor<NodePtr> stmt2check([&](const NodePtr& cur) {
		//		EXPECT_TRUE(manager2.addressesLocal(cur));
		//	});
		//	stmt2check.visit(bSCVec2);

		EXPECT_FALSE(manager.addressesLocal(bSCVec2));
		EXPECT_TRUE(manager.contains(bSCVec2));
	}

	TEST(StatementsTest, CreationAndIdentity) {
		NodeManager manager;

		BreakStmtPtr bS = BreakStmt::get(manager);
		EXPECT_EQ(bS, BreakStmt::get(manager));

		CompoundStmtPtr nS = CompoundStmt::get(manager);
		EXPECT_NE(*bS, *nS);
	}

	TEST(StatementsTest, CompoundStmt) {
		NodeManager stmtMan;
		BreakStmtPtr bS = BreakStmt::get(stmtMan);
		ContinueStmtPtr cS = ContinueStmt::get(stmtMan);

		CompoundStmtPtr empty = CompoundStmt::get(stmtMan);
		CompoundStmtPtr empty2 = CompoundStmt::get(stmtMan);

		EXPECT_EQ(empty, empty2);

		CompoundStmtPtr bSC = CompoundStmt::get(stmtMan, bS);
		vector<StatementPtr> stmtVec;
		stmtVec.push_back(bS);
		CompoundStmtPtr bSCVec = CompoundStmt::get(stmtMan, stmtVec);
		EXPECT_EQ(bSC, bSCVec);
		EXPECT_EQ(*bSC, *bSCVec);
		stmtVec.push_back(cS);
		CompoundStmtPtr bScSCVec = CompoundStmt::get(stmtMan, stmtVec);
		EXPECT_NE(bSC, bScSCVec);
		EXPECT_NE((*bSC).hash(), (*bScSCVec).hash());
		EXPECT_EQ((*bSC)[0], (*bScSCVec)[0]);
		EXPECT_EQ("{break; continue;}", toString(*bScSCVec));
	}

	TEST(StatementsTest, IntLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		{
			LiteralPtr intLit = builder.literal(builder.getLangBasic().getInt2(), "-10");
			boost::optional<int> val = intLit->getValueAs<int>();
			assert_true(val) << "Type error: Cannot cast val to int!";
			EXPECT_EQ(*val, -10);
		}
		{
			LiteralPtr intLit = builder.literal(builder.getLangBasic().getInt2(), "0x10");
			boost::optional<unsigned long> val = intLit->getValueAs<unsigned long>();
			assert_true(val) << "Type error: Cannot cast val to unsigned long!";
;			EXPECT_EQ(static_cast<unsigned>(16), *val);
		}
		{
			LiteralPtr intLit = builder.literal(builder.getLangBasic().getInt2(), "-0x10");
			boost::optional<short> val = intLit->getValueAs<short>();
			assert_true(val) << "Type error: Cannot cast val to short!";
;			EXPECT_EQ(-16, *val);
		}
		{
			LiteralPtr intLit = builder.literal(builder.getLangBasic().getInt2(), "010");
			boost::optional<unsigned short> val = intLit->getValueAs<unsigned short>();
			assert_true(val) << "Type error: Cannot cast val to unsigned!";
;			EXPECT_EQ(static_cast<unsigned>(8), *val);
		}
	}

	TEST(StatementsTest, RealLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		{
			LiteralPtr floatLit = builder.literal(builder.getLangBasic().getFloat(), "0.4");
			boost::optional<float> val = floatLit->getValueAs<float>();
			assert_true(val) << "Type error: Cannot cast val to float!";
;			EXPECT_FLOAT_EQ(*val, 0.4);
		}
		{
			LiteralPtr intLit = builder.literal(builder.getLangBasic().getDouble(), "0.00001");
			boost::optional<double> val = intLit->getValueAs<double>();
			assert_true(val) << "Type error: Cannot cast val to double!";
;			EXPECT_DOUBLE_EQ(*val, 0.00001);
		}
	}

	TEST(StatementsTest, DefaultParams) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr one = builder.literal(builder.getLangBasic().getIntGen(), "1");
		DeclarationStmtPtr decl = builder.declarationStmt(builder.getLangBasic().getIntGen(), one);
		ForStmtPtr forStmt = builder.forStmt(decl, one, one, decl);

		EXPECT_EQ(one, forStmt->getStep());
	}


	TEST(StatementsTest, Break) {
		NodeManager manager;

		BreakStmtPtr stmt = BreakStmt::get(manager);

		EXPECT_EQ("break", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, NodeList());
	}

	TEST(StatementsTest, Continue) {
		NodeManager manager;

		ContinueStmtPtr stmt = ContinueStmt::get(manager);

		EXPECT_EQ("continue", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, NodeList());
	}

	TEST(StatementsTest, Return) {
		NodeManager manager;

		LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
		IRBuilder builder(manager);
		ReturnStmtPtr stmt = ReturnStmt::get(manager, literal, literal->getType());

		EXPECT_EQ("return 12", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(builder.declaration(literal->getType(), literal)));
	}

	TEST(StatementsTest, Goto) {
		NodeManager manager;

		StringValuePtr str = StringValue::get(manager, "test");
		GotoStmtPtr stmt = GotoStmt::get(manager, str);

		EXPECT_EQ("goto test", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(str));
	}

	TEST(StatementsTest, Label) {
		NodeManager manager;

		StringValuePtr str = StringValue::get(manager, "test");
		LabelStmtPtr stmt = LabelStmt::get(manager, str);

		EXPECT_EQ("test: ", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(str));
	}

	TEST(StatementsTest, Throw) {
		NodeManager manager;

		LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
		ThrowStmtPtr stmt = ThrowStmt::get(manager, literal);

		EXPECT_EQ("throw 12", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(literal));
	}

	TEST(StatementsTest, Declaration) {
		NodeManager manager;

		LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
		auto var = Variable::get(manager, manager.getLangBasic().getInt4(), 1);
		DeclarationStmtPtr stmt = DeclarationStmt::get(manager, var, literal);

		EXPECT_EQ("int<4> v1 = 12", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(Declaration::get(manager, manager.getLangBasic().getInt4(), literal), var));
	}

	TEST(StatementsTest, Compound) {
		NodeManager manager;

		LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
		DeclarationStmtPtr stmt1 = DeclarationStmt::get(manager, Variable::get(manager, manager.getLangBasic().getInt4(), 1), literal);
		DeclarationStmtPtr stmt2 = DeclarationStmt::get(manager, Variable::get(manager, manager.getLangBasic().getInt4(), 2), literal);
		DeclarationStmtPtr stmt3 = DeclarationStmt::get(manager, Variable::get(manager, manager.getLangBasic().getInt4(), 3), literal);

		CompoundStmtPtr cs0 = CompoundStmt::get(manager);
		CompoundStmtPtr cs1 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt1));
		CompoundStmtPtr cs2 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt1, stmt2));
		CompoundStmtPtr cs3 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt1, stmt2, stmt3));

		CompoundStmtPtr all[] = {cs0, cs1, cs2, cs3};
		for(int i = 0; i < 4; i++) {
			for(int j = 0; j < 4; j++) {
				EXPECT_EQ(i == j, *all[i] == *all[j]);
				EXPECT_EQ(i == j, (*all[i]).hash() == (*all[j]).hash());
			}
		}

		EXPECT_EQ("{}", toString(*cs0));
		EXPECT_EQ("{int<4> v1 = 12;}", toString(*cs1));
		EXPECT_EQ("{int<4> v1 = 12; int<4> v2 = 12;}", toString(*cs2));
		EXPECT_EQ("{int<4> v1 = 12; int<4> v2 = 12; int<4> v3 = 12;}", toString(*cs3));

		// check hash codes, children and cloning
		basicNodeTests(cs0, toVector<NodePtr>());
		basicNodeTests(cs1, toVector<NodePtr>(stmt1));
		basicNodeTests(cs2, toVector<NodePtr>(stmt1, stmt2));
		basicNodeTests(cs3, toVector<NodePtr>(stmt1, stmt2, stmt3));
	}

	TEST(StatementsTest, For) {
		NodeManager manager;
		IRBuilder builder(manager);

		VariablePtr var = Variable::get(manager, manager.getLangBasic().getInt4());
		LiteralPtr start = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
		LiteralPtr end = Literal::get(manager, manager.getLangBasic().getInt4(), "9");
		LiteralPtr step = Literal::get(manager, manager.getLangBasic().getInt4(), "2");

		StatementPtr body = builder.getNoOp();

		ForStmtPtr stmt = builder.forStmt(var, start, end, step, body);

		EXPECT_EQ("for(int<4> v1 = 1 .. 9 : 2) {}", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(stmt->getDeclaration(), end, step, body));
	}

	TEST(StatementsTest, While) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr condition = Literal::get(manager, manager.getLangBasic().getBool(), "true");
		StatementPtr body = builder.getNoOp();

		WhileStmtPtr stmt = builder.whileStmt(condition, body);

		EXPECT_EQ("while(true) {}", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(condition, body));
	}


	TEST(StatementsTest, If) {
		NodeManager manager;
		IRBuilder builder(manager);

		VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
		StatementPtr then = builder.getNoOp();
		StatementPtr other = builder.getNoOp();

		IfStmtPtr stmt = builder.ifStmt(var, then, other);

		EXPECT_EQ("if(v1) {} else {}", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(var, then, other));
	}

	TEST(StatementsTest, Switch) {
		NodeManager manager;
		IRBuilder builder(manager);

		VariablePtr var = Variable::get(manager, manager.getLangBasic().getInt4(), 1);

		LiteralPtr literalA = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
		LiteralPtr literalB = Literal::get(manager, manager.getLangBasic().getInt4(), "2");

		std::vector<SwitchCasePtr> cases;
		cases.push_back(builder.switchCase(literalA, builder.getNoOp()));
		cases.push_back(builder.switchCase(literalB, builder.compoundStmt(builder.continueStmt())));

		CompoundStmtPtr other = builder.compoundStmt(BreakStmt::get(manager));

		auto switchCases = SwitchCases::get(manager, cases);
		SwitchStmtPtr stmt = SwitchStmt::get(manager, var, switchCases, other);

		EXPECT_EQ("switch(v1) [ case 1: {} | case 2: {continue;} | default: {break;} ]", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(var, switchCases, other));
	}

	TEST(StatementsTest, TryCatch) {
		NodeManager manager;
		IRBuilder builder(manager);

		VariablePtr varA = builder.variable(builder.genericType("A"), 1);
		VariablePtr varB = builder.variable(builder.genericType("B"), 2);

		CompoundStmtPtr body = builder.compoundStmt();
		CatchClausePtr clauseA = builder.catchClause(varA, builder.compoundStmt(varA));
		CatchClausePtr clauseB = builder.catchClause(varB, builder.compoundStmt(varB));

		TryCatchStmtPtr stmt = builder.tryCatchStmt(body, toVector(clauseA, clauseB));

		EXPECT_EQ("try {} catch (A v1) {v1;} catch (B v2) {v2;}", toString(*stmt));

		// check hash codes, children and cloning
		basicNodeTests(stmt, toVector<NodePtr>(body, clauseA, clauseB));
	}

	TEST(StatementsTest, MarkerStmt) {
		NodeManager manager;

		TypePtr type = GenericType::get(manager, "A");
		LiteralPtr literal = Literal::get(manager, type, "1");

		MarkerStmtPtr markerA = MarkerStmt::get(manager, literal);
		MarkerStmtPtr markerB = MarkerStmt::get(manager, literal);

		EXPECT_NE(markerA, markerB);
		EXPECT_NE(*markerA, *markerB);

		EXPECT_EQ(literal, markerA->getSubStatement());
		EXPECT_EQ(markerA->getSubStatement(), markerB->getSubStatement());

		EXPECT_NE(markerA->getID(), markerB->getID());

		// check hash codes, children and cloning
		basicNodeTests(markerA, toList(markerA->getID(), literal));
		basicNodeTests(markerB, toList(markerB->getID(), literal));
	}


} // end namespace core
} // end namespace insieme
