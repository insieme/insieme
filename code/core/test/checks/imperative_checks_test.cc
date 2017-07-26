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

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/imperative_checks.h"

namespace insieme {
namespace core {
namespace checks {

	bool containsMSG(const MessageList& list, const Message& msg) {
		return contains(list.getAll(), msg);
	}

	TEST(UndeclaredVariableCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create a function literal
		TypePtr type = builder.genericType("int");
		VariablePtr varA = builder.variable(type);
		VariablePtr varB = builder.variable(type);

		ExpressionPtr init = builder.literal(type, "4");

		FunctionTypePtr funType = builder.functionType(TypeList(), type);

		NodePtr ok = builder.lambdaExpr(funType, toVector<VariablePtr>(), builder.declarationStmt(varA, init));
		NodePtr err = builder.lambdaExpr(funType, toVector<VariablePtr>(), builder.declarationStmt(varA, varB));


		CheckPtr typeCheck = makeRecursive(make_check<UndeclaredVariableCheck>());
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		NodeAddress errorAdr = NodeAddress(err).getAddressOfChild(2, 0, 1, 2, 0, 0, 1);
		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(errorAdr, EC_IMPERATIVE_UNDECLARED_VARIABLE_USAGE, "", Message::ERROR));

		//using the declared variable within it's initialization is ok too
		NodePtr ok2 = builder.lambdaExpr(funType, toVector<VariablePtr>(), builder.declarationStmt(varA, varA));
		EXPECT_TRUE(check(ok2, typeCheck).empty());
	}


	namespace {

		LambdaDefinitionPtr wrap(StatementPtr body) {
			NodeManager& manager = body->getNodeManager();
			IRBuilder builder(manager);

			// construct lambda
			FunctionTypePtr funType = builder.functionType(toVector<TypePtr>(), manager.getLangBasic().getUnit());
			LambdaReferencePtr lambdaRef = builder.lambdaReference(funType,"f");
			LambdaPtr lambda = builder.lambda(funType, toVector<VariablePtr>(), body);

			// create and lambda definitions
			return LambdaDefinition::get(manager, { { LambdaBinding::get(manager, lambdaRef, lambda) } });
		}

		bool isUndeclaredVariableError(const MessageList& msgs, const NodePtr& target) {
			bool res = true;
			res = res && static_cast<std::size_t>(1) == msgs.size();
			if(!res) { return res; }

			EXPECT_EQ(EC_IMPERATIVE_UNDECLARED_VARIABLE_USAGE, msgs[0].getErrorCode());
			res = res && msgs[0].getErrorCode() == EC_IMPERATIVE_UNDECLARED_VARIABLE_USAGE;

			EXPECT_EQ(*target, *msgs[0].getOrigin());
			res = res && *msgs[0].getOrigin() == *target;
			return res;
		}
	}

	TEST(UndeclaredVariableCheck, CompoundStmt) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.genericType("A");
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);

		DeclarationStmtPtr declA = builder.declarationStmt(varA, builder.literal(type, "X"));
		DeclarationStmtPtr declB = builder.declarationStmt(varB, builder.literal(type, "X"));

		StatementPtr useA = varA;
		StatementPtr useB = varB;

		CheckPtr scopeChecker = makeRecursive(make_check<UndeclaredVariableCheck>());

		// create compound statement combinations
		StatementPtr ok1 = builder.compoundStmt(declA, varA);
		StatementPtr ok2 = builder.compoundStmt(declB, varB);
		StatementPtr ok3 = builder.compoundStmt(declA, ok2, varA);

		EXPECT_EQ("{A v1 = X; v1;}", toString(*ok1));
		EXPECT_EQ("{A v2 = X; v2;}", toString(*ok2));
		EXPECT_EQ("{A v1 = X; {A v2 = X; v2;}; v1;}", toString(*ok3));

		EXPECT_TRUE(check(wrap(ok1), scopeChecker).empty());
		EXPECT_TRUE(check(wrap(ok2), scopeChecker).empty());
		EXPECT_TRUE(check(wrap(ok3), scopeChecker).empty());


		// create some illegal setups
		StatementPtr err = builder.compoundStmt(declA, varB);
		EXPECT_EQ("{A v1 = X; v2;}", toString(*err));
		EXPECT_FALSE(check(wrap(err), scopeChecker).empty());
		EXPECT_PRED2(isUndeclaredVariableError, check(wrap(err), scopeChecker), varB);

		err = builder.compoundStmt(builder.compoundStmt(declA, varA), varB);
		EXPECT_EQ("{{A v1 = X; v1;}; v2;}", toString(*err));
		EXPECT_FALSE(check(wrap(err), scopeChecker).empty());
		EXPECT_PRED2(isUndeclaredVariableError, check(wrap(err), scopeChecker), varB);

		err = builder.compoundStmt(builder.compoundStmt(declA, varB));
		EXPECT_EQ("{{A v1 = X; v2;};}", toString(*err));
		EXPECT_FALSE(check(wrap(err), scopeChecker).empty());
		EXPECT_PRED2(isUndeclaredVariableError, check(wrap(err), scopeChecker), varB);

		err = builder.compoundStmt(builder.compoundStmt(declA), varA);
		EXPECT_EQ("{{A v1 = X;}; v1;}", toString(*err));
		EXPECT_FALSE(check(wrap(err), scopeChecker).empty());
		EXPECT_PRED2(isUndeclaredVariableError, check(wrap(err), scopeChecker), varA);
	}

	TEST(UndeclaredVariableCheck, BindExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.genericType("A");
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);

		DeclarationStmtPtr declA = builder.declarationStmt(varA, builder.literal(type, "X"));
		DeclarationStmtPtr declB = builder.declarationStmt(varB, builder.literal(type, "X"));

		LiteralPtr fun0 = builder.literal(builder.functionType(toVector<TypePtr>(), type), "f");
		LiteralPtr fun1 = builder.literal(builder.functionType(toVector(type), type), "f");
		LiteralPtr fun2 = builder.literal(builder.functionType(toVector(type, type), type), "f");

		StatementPtr useA = varA;
		StatementPtr useB = varB;

		BindExprPtr bind00 = builder.bindExpr(toVector<VariablePtr>(), builder.callExpr(fun0, toVector<ExpressionPtr>()));
		BindExprPtr bind01 = builder.bindExpr(toVector<VariablePtr>(), builder.callExpr(fun1, toVector<ExpressionPtr>(builder.literal(type, "12"))));
		BindExprPtr bind11a = builder.bindExpr(toVector<VariablePtr>(varA), builder.callExpr(fun1, toVector<ExpressionPtr>(varA)));
		BindExprPtr bind11b = builder.bindExpr(toVector<VariablePtr>(varA), builder.callExpr(fun1, toVector<ExpressionPtr>(varB)));
		BindExprPtr bind11c = builder.bindExpr(toVector<VariablePtr>(varA), builder.callExpr(fun1, toVector<ExpressionPtr>(builder.callExpr(fun1, varA))));
		BindExprPtr bind21 = builder.bindExpr(toVector<VariablePtr>(varA), builder.callExpr(fun2, toVector<ExpressionPtr>(varA, varB)));


		CheckPtr scopeChecker = makeRecursive(make_check<UndeclaredVariableCheck>());

		StatementPtr cur;
		cur = builder.compoundStmt(bind00);
		EXPECT_EQ("{bind(){f()};}", toString(*cur));
		EXPECT_TRUE(check(wrap(cur), scopeChecker).empty());

		cur = builder.compoundStmt(bind01);
		EXPECT_EQ("{bind(){f(12)};}", toString(*cur));
		EXPECT_TRUE(check(wrap(cur), scopeChecker).empty());

		cur = builder.compoundStmt(bind11a);
		EXPECT_EQ("{bind(v1){f(v1)};}", toString(*cur));
		EXPECT_TRUE(check(wrap(cur), scopeChecker).empty());

		cur = builder.compoundStmt(bind11b);
		EXPECT_EQ("{bind(v1){f(v2)};}", toString(*cur));
		EXPECT_FALSE(check(wrap(cur), scopeChecker).empty());
		EXPECT_PRED2(isUndeclaredVariableError, check(wrap(cur), scopeChecker), varB);

		cur = builder.compoundStmt(bind11c);
		EXPECT_EQ("{bind(v1){f(f(v1))};}", toString(*cur));
		EXPECT_TRUE(check(wrap(cur), scopeChecker).empty());

		cur = builder.compoundStmt(bind21);
		EXPECT_EQ("{bind(v1){f(v1, v2)};}", toString(*cur));
		EXPECT_FALSE(check(wrap(cur), scopeChecker).empty());
		EXPECT_PRED2(isUndeclaredVariableError, check(wrap(cur), scopeChecker), varB);

		cur = builder.compoundStmt(declB, bind21);
		EXPECT_EQ("{A v2 = X; bind(v1){f(v1, v2)};}", toString(*cur));
		EXPECT_TRUE(check(wrap(cur), scopeChecker).empty());
	}

	TEST(UndeclaredVariableCheck, JobExpr) {
		// TODO: implement these checks!
	}

	TEST(UndeclaredVariableCheck, Exceptions) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto code = builder.normalize(builder.parseStmt(
		    R"(

			try {

			} catch(x : int<4>) {
				x;
			}

			)"));

		ASSERT_TRUE(code);

		CheckPtr scopeChecker = makeRecursive(make_check<UndeclaredVariableCheck>());

		EXPECT_TRUE(check(wrap(code), scopeChecker).empty()) << check(wrap(code), scopeChecker);
	}

} // end namespace checks
} // end namespace core
} // end namespace insieme
