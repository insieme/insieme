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
#include "insieme/core/ir_expressions.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/attributes.h"

using namespace insieme::core;
using namespace insieme::core::printer;

TEST(PrettyPrinter, Basic) {

	// check setup
	EXPECT_EQ(static_cast<unsigned>(0), PrettyPrinter::OPTIONS_DEFAULT);
	EXPECT_EQ(static_cast<unsigned>(PrettyPrinter::PRINT_BRACKETS | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY ),
			PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_EQ(static_cast<unsigned>(PrettyPrinter::OPTIONS_DETAIL | PrettyPrinter::PRINT_SINGLE_LINE),
			PrettyPrinter::OPTIONS_SINGLE_LINE);

	NodePtr ptr;

	PrettyPrinter printerA(ptr, PrettyPrinter::OPTIONS_DEFAULT);
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_BRACKETS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	PrettyPrinter printerB(ptr, PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_BRACKETS));
	EXPECT_FALSE(printerB.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	PrettyPrinter printerC(ptr, PrettyPrinter::OPTIONS_SINGLE_LINE);
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_BRACKETS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	printerC.setOption(PrettyPrinter::PRINT_DEREFS, false);
	EXPECT_FALSE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	printerC.setOption(PrettyPrinter::PRINT_DEREFS);
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	printerC.setOption(PrettyPrinter::PRINT_DEREFS, false);
	EXPECT_FALSE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));

}

TEST(PrettyPrinter, Wrapper) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	LiteralPtr lit = Literal::get(mgr, mgr.getLangBasic().getString(), "\"this is a string literal\"");
	LiteralPtr one = Literal::get(mgr, mgr.getLangBasic().getInt4(), "1");
	VariablePtr iter = Variable::get(mgr, mgr.getLangBasic().getInt4());
	ForStmtPtr forStmt = builder.forStmt(iter, one, one, one, lit);

	PrettyPrinter printerA(forStmt, PrettyPrinter::OPTIONS_DEFAULT);

	std::ostringstream ss1;
	SourceLocationMap srcMap = printAndMap(ss1, printerA);

	std::ostringstream ss2;
	ss2 << printerA;

	// EXPECT_EQ(ss2.str(), ss1.str());

	// print the map
	std::cout << ss2.str() << std::endl;
	std::cout << srcMap;

	// ForStmt loc
	SourceLocationMap::const_iterator it = srcMap.begin();
	EXPECT_EQ(forStmt, it->second);
	EXPECT_EQ(SourceLocation(0,0), it->first.first );
	EXPECT_EQ(SourceLocation(2,1), it->first.second );
	
	++it;

	// int<4> type loc
	EXPECT_EQ(mgr.getLangBasic().getInt4(), it->second);
	EXPECT_EQ(SourceLocation(0,9), it->first.first );
	EXPECT_EQ(SourceLocation(0,15), it->first.second );

	++it;

	// variable loc
	EXPECT_EQ(iter, it->second);
	EXPECT_EQ(SourceLocation(0,16), it->first.first );
	EXPECT_EQ(SourceLocation(0,18), it->first.second );
	
	++it;

	// init value (1) loc
	EXPECT_EQ(forStmt->getStart(), it->second);
	EXPECT_EQ(SourceLocation(0,21), it->first.first );
	EXPECT_EQ(SourceLocation(0,22), it->first.second );

	++it;

	// for loop end condition (1) loc
	EXPECT_EQ(forStmt->getEnd(), it->second);
	EXPECT_EQ(SourceLocation(0,26), it->first.first );
	EXPECT_EQ(SourceLocation(0,27), it->first.second );

	++it;

	// for loop step (1) loc
	EXPECT_EQ(forStmt->getStep(), it->second);
	EXPECT_EQ(SourceLocation(0,30), it->first.first );
	EXPECT_EQ(SourceLocation(0,31), it->first.second );

	++it;

	// for loop body (compound) loc
	EXPECT_EQ(forStmt->getBody(), it->second);
	EXPECT_EQ(SourceLocation(0,33), it->first.first );
	EXPECT_EQ(SourceLocation(2,1), it->first.second );

	++it;

	// for loop body (lit) loc
	EXPECT_EQ(forStmt->getBody()->getStatements()[0], it->second);
	EXPECT_EQ(SourceLocation(1,4), it->first.first );
	EXPECT_EQ(SourceLocation(1,30), it->first.second );

}

TEST(PrettyPrinter, HiddenAttributes) {

	NodeManager manager;
	IRBuilder builder(manager);
	auto& ext = manager.getLangExtension<analysis::AttributeExtension>();

	analysis::AttributePtr a1 = ext.getUnordered();

	ExpressionPtr expr = builder.intLit(1);
	expr = analysis::addAttribute(expr, a1);

	EXPECT_EQ("1", toString(PrettyPrinter(expr)));
	EXPECT_EQ("attr(1, ([unordered]))", toString(PrettyPrinter(expr, PrettyPrinter::OPTIONS_DETAIL)));
}

TEST(PrettyPrinter, StructSuperTypes) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr classA = builder.structType(toVector(builder.namedType("a", builder.genericType("A"))));
	EXPECT_EQ("let type000 = struct<\n\ta:A\n>;\n\ntype000", toString(PrettyPrinter(classA)));

	TypePtr classB = builder.structType(toVector(classA), toVector(builder.namedType("b", builder.genericType("B"))));
	EXPECT_EQ("let type000 = struct<\n\ta:A\n>;\n\nlet type001 = struct : type000 <\n\tb:B\n>;\n\ntype001", toString(PrettyPrinter(classB)));

	TypePtr classC = builder.structType(toVector(builder.parent(true, classB)), toVector(builder.namedType("c", builder.genericType("C"))));
	EXPECT_EQ("let type000 = struct<\n\ta:A\n>;\n\nlet type001 = struct : type000 <\n\tb:B\n>;\n\nlet type002 = struct : virtual type001 <\n\tc:C\n>;\n\ntype002", toString(PrettyPrinter(classC)));

}

TEST(PrettyPrinter, FunctionTypes) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr typeA = builder.genericType("A");
	TypePtr typeB = builder.genericType("B");
	TypePtr typeC = builder.genericType("C");
	TypePtr typeR = builder.genericType("R");

	TypePtr objC = builder.refType(typeC);

	TypePtr funA = builder.functionType(toVector(typeA, typeB), typeR, FK_PLAIN);
	TypePtr funB = builder.functionType(toVector(typeA, typeB), typeR, FK_CLOSURE);
	TypePtr funC = builder.functionType(toVector(objC, typeA, typeB), FK_CONSTRUCTOR);
	TypePtr funD = builder.functionType(toVector(objC), FK_DESTRUCTOR);
	TypePtr funE = builder.functionType(toVector(objC, typeA, typeB), typeR, FK_MEMBER_FUNCTION);

	EXPECT_EQ("(A, B) -> R", toString(PrettyPrinter(funA)));
	EXPECT_EQ("(A, B) => R", toString(PrettyPrinter(funB)));
	EXPECT_EQ("C::(A, B)", toString(PrettyPrinter(funC)));
	EXPECT_EQ("~C::()", toString(PrettyPrinter(funD)));
	EXPECT_EQ("C::(A, B) -> R", toString(PrettyPrinter(funE)));

}

TEST(PrettyPrinter, LambdaTypes) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr typeA = builder.genericType("A");
	TypePtr typeB = builder.genericType("B");
	TypePtr typeC = builder.genericType("C");
	TypePtr typeR = builder.genericType("R");

	TypePtr objC = builder.refType(typeC);

	FunctionTypePtr funA = builder.functionType(toVector(objC, typeA, typeB), typeR, FK_PLAIN);
	FunctionTypePtr funB = builder.functionType(toVector(objC, typeA, typeB), FK_CONSTRUCTOR);
	FunctionTypePtr funC = builder.functionType(toVector(objC), FK_DESTRUCTOR);
	FunctionTypePtr funD = builder.functionType(toVector(objC, typeA, typeB), typeR, FK_MEMBER_FUNCTION);

	StatementPtr body = builder.compoundStmt();

	VariablePtr varO = builder.variable(objC, 0);
	VariablePtr varA = builder.variable(typeA, 1);
	VariablePtr varB = builder.variable(typeB, 2);
	VariablePtr varC = builder.variable(typeC, 3);
	VariablePtr varR = builder.variable(typeR, 4);

	LambdaExprPtr lambdaA = builder.lambdaExpr(funA, toVector(varO, varA, varB), body);
	LambdaExprPtr lambdaB = builder.lambdaExpr(funB, toVector(varO, varA, varB), body);
	LambdaExprPtr lambdaC = builder.lambdaExpr(funC, toVector(varO), body);
	LambdaExprPtr lambdaD = builder.lambdaExpr(funD, toVector(varO, varA, varB), body);

	EXPECT_EQ("fun(ref<C> v0, A v1, B v2) -> R { }", toString(PrettyPrinter(lambdaA, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
	EXPECT_EQ("ctor C v0 :: (A v1, B v2) { }", toString(PrettyPrinter(lambdaB, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
	EXPECT_EQ("dtor ~C v0 :: () { }", toString(PrettyPrinter(lambdaC, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
	EXPECT_EQ("mfun C v0 :: (A v1, B v2) -> R { }", toString(PrettyPrinter(lambdaD, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));

}

TEST(PrettyPrinter, DerivedLiterals) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	// create a function
	auto type = builder.structType();
	auto var = builder.variable(type);
	auto val = builder.literal("x", type);

	auto fun = builder.lambdaExpr(builder.compoundStmt(), toVector(var));
	auto call = builder.callExpr(fun, val);

	EXPECT_FALSE(lang::isDerived(fun));

	EXPECT_EQ("let type000 = struct<\n\t\n>;\n\nlet fun000 = fun(type000 v1) -> unit { };\n\nfun000(x)", toString(PrettyPrinter(call)));

	// mark it derived
	lang::markAsDerived(fun, "id");
	EXPECT_TRUE(lang::isDerived(fun));

	EXPECT_EQ("let type000 = struct<\n\t\n>;\n\nid(x)", toString(PrettyPrinter(call)));

	// without derived interception
	EXPECT_EQ("let type000 = struct<\n\t\n>;\n\nlet fun000 = fun(type000 v1) -> unit { };\n\nfun000(x)", toString(PrettyPrinter(call, PrettyPrinter::PRINT_DERIVED_IMPL)));
}
