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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(FreeVariables, BindTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: free variables within binds have not been recognized correctly
		// reason: recursive call for bound parameters was wrong =>

		// add some free variables
		std::map<string, NodePtr> symbols;
		symbols["v"] = builder.variable(manager.getLangBasic().getInt4(), 77);

		CallExprPtr call = analysis::normalize(builder.parseExpr(
				"let int = int<4> in (((int)=>int a)->int { return a(2); } ((int x)=> (2+v) + x))",
				symbols
		)).as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int.add(int.add(2, v77), v0)})", toString(*call));
		EXPECT_EQ("[AP(v77)]", toString(getFreeVariables(call)));
	}

	TEST(FreeVariables, AddressTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: free variables within binds have not been recognized correctly
		// reason: recursive call for bound parameters was wrong =>

		// add some free variables
		std::map<string, NodePtr> symbols;
		symbols["v"] = builder.variable(manager.getLangBasic().getInt4(), 77);

		CallExprPtr call = analysis::normalize(builder.parseExpr(
				"let int = int<4> in (((int)=>int a)->int { return a(2); } ((int x)=> (2+v) + x))",
				symbols
		)).as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int.add(int.add(2, v77), v0)})", toString(*call));
		EXPECT_EQ("[0-2-2-2-3]", toString(getFreeVariableAddresses(call)));
	}

	TEST(FreeVariables, RecursiveVariableBug_1) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: in a code fragment using the ?: operator free variables are not detected properly
		// reason: the lazy-construct created for the required arguments are based on a bind - binds
		// have not been handled correctly by the "getFreeVariable" utility

		// create example
		auto fun = builder.parseExpr(
				"let f = (int<4> x)->int<4> {"
				"	return (x==0)?1:f(x-1)*x;"
				"} in f"
		).as<LambdaExprPtr>();

		fun = transform::correctRecursiveLambdaVariableUsage(manager, fun);

		EXPECT_EQ(toVector(fun->getVariable()), getFreeVariables(fun->getLambda()))
			<< "Lambda:   " << core::printer::PrettyPrinter(fun->getLambda(), core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS | core::printer::PrettyPrinter::NO_EVAL_LAZY);
	}


	TEST(FreeVariables, RecursiveVariableBug_2) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: - the recursive variable of a function is identified as a free variable outside the recursion
		//      - this seems to only happens within mutual recursive functions
		// Reason: the normalize operation is broken - recursive variables are not preserved properly!!

		// create example
		auto code = builder.parseExpr(
				"let f = (int<4> x)->int<4> {"
				"	return (int<4> y)->int<4> {"
				"		return f(y);"
				"	}(x);"
				"} in f(3)"
		);

		ASSERT_TRUE(code);

		// normalize
//		code = builder.normalize(code);

		std::cout << "\n";
		std::cout << *code << "\n";
		std::cout << *builder.normalize(code) << "\n";
		std::cout << core::printer::PrettyPrinter(code, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS) << "\n";

		// get list of free variables
		auto freeVars = analysis::getFreeVariables(code);
		EXPECT_TRUE(freeVars.empty()) << freeVars;

		// should also be true for addresses
		auto freeAds = analysis::getFreeVariableAddresses(code);
		EXPECT_TRUE(freeAds.empty()) << freeAds;

	}



	TEST(AllVariables, BindTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: free variables within binds have not been recognized correctly
		// reason: recursive call for bound parameters was wrong =>

		// add some free variables
		TypePtr int4 = manager.getLangBasic().getInt4();
		std::map<string, NodePtr> symbols;
		symbols["v"] = builder.variable(int4, 77);

		CallExprPtr call = analysis::normalize(builder.parseExpr(
				"let int = int<4> in (((int)=>int a)->int { return a(2); } ((int x)=> (2+v) + x))",
				symbols
		)).as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int.add(int.add(2, v77), v0)})", toString(*call));
		EXPECT_EQ(utils::set::toSet<VariableSet>(
			builder.variable(int4, 0),
			builder.variable(builder.functionType(int4, int4, FK_CLOSURE), 1),
			builder.variable(int4, 77),
			builder.variable(builder.functionType(builder.functionType(int4, int4, FK_CLOSURE), int4), 0)
		), getAllVariables(call));
	}


	TEST(TypeUtils, isGeneric) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr var = builder.typeVariable("a");
		TypePtr constA = builder.genericType("type");

		EXPECT_TRUE(isGeneric(var));
		EXPECT_FALSE(isGeneric(constA));

		EXPECT_TRUE(isGeneric(builder.functionType(toVector(var), var)));
		EXPECT_TRUE(isGeneric(builder.functionType(toVector(var), constA)));
		EXPECT_FALSE(isGeneric(builder.functionType(toVector(constA), constA)));

		// also make sure that recursive types are not recognized
		{
			TypeVariablePtr rec = builder.typeVariable("list");
			TypePtr listElem = builder.structType(toVector(
					builder.namedType("load", manager.getLangBasic().getInt4()),
					builder.namedType("next", builder.refType(rec))
			));
			TypePtr constRecType = builder.recType(rec, builder.recTypeDefinition(toVector(builder.recTypeBinding(rec, listElem))));

			EXPECT_EQ("rec 'list.{'list=struct<load:int<4>,next:ref<'list>>}", toString(*constRecType));
			EXPECT_FALSE(isGeneric(constRecType));
		}

		// yet, a generic recursive type should be recognized
		{
			TypeVariablePtr rec = builder.typeVariable("list");
			TypePtr listElem = builder.structType(toVector(
					builder.namedType("load", builder.typeVariable("b")),
					builder.namedType("next", builder.refType(rec))
			));
			TypePtr constRecType = builder.recType(rec, builder.recTypeDefinition(toVector(builder.recTypeBinding(rec, listElem))));

			EXPECT_EQ("rec 'list.{'list=struct<load:'b,next:ref<'list>>}", toString(*constRecType));
			EXPECT_TRUE(isGeneric(constRecType));
		}
	}


	TEST(TypeUtils, getElementTypes) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");
		EXPECT_EQ(toVector<TypePtr>(), getElementTypes(A));

		TypePtr D = builder.genericType("D", toVector(A,B));
		EXPECT_EQ(toVector(A,B), getElementTypes(D));

		TypePtr fun = builder.functionType(toVector(A,B), C);
		EXPECT_EQ(toVector(A,B,C), getElementTypes(fun));

		TypePtr structType = builder.structType(toVector(builder.namedType("a", A), builder.namedType("d",D)));
		EXPECT_EQ(toVector(A,D), getElementTypes(structType));
	}

	TEST(ExitPoints, Basic) {

		NodeManager manager;
		IRBuilder builder(manager);

		vector<StatementAddress> res;

		// -----------  some stuff without exit points -----------------------
		res = getExitPoints(builder.parseStmt(" { } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { int<4> x = 4; } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { continue; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { break; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { while(true) { continue; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { while(true) { break; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { let f = ()->int<4> { return 0; }; f(); } "));
		EXPECT_TRUE(res.empty());

		// ----------- return exit points -----------------------

		res = getExitPoints(builder.parseStmt(" { return 0; } "));
		EXPECT_EQ("[0-0]", toString(res));

		res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { return 0; } } "));
		EXPECT_EQ("[0-0-3-0]", toString(res));

		res = getExitPoints(builder.parseStmt(" { while(true) { return 0; } } "));
		EXPECT_EQ("[0-0-1-0]", toString(res));

		res = getExitPoints(builder.parseStmt(" { while(true) { if (false) { return 0; } else { return 1; } } } "));
		EXPECT_EQ("[0-0-1-0-1-0,0-0-1-0-2-0]", toString(res));

		res = getExitPoints(builder.parseStmt(" { let f = ()->int<4> { return 0; }; f(); return 0; } "));
		EXPECT_EQ("[0-1]", toString(res));

		// ------------ break and continue ------------------------

		res = getExitPoints(builder.parseStmt(" { break; } "));
		EXPECT_EQ("[0-0]", toString(res));

		res = getExitPoints(builder.parseStmt(" { continue; } "));
		EXPECT_EQ("[0-0]", toString(res));

		res = getExitPoints(builder.parseStmt(" { if(true) { break; } else { continue; } } "));
		EXPECT_EQ("[0-0-1-0,0-0-2-0]", toString(res));

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
