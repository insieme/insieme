/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
	        "let int = int<4>;  lambda ((int)=>int a)->int { return a(2); } (lambda (int x)=> (2+v) + x)",
	        symbols
	                                       )).as<CallExprPtr>();
	ASSERT_TRUE(call);
	
	// check free variables
	EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int_add(int_add(2, v77), v0)})", toString(*call));
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
	        "let int = int<4>; lambda ((int)=>int a)->int { return a(2); } (lambda (int x)=> (2+v) + x)",
	        symbols
	                                       )).as<CallExprPtr>();
	ASSERT_TRUE(call);
	
	// check free variables
	EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int_add(int_add(2, v77), v0)})", toString(*call));
	EXPECT_EQ("[0-2-2-2-3]", toString(getFreeVariableAddresses(call)));
}

TEST(FreeVariables, TryCatchTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	TypePtr type = builder.genericType("A");
	VariablePtr v1 = builder.variable(type, 1);
	VariablePtr v2 = builder.variable(type, 2);
	VariablePtr v3 = builder.variable(type, 3);
	
	CompoundStmtPtr body = builder.compoundStmt();
	
	EXPECT_EQ(VariableList(), getFreeVariables(builder.tryCatchStmt(body, toVector(builder.catchClause(v1, builder.compoundStmt(v1))))));
	EXPECT_EQ(toVector(v2), getFreeVariables(builder.tryCatchStmt(builder.compoundStmt(v2), toVector(builder.catchClause(v1, builder.compoundStmt(v1))))));
	EXPECT_EQ(toVector(v2,v3), getFreeVariables(builder.tryCatchStmt(builder.compoundStmt(v2), toVector(builder.catchClause(v1, builder.compoundStmt(v1,v3))))));
	
	EXPECT_EQ(toVector(v1,v2), getFreeVariables(builder.tryCatchStmt(body, toVector(
	              builder.catchClause(v1, builder.compoundStmt(v2)),
	              builder.catchClause(v2, builder.compoundStmt(v1))
	          ))));
	          
	EXPECT_EQ(VariableList(), getFreeVariables(builder.tryCatchStmt(body, toVector(
	              builder.catchClause(v1, builder.compoundStmt(v1)),
	              builder.catchClause(v2, builder.compoundStmt(v2))
	          ))));
}

TEST(FreeVariables, RecursiveVariableBug_1) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	// BUG: in a code fragment using the ?: operator free variables are not detected properly
	// reason: the lazy-construct created for the required arguments are based on a bind - binds
	// have not been handled correctly by the "getFreeVariable" utility
	
	// create example
	auto fun = builder.parseExpr(
	               "let f = lambda (int<4> x)->int<4> {"
	               "	return (x==0)?1:f(x-1)*x;"
	               "}; f"
	           ).as<LambdaExprPtr>();
	           
	fun = transform::correctRecursiveLambdaVariableUsage(manager, fun);
	
	EXPECT_EQ(toVector(fun->getVariable()), getFreeVariables(fun->getLambda()))
	        << "Lambda:   " << core::printer::PrettyPrinter(fun->getLambda(),
	                core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS | core::printer::PrettyPrinter::NO_EVAL_LAZY);
}


TEST(FreeVariables, RecursiveVariableBug_2) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	// BUG: - the recursive variable of a function is identified as a free variable outside the recursion
	//      - this seems to only happens within mutual recursive functions
	// Reason: the normalize operation is broken - recursive variables are not preserved properly!!
	
	// create example
	auto code = builder.parseExpr(
	                "let f = lambda (int<4> x)->int<4> {"
	                "	return lambda (int<4> y)->int<4> {"
	                "		return f(y);"
	                "	}(x);"
	                "};  f(3)"
	            );
	            
	ASSERT_TRUE(code);
	
	// normalize
//		code = builder.normalize(code);

	//std::cout << "\n";
	//std::cout << *code << "\n";
	//std::cout << *builder.normalize(code) << "\n";
	//std::cout << core::printer::PrettyPrinter(code, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS) << "\n";
	
	// get list of free variables
	auto freeVars = analysis::getFreeVariables(code);
	EXPECT_TRUE(freeVars.empty()) << freeVars;
	
	// should also be true for addresses
	auto freeAds = analysis::getFreeVariableAddresses(code);
	EXPECT_TRUE(freeAds.empty()) << freeAds;
	
}

TEST(FreeVariables, NestedScopeBug) {

	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();
	
	// BUG: - scopes are not respected when identifying free variables
	//				e.g. { { decl v1 = ..; } v1; }
	//      - v1 is free, but not identified as such
	
	{
		VariablePtr v1 = builder.variable(basic.getInt4(), 1);
		
		auto decl = builder.declarationStmt(v1, builder.intLit(14));
		auto inner = builder.compoundStmt(decl);
		auto outer = builder.compoundStmt(inner, v1);
		
//			dump(outer);

		// get free variables
		EXPECT_EQ("[AP(v1)]", toString(getFreeVariables(outer)));
	}
	
	// more complex stuff
	{
		VariablePtr v1 = builder.variable(basic.getInt4(), 1);
		VariablePtr v2 = builder.variable(basic.getInt4(), 2);
		
		auto decl = builder.declarationStmt(v1, builder.intLit(14));
		auto inner = builder.compoundStmt(decl, v1, v2);
		auto outer = builder.compoundStmt(inner);
		
//			dump(outer);

		// get free variables
		EXPECT_EQ("[AP(v2)]", toString(getFreeVariables(outer)));
	}
	
	{
		VariablePtr v1 = builder.variable(basic.getInt4(), 1);
		VariablePtr v2 = builder.variable(basic.getInt4(), 2);
		VariablePtr v3 = builder.variable(basic.getInt4(), 3);
		
		auto decl = builder.declarationStmt(v1, builder.intLit(14));
		auto inner = builder.compoundStmt(decl, v1, v2);
		auto outer = builder.compoundStmt(inner, v3);
		
//			dump(outer);

		// get free variables
		auto freeVars = getFreeVariables(outer);
		EXPECT_EQ(2u, freeVars.size());
		EXPECT_TRUE(contains(freeVars, v2)) << freeVars;
		EXPECT_TRUE(contains(freeVars, v3)) << freeVars;
	}
	
	{
		VariablePtr v1 = builder.variable(basic.getInt4(), 1);
		VariablePtr v2 = builder.variable(basic.getInt4(), 2);
		
		auto decl = builder.declarationStmt(v1, builder.intLit(14));
		auto outer = builder.compoundStmt(v1, decl, v1);
		
//			dump(outer);

		// get free variables
		EXPECT_EQ("[AP(v1)]", toString(getFreeVariables(outer)));
		EXPECT_EQ("[0-0]", toString(getFreeVariableAddresses(outer)));
	}
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
	        "let int = int<4>; lambda ((int)=>int a)->int { return a(2); } (lambda (int x)=> (2+v) + x)",
	        symbols
	                                       )).as<CallExprPtr>();
	ASSERT_TRUE(call);
	
	// check free variables
	EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int_add(int_add(2, v77), v0)})", toString(*call));
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
	
	res = getExitPoints(builder.parseStmt(" { decl int<4> x = 4; } "));
	EXPECT_TRUE(res.empty());
	
	res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { continue; } } "));
	EXPECT_TRUE(res.empty());
	
	res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { break; } } "));
	EXPECT_TRUE(res.empty());
	
	res = getExitPoints(builder.parseStmt(" { while(true) { continue; } } "));
	EXPECT_TRUE(res.empty());
	
	res = getExitPoints(builder.parseStmt(" { while(true) { break; } } "));
	EXPECT_TRUE(res.empty());
	
	res = getExitPoints(builder.parseStmt(" { let f = lambda ()->int<4> { return 0; }; f(); } "));
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
	
	res = getExitPoints(builder.parseStmt(" { let f = lambda ()->int<4> { return 0; }; f(); return 0; } "));
	EXPECT_EQ("[0-1]", toString(res));
	
	// ------------ break and continue ------------------------
	
	res = getExitPoints(builder.parseStmt(" { break; } "));
	EXPECT_EQ("[0-0]", toString(res));
	
	res = getExitPoints(builder.parseStmt(" { continue; } "));
	EXPECT_EQ("[0-0]", toString(res));
	
	res = getExitPoints(builder.parseStmt(" { if(true) { break; } else { continue; } } "));
	EXPECT_EQ("[0-0-1-0,0-0-2-0]", toString(res));
	
}

namespace {

bool readOnly(const StatementPtr& stmt, const VariablePtr& var) {
	return insieme::core::analysis::isReadOnly(stmt, var);
}

bool notReadOnly(const StatementPtr& stmt, const VariablePtr& var) {
	return !isReadOnly(stmt, var);
};

}

TEST(IsReadOnly, Basic) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	const auto& basic = mgr.getLangBasic();
	
	
	VariablePtr x = builder.variable(builder.refType(basic.getInt4()), 1);
	std::map<string, NodePtr> symbols;
	symbols["x"] = x;
	
	EXPECT_PRED2(readOnly, 	 builder.parseStmt("{ x; }",symbols), x);
	EXPECT_PRED2(readOnly,    builder.parseStmt("{ *x; }",symbols), x);
	
	EXPECT_PRED2(notReadOnly,    builder.parseStmt("{ x = 4; }",symbols), x);
	EXPECT_PRED2(notReadOnly,    builder.parseStmt("{ let f = lambda (ref<int<4>> x)->unit { x = 3; }; f(x); }",symbols), x);
	
	EXPECT_PRED2(readOnly,       builder.parseStmt("{ let f = lambda (int<4> x)->unit {}; f(*x); }",symbols), x);
	
}

TEST(IsReadOnly, Bug) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr x = builder.variable(builder.parseType("ref<struct { int<4> a; }>"), 1);
	std::map<string, NodePtr> symbols;
	symbols["x"] = x;
	
	EXPECT_PRED2(notReadOnly,    builder.parseStmt("{ x.a; }", symbols), x);
	
}

TEST(IsReadOnly, MemberAccess) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr x = builder.variable(builder.parseType("ref<array<int<4>,1>>"), 1);
	std::map<string, NodePtr> symbols;
//		symbols["x"] = x;

//		EXPECT_PRED2( notReadOnly,    builder.parseStmt("{ array.ref.elem.1D(x, 0); }", symbols), x);

	x = builder.variable(builder.parseType("ref<(int<4>, real<4>)>"), 1);
	symbols["x"] = x;
	
//		doesn't work
//		EXPECT_PRED2( notReadOnly,    builder.parseStmt("{ tuple.member.access(x, 0u, type<int<4>>); }", symbols), x);
	StatementList list;
	list.push_back(builder.callExpr(mgr.getLangBasic().getTupleRefElem(), x, builder.uintLit(0),
	                                builder.getTypeLiteral(mgr.getLangBasic().getInt4())));
	StatementPtr stmt = builder.compoundStmt(list);
	EXPECT_PRED2(notReadOnly, stmt, x);
}

TEST(IsReadOnly, NestedFunctions) {

	// check whether usage in nested functions is discovered
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	ExpressionPtr none = builder.parseExpr("lambda (ref<int<4>> x)->unit { *x; }");
	ExpressionPtr used = builder.parseExpr("lambda (ref<int<4>> x)->unit { x = 4; }");
	
	VariablePtr var = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 5);
	
	ExpressionPtr call1 = builder.callExpr(none, var);
	ExpressionPtr call2 = builder.callExpr(used, var);
	
	EXPECT_PRED2(readOnly, call1, var);
	EXPECT_PRED2(notReadOnly, call2, var);
	
	// check two parameters
	ExpressionPtr fun = builder.parseExpr("lambda (ref<int<4>> a, ref<int<4>> b)->unit { a = *b; }");
	
	VariablePtr varA = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 6);
	VariablePtr varB = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 7);
	
	ExpressionPtr call = builder.callExpr(fun, varA, varB);
	
	EXPECT_PRED2(notReadOnly, call, varA);
	EXPECT_PRED2(readOnly, call, varB);
	
	
	// check recursive functions
	// 	a ... just read
	//  b ... written in f, not in g
	//  c ... written in g, not in f
	//  d ... written in both
	ExpressionPtr recFun = builder.parseExpr(
	                           "let f,g = "
	                           "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { b = *a; d = *b; g(a,b,c,d); },"
	                           "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { c = *a; d = *c; f(a,b,c,d); };"
	                           "f"
	                       );
	                       
	VariablePtr varC = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 8);
	VariablePtr varD = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 9);
	
	call = builder.callExpr(recFun, varA, varB, varC, varD);
	
	EXPECT_PRED2(readOnly, call, varA);
	EXPECT_PRED2(notReadOnly, call, varB);
	EXPECT_PRED2(notReadOnly, call, varC);
	EXPECT_PRED2(notReadOnly, call, varD);
	
}
namespace {
bool isNotReadOnlyWithinScope(const StatementPtr& stmt, const VariablePtr& var) {
	return !isReadOnlyWithinScope(stmt, var);
};

}
TEST(IsReadOnly, LocalScope) {

	// check whether usage in nested functions is discovered
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	ExpressionPtr none = builder.parseExpr("lambda (ref<int<4>> x)->unit { *x; }");
	ExpressionPtr used = builder.parseExpr("lambda (ref<int<4>> x)->unit { x = 4; }");
	
	VariablePtr var = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 5);
	
	ExpressionPtr call1 = builder.callExpr(none, var);
	ExpressionPtr call2 = builder.callExpr(used, var);
	
	EXPECT_PRED2(readOnly, call1, var);
	EXPECT_PRED2(notReadOnly, call2, var);
	
	// check two parameters
	ExpressionPtr fun = builder.parseExpr("lambda (ref<int<4>> a, ref<int<4>> b)->unit { a = *b; }");
	
	VariablePtr varA = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 6);
	VariablePtr varB = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 7);
	
	ExpressionPtr call = builder.callExpr(fun, varA, varB);
	
	EXPECT_PRED2(isNotReadOnlyWithinScope, call, varA);
	EXPECT_PRED2(isNotReadOnlyWithinScope, call, varB);
	
	
	// check recursive functions
	// 	a ... just read
	//  b ... written in f, not in g
	//  c ... written in g, not in f
	//  d ... written in both
	ExpressionPtr recFun = builder.parseExpr(
	                           "let f,g = "
	                           "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { b = *a; d = *b; g(a,b,c,d); },"
	                           "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { c = *a; d = *c; f(a,b,c,d); };"
	                           "f"
	                       );
	                       
	VariablePtr varC = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 8);
	VariablePtr varD = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 9);
	
	call = builder.callExpr(recFun, varA, varB, varC, varD);
	
	EXPECT_PRED2(isNotReadOnlyWithinScope, call, varA);
	EXPECT_PRED2(isNotReadOnlyWithinScope, call, varB);
	EXPECT_PRED2(isNotReadOnlyWithinScope, call, varC);
	EXPECT_PRED2(isNotReadOnlyWithinScope, call, varD);
	
}
TEST(IsReadOnly, Forwarding) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	
	ExpressionPtr funA = builder.parseExpr(
	                         "let f = "
	                         "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { decl ref<int<4>> x = var(*d); f(x,a,b,c); };"
	                         "f"
	                     );
	                     
	ExpressionPtr funB = builder.parseExpr(
	                         "let f = "
	                         "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { decl ref<int<4>> x = var(*d); d = 2; f(x,a,b,c); };"
	                         "f"
	                     );
	                     
	VariablePtr varA = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 6);
	VariablePtr varB = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 7);
	VariablePtr varC = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 8);
	VariablePtr varD = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 9);
	
	auto callA = builder.callExpr(funA, varA, varB, varC, varD);
	auto callB = builder.callExpr(funB, varA, varB, varC, varD);
	
	EXPECT_PRED2(readOnly, callA, varA);
	EXPECT_PRED2(notReadOnly, callB, varA);
	
}

TEST(IsReadOnly, ForwardingMutualRecursive) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	
	ExpressionPtr funA = builder.parseExpr(
	                         "let f,g = "
	                         "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { decl ref<int<4>> x = var(*d); g(x,a,b,c); },"
	                         "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { decl ref<int<4>> x = var(*d); f(x,a,b,c); };"
	                         " f"
	                     );
	                     
	ExpressionPtr funB = builder.parseExpr(
	                         "let f,g = "
	                         "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { decl ref<int<4>> x = var(*d); d = 2; g(x,a,b,c); },"
	                         "	lambda (ref<int<4>> a, ref<int<4>> b, ref<int<4>> c, ref<int<4>> d)->unit { decl ref<int<4>> x = var(*d); d = 2; f(x,a,b,c); };"
	                         "f"
	                     );
	                     
	VariablePtr varA = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 6);
	VariablePtr varB = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 7);
	VariablePtr varC = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 8);
	VariablePtr varD = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 9);
	
	auto callA = builder.callExpr(funA, varA, varB, varC, varD);
	auto callB = builder.callExpr(funB, varA, varB, varC, varD);
	
	EXPECT_PRED2(readOnly, callA, varA);
	EXPECT_PRED2(readOnly, callA, varB);
	EXPECT_PRED2(readOnly, callA, varC);
	EXPECT_PRED2(readOnly, callA, varD);
	EXPECT_PRED2(notReadOnly, callB, varA);
	EXPECT_PRED2(notReadOnly, callB, varB);
	EXPECT_PRED2(notReadOnly, callB, varC);
	EXPECT_PRED2(notReadOnly, callB, varD);
	
}

TEST(IsParallel, Negative) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	auto prog = builder.parseProgram(R"1N5P1RE(
			let int = int<4>;
			let taskfun = lambda (int v) -> unit {
				if(v == 0) return;
				taskfun(v-1);
				mergeAll();
			};
			unit main() {
				taskfun(1);
			}
		)1N5P1RE");

	EXPECT_FALSE(isParallel(prog));
}
TEST(IsParallel, Positive) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	auto prog = builder.parseProgram(R"1N5P1RE(
			let int = int<4>;
			let taskfun = lambda (int v) -> unit {
				if(v == 0) return;
				parallel(job([1:1], taskfun(v-1)));
				mergeAll();
			};
			unit main() {
				taskfun(1);
			}
		)1N5P1RE");

	EXPECT_TRUE(isParallel(prog));
}

TEST(CountInstances, Simple) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	auto prog = builder.parseProgram(R"1N5P1RE(
			let int = int<4>;
			let fun = lambda () -> unit {
				decl int x = 1;
				decl int y;
			};
			unit main() {
				fun();
				fun();
			}
		)1N5P1RE");

	EXPECT_EQ(countInstances(prog, builder.parseType("int<4>")), 10);
	EXPECT_EQ(countInstances(prog, builder.intLit(1)), 2);
}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
