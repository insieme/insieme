/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/utils/name_mangling.h"

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

		CallExprPtr call =
			analysis::normalize(builder.parseExpr("alias int = int<4>;  (a : (int)=>int)->int { return a(2); } ((x : int)=> (2+v) + x)", symbols))
			    .as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec _.{_=fun(ref<((int<4>)=>int<4>),f,f,plain> v0) {return ref_deref(v0)(2);}}(bind(v0){int_add(int_add(2, v77), v0)})", toString(*call));
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

		CallExprPtr call =
		    analysis::normalize(builder.parseExpr("alias int = int<4>; (a: (int)=>int)->int { return a(2); } ((x : int)=> (2+v) + x)", symbols))
		        .as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec _.{_=fun(ref<((int<4>)=>int<4>),f,f,plain> v0) {return ref_deref(v0)(2);}}(bind(v0){int_add(int_add(2, v77), v0)})", toString(*call));
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
		EXPECT_EQ(toVector(v2, v3),
		          getFreeVariables(builder.tryCatchStmt(builder.compoundStmt(v2), toVector(builder.catchClause(v1, builder.compoundStmt(v1, v3))))));

		EXPECT_EQ(toVector(v1, v2), getFreeVariables(builder.tryCatchStmt(
		                                body, toVector(builder.catchClause(v1, builder.compoundStmt(v2)), builder.catchClause(v2, builder.compoundStmt(v1))))));

		EXPECT_EQ(VariableList(), getFreeVariables(builder.tryCatchStmt(
		                              body, toVector(builder.catchClause(v1, builder.compoundStmt(v1)), builder.catchClause(v2, builder.compoundStmt(v2))))));
	}

	TEST(FreeVariables, RecursiveVariableBug_1) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: in a code fragment using the ?: operator free variables are not detected properly
		// reason: the lazy-construct created for the required arguments are based on a bind - binds
		// have not been handled correctly by the "getFreeVariable" utility

		// create example
		auto fun = builder.parseExpr("decl f : (int<4>)->int<4>;"
			                         "def f = (x : int<4>)->int<4> {"
			                         "	return (x==0)?1:f(x-1)*x;"
			                         "}; f")
			           .as<LambdaExprPtr>();

		fun = transform::correctRecursiveLambdaVariableUsage(manager, fun);

		EXPECT_EQ(VariableList(), getFreeVariables(fun->getLambda()))
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
		auto code = builder.parseExpr("decl f : (int<4>)->int<4>;"
			                          " def f = (x : int<4>)->int<4> {"
			                          "		return (y : int<4>)->int<4> {"
			                          "			return f(y);"
			                          "		}(x);"
			                          "	}; f(3)");

		ASSERT_TRUE(code);

		// normalize
		//		code = builder.normalize(code);

		// std::cout << "\n";
		// std::cout << *code << "\n";
		// std::cout << *builder.normalize(code) << "\n";
		// std::cout << core::printer::PrettyPrinter(code, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS) << "\n";

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

	TEST(FreeVariables, ImplicitlyDeclared) {
		NodeManager manager;
		IRBuilder builder(manager);

		{
			auto ret1 = builder.parseStmt("return 5;");
			EXPECT_FALSE(hasFreeVariables(ret1));
		}
		{
			auto for1 = builder.parseStmt("for(int<4> k = 0 .. 10) { }");
			EXPECT_FALSE(hasFreeVariables(for1));
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

		CallExprPtr call =
		    analysis::normalize(builder.parseExpr("alias int = int<4>; (a : (int)=>int)->int { return a(2); } ((x : int)=> (2+v) + x)", symbols))
		        .as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check variables
		EXPECT_EQ("rec _.{_=fun(ref<((int<4>)=>int<4>),f,f,plain> v0) {return ref_deref(v0)(2);}}(bind(v0){int_add(int_add(2, v77), v0)})", toString(*call));
		EXPECT_EQ(utils::set::toSet<VariableSet>(builder.variable(int4, 0), builder.variable(builder.refType(int4), 1), builder.variable(int4, 77),
			                                     builder.variable(builder.refType(builder.functionType(int4, int4, FK_CLOSURE)), 0)),
			      getAllVariables(call));
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
			TagTypeReferencePtr tag = builder.tagTypeReference("list");
			auto listElem = builder.structRecord(toVector(builder.field("load", manager.getLangBasic().getInt4()), builder.field("next", builder.refType(tag))));
			TypePtr constRecType = builder.tagType(tag, builder.tagTypeDefinition({ { tag, listElem } }));

			EXPECT_EQ("rec ^list.{^list=struct {load:int<4>,next:ref<^list,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName()
			          + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}}",
			          toString(*constRecType));
			EXPECT_FALSE(isGeneric(constRecType));
		}

		// yet, a generic recursive type should be recognized
		{
			TagTypeReferencePtr tag = builder.tagTypeReference("list");
			auto listElem = builder.structRecord(toVector(builder.field("load", builder.typeVariable("b")), builder.field("next", builder.refType(tag))));
			TypePtr constRecType = builder.tagType(tag, builder.tagTypeDefinition( { { tag, listElem } }));

			EXPECT_EQ("rec ^list.{^list=struct {load:'b,next:ref<^list,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName()
			          + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}}",
			          toString(*constRecType));
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

		TypePtr D = builder.genericType("D", toVector(A, B));
		EXPECT_EQ(toVector(A, B), getElementTypes(D));

		TypePtr fun = builder.functionType(toVector(A, B), C);
		EXPECT_EQ(toVector(A, B, C), getElementTypes(fun));

		TypePtr structType = builder.structType(toVector(builder.field("a", A), builder.field("d", D)));
		EXPECT_EQ("[AP(A),AP(D<A,B>)]", toString(getElementTypes(structType)));
	}

	TEST(ExitPoints, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		vector<StatementAddress> res;

		// -----------  some stuff without exit points -----------------------
		res = getExitPoints(builder.parseStmt(" { } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { var int<4> x = 4; } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { continue; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { for(int<4> i = 1 .. 10 ) { break; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { while(true) { continue; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { while(true) { break; } } "));
		EXPECT_TRUE(res.empty());

		res = getExitPoints(builder.parseStmt(" { ()->int<4> { return 0; }(); } "));
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

		res = getExitPoints(builder.parseStmt(" { ()->int<4> { return 0; }(); return 0; } "));
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

		EXPECT_PRED2(readOnly, builder.parseStmt("{ x; }", symbols), x);
		EXPECT_PRED2(readOnly, builder.parseStmt("{ *x; }", symbols), x);

		EXPECT_PRED2(notReadOnly, builder.parseStmt("{ x = 4; }", symbols), x);
		EXPECT_PRED2(notReadOnly, builder.parseStmt("{ (x : ref<int<4>>) -> unit { x = 3; }(x); }", symbols), x);

		EXPECT_PRED2(readOnly, builder.parseStmt("{ (x : int<4>) -> unit {}(*x); }", symbols), x);
	}

	TEST(IsReadOnly, Bug) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		VariablePtr x = builder.variable(builder.parseType("ref<struct { a : int<4>; }>"), 1);
		std::map<string, NodePtr> symbols;
		symbols["x"] = x;

		EXPECT_PRED2(notReadOnly, builder.parseStmt("{ x.a; }", symbols), x);
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
		list.push_back(builder.callExpr(mgr.getLangExtension<lang::ReferenceExtension>().getRefComponentAccess(), x, builder.uintLit(0),
		                                builder.getTypeLiteral(mgr.getLangBasic().getInt4())));
		StatementPtr stmt = builder.compoundStmt(list);
		EXPECT_PRED2(notReadOnly, stmt, x);
	}

	TEST(IsReadOnly, NestedFunctions) {
		// check whether usage in nested functions is discovered
		NodeManager mgr;
		IRBuilder builder(mgr);

		ExpressionPtr none = builder.parseExpr("(x : ref<int<4>>)->unit { *x; }");
		ExpressionPtr used = builder.parseExpr("(x : ref<int<4>>)->unit { x = 4; }");

		VariablePtr var = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 5);

		ExpressionPtr call1 = builder.callExpr(none, var);
		ExpressionPtr call2 = builder.callExpr(used, var);

		EXPECT_PRED2(readOnly, call1, var) << dumpColor(call1);
		EXPECT_PRED2(notReadOnly, call2, var) << dumpColor(call2);

		// check two parameters
		ExpressionPtr fun = builder.parseExpr("(a : ref<int<4>>, b : ref<int<4>>)->unit { a = *b; }");

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
		ExpressionPtr recFun =
			builder.parseExpr("alias ftype = (ref<int<4>>, ref<int<4>>, ref<int<4>>, ref<int<4>>)->unit;"
			                  "decl f : ftype; decl g : ftype;"
			                  "def f = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { b = *a; d = *b; g(a,b,c,d); };"
			                  "def g = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { c = *a; d = *c; f(a,b,c,d); };"
			                  "f");

		VariablePtr varC = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 8);
		VariablePtr varD = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 9);

		call = builder.callExpr(recFun, varA, varB, varC, varD);

		EXPECT_PRED2(readOnly, call, varA) << dumpColor(call);
		EXPECT_PRED2(notReadOnly, call, varB) << dumpColor(call);
		EXPECT_PRED2(notReadOnly, call, varC) << dumpColor(call);
		EXPECT_PRED2(notReadOnly, call, varD) << dumpColor(call);
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

		ExpressionPtr none = builder.parseExpr("(x : ref<int<4>>)->unit { *x; }");
		ExpressionPtr used = builder.parseExpr("(x : ref<int<4>>)->unit { x = 4; }");

		VariablePtr var = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 5);

		ExpressionPtr call1 = builder.callExpr(none, var);
		ExpressionPtr call2 = builder.callExpr(used, var);

		EXPECT_PRED2(readOnly, call1, var);
		EXPECT_PRED2(notReadOnly, call2, var);

		// check two parameters
		ExpressionPtr fun = builder.parseExpr("(a : ref<int<4>>, b : ref<int<4>>)->unit { a = *b; }");

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
		ExpressionPtr recFun =
			builder.parseExpr("alias ftype = (ref<int<4>>, ref<int<4>>, ref<int<4>>, ref<int<4>>)->unit;"
			                  "decl f : ftype; decl g : ftype;"
			                  "def f = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { b = *a; d = *b; g(a,b,c,d); };"
			                  "def g = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { c = *a; d = *c; f(a,b,c,d); };"
			                  "f");

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
			"alias ftype = (ref<int<4>>, ref<int<4>>, ref<int<4>>, ref<int<4>>)->unit;"
			"decl f : ftype;"
			"def f = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { var ref<int<4>> x = *d; f(x,a,b,c); };"
			"f");

		ExpressionPtr funB = builder.parseExpr(
			"alias ftype = (ref<int<4>>, ref<int<4>>, ref<int<4>>, ref<int<4>>)->unit;"
			"decl f : ftype;"
			"def f = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { var ref<int<4>> x = *d; d = 2; f(x,a,b,c); };"
			"f");

		VariablePtr varA = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 6);
		VariablePtr varB = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 7);
		VariablePtr varC = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 8);
		VariablePtr varD = builder.variable(builder.refType(mgr.getLangBasic().getInt4()), 9);

		auto callA = builder.callExpr(funA, varA, varB, varC, varD);
		auto callB = builder.callExpr(funB, varA, varB, varC, varD);

		EXPECT_PRED2(readOnly, callA, varA) << dumpColor(callA);
		EXPECT_PRED2(notReadOnly, callB, varA) << dumpColor(callB);
	}

	TEST(IsReadOnly, ForwardingMutualRecursive) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		ExpressionPtr funA = builder.parseExpr(
			"alias ftype = (ref<int<4>>, ref<int<4>>, ref<int<4>>, ref<int<4>>)->unit;"
			"decl f : ftype; decl g : ftype;"
			"def f = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { var ref<int<4>> x = *d; g(x,a,b,c); };"
			"def g = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { var ref<int<4>> x = *d; f(x,a,b,c); };"
			"f");

		ExpressionPtr funB =
		    builder.parseExpr(
			"alias ftype = (ref<int<4>>, ref<int<4>>, ref<int<4>>, ref<int<4>>)->unit;"
			"decl f : ftype; decl g : ftype;"
			"def f = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { var ref<int<4>> x = *d; d = 2; g(x,a,b,c); };"
			"def g = (a : ref<int<4>>, b : ref<int<4>>, c : ref<int<4>>, d : ref<int<4>>)->unit { var ref<int<4>> x = *d; d = 2; f(x,a,b,c); };"
			"f");

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

	TEST(isSideEffectFree, Expressions) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto notSideEffectFree = [](const ExpressionPtr& expr) { return !isSideEffectFree(expr); };

		IRBuilder::EagerDefinitionMap symbols { {"v", builder.variable(builder.refType(mgr.getLangBasic().getInt4())) } };

		EXPECT_TRUE(isSideEffectFree(builder.parseExpr(R"("Hello")")));
		EXPECT_TRUE(isSideEffectFree(builder.parseExpr(R"(5 + 3 - v)", symbols)));
		EXPECT_TRUE(notSideEffectFree(builder.parseExpr(R"(v = 5)", symbols)));
	}

	TEST(isSideEffectFree, Declarations) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto notSideEffectFree = [](const DeclarationPtr& decl) { return !isSideEffectFree(decl); };

		IRBuilder::EagerDefinitionMap symbols {
			{"S", builder.parseType("struct S { a: int<4>, b: int<4> }") },
			{"fun", builder.literal("fun", builder.parseType("ref<int<4>> -> int<4>")) },
			{"v", builder.variable(builder.refType(mgr.getLangBasic().getInt4())) }
		};

		EXPECT_TRUE(isSideEffectFree(builder.parseStmt(R"(var ref<int<4>> v = 1;)").as<DeclarationStmtPtr>()->getDeclaration()));
		EXPECT_TRUE(isSideEffectFree(builder.parseStmt(R"(var ref<S> s = <s>(1,2);)", symbols).as<DeclarationStmtPtr>()->getDeclaration()));
		EXPECT_TRUE(notSideEffectFree(builder.parseStmt(R"(var ref<S> s = <s>(fun(v),2);)", symbols).as<DeclarationStmtPtr>()->getDeclaration()));
	}

	TEST(IsParallel, Negative) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto prog = builder.parseProgram(R"1N5P1RE(
			alias int = int<4>;
			decl taskfun : (int) -> unit;
			def taskfun = (v : int) -> unit {
				if(v == 0) { return; }
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
			alias int = int<4>;
			decl taskfun : (int) -> unit;
			def taskfun = (v : int) -> unit {
				if(v == 0) { return; }
				parallel(job [1..1] => taskfun(v-1));
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
			alias int = int<4>;
			def fun = () -> unit {
				var int x = 1;
				var int y;
			};
			unit main() {
				fun();
				fun();
			}
		)1N5P1RE");

		EXPECT_EQ(8, countInstances(prog, builder.parseType("int<4>")));
		EXPECT_EQ(2, countInstances(prog, builder.intLit(1)));
	}

	TEST(Types, FreeTagTypeReferences) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto hasNoFreeTagTypeReferences = [](const TypePtr& type) {
			return !hasFreeTagTypeReferences(type);
		};

		// some basic types do not have free references
		EXPECT_PRED1(hasNoFreeTagTypeReferences, builder.getLangBasic().getInt16());

		// closed recursive types are also free of free tag type references
		EXPECT_PRED1(hasNoFreeTagTypeReferences, builder.parseType("decl struct S; def struct S { x : ref<S>; }; S"));

		// a tag type reference has a free tag type reference
		EXPECT_PRED1(hasFreeTagTypeReferences, builder.parseType("^T"));
		EXPECT_PRED1(hasFreeTagTypeReferences, builder.parseType("ref<^T>"));

		// but also within structs tag types may be free
		EXPECT_PRED1(hasFreeTagTypeReferences, builder.parseType("def struct S { x : ref<^T>; }; S"));

		// but if the free reference is closed again, it should not be identified as free
		EXPECT_PRED1(hasNoFreeTagTypeReferences, builder.parseType("def struct S { x : ref<^T>; }; struct T { y : S; }"));
	}

	TEST(Types, CanonicalType) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some basic types
		EXPECT_EQ("A", 				toString(*getCanonicalType(builder.parseType("A"))));
		EXPECT_EQ("int<4>", 		toString(*getCanonicalType(builder.parseType("int<4>"))));

		// generic types
		EXPECT_EQ("'a", 			toString(*getCanonicalType(builder.parseType("'a"))));
		EXPECT_EQ("'a", 			toString(*getCanonicalType(builder.parseType("'b"))));

		EXPECT_EQ("(('a)->'a)", 	toString(*getCanonicalType(builder.parseType("('a)->'a"))));
		EXPECT_EQ("(('a)->'a)", 	toString(*getCanonicalType(builder.parseType("('b)->'b"))));

		EXPECT_EQ("(('a,(('a)->'b))->'a)", 	toString(*getCanonicalType(builder.parseType("('b, ('b)->'c )->'b"))));
		EXPECT_EQ("(('a)->'b)", 	toString(*getCanonicalType(builder.parseType("('c)->'d"))));


		// recursive types
		TagTypePtr recType = builder.parseType("decl struct g; def struct g { x : int<4>; next : link<g>; }; g").as<TagTypePtr>();
		EXPECT_TRUE(recType->isRecursive());

		EXPECT_EQ(recType, getCanonicalType(recType));
		EXPECT_EQ(recType, getCanonicalType(recType->peel(0)));
		EXPECT_EQ(recType, getCanonicalType(recType->peel(1)));
		EXPECT_EQ(recType, getCanonicalType(recType->peel(2)));
		EXPECT_EQ(recType, getCanonicalType(recType->peel(3)));


		// mutual recursive types
		TagTypePtr typeA = builder.parseType("decl struct g; def struct h { x : int<4>; next : link<g>; }; def struct g { y : double; next : link<h>; }; h").as<TagTypePtr>();
		TagTypePtr typeB = builder.parseType("decl struct g; def struct h { x : int<4>; next : link<g>; }; def struct g { y : double; next : link<h>; }; g").as<TagTypePtr>();

//		std::cout << *typeA << "\n";
//		std::cout << *typeA->peel(0) << " = \n\t\t" << *getCanonicalType(typeA->peel(0)) << "\n";
//		std::cout << *typeA->peel(1) << " = \n\t\t" << *getCanonicalType(typeA->peel(1)) << "\n";
//		std::cout << *typeA->peel(2) << " = \n\t\t" << *getCanonicalType(typeA->peel(2)) << "\n";
//		std::cout << *typeA->peel(3) << " = \n\t\t" << *getCanonicalType(typeA->peel(3)) << "\n";

		EXPECT_EQ(typeA, getCanonicalType(typeA));
		EXPECT_EQ(typeA, getCanonicalType(typeA->peel(0)));
		EXPECT_EQ(typeA, getCanonicalType(typeA->peel(1)));
		EXPECT_EQ(typeA, getCanonicalType(typeA->peel(2)));
		EXPECT_EQ(typeA, getCanonicalType(typeA->peel(3)));

		EXPECT_EQ(typeB, getCanonicalType(typeB));
		EXPECT_EQ(typeB, getCanonicalType(typeB->peel(0)));
		EXPECT_EQ(typeB, getCanonicalType(typeB->peel(1)));
		EXPECT_EQ(typeB, getCanonicalType(typeB->peel(2)));
		EXPECT_EQ(typeB, getCanonicalType(typeB->peel(3)));


		// test nested
		std::map<string,NodePtr> symbols;
		symbols["A0"] = typeA->peel(0);
		symbols["A1"] = typeA->peel(1);
		symbols["A2"] = typeA->peel(2);
		symbols["A3"] = typeA->peel(3);

		auto ref = builder.parseType("ref<A0>", symbols);
		EXPECT_EQ(ref, getCanonicalType(builder.parseType("ref<A0>", symbols)));
		EXPECT_EQ(ref, getCanonicalType(builder.parseType("ref<A1>", symbols)));
		EXPECT_EQ(ref, getCanonicalType(builder.parseType("ref<A2>", symbols)));
		EXPECT_EQ(ref, getCanonicalType(builder.parseType("ref<A3>", symbols)));
	}

	TEST(Types, CanonicalType_PartiallyUnrolled) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// build up a mutual recursive type
		auto recType = builder.parseType(
				"decl struct X; decl struct Y; def struct X { a : T; y : Y; }; def struct Y { a : S; x : X; }; X"
			).as<TagTypePtr>();

		// check that the composed type is properly composed
		EXPECT_TRUE(core::checks::check(recType).empty()) << core::checks::check(recType);

		// make sure that standard properties hold
		EXPECT_TRUE(recType->isRecursive());


		// unroll first type manually
		TypeAddress fieldType = TagTypeAddress(recType)->getRecord()->getFieldType("y");
		auto nestedTag = builder.tagTypeReference("Y");
		TagTypeBindingMap definition;
		definition[nestedTag] = recType->getDefinition()[1]->getRecord();
		TagTypePtr nestedY = builder.tagType(nestedTag, builder.tagTypeDefinition(definition));
		auto partiallyUnrolled = core::transform::replaceNode(mgr, fieldType, nestedY).as<TagTypePtr>();

		// check that the composed type is properly composed
		EXPECT_TRUE(core::checks::check(partiallyUnrolled).empty()) << core::checks::check(partiallyUnrolled);

		// make sure that standard properties hold
		EXPECT_TRUE(partiallyUnrolled->isRecursive());

		// check whether canonicalization is capable of reverting the partial unroll
		EXPECT_EQ(recType, getCanonicalType(partiallyUnrolled));
		EXPECT_EQ(recType, getCanonicalType(partiallyUnrolled->peel()));

		std::map<string, NodePtr> symbols;
		symbols["R"] = recType;
		symbols["T"] = partiallyUnrolled->peel();

		EXPECT_EQ(
			builder.parseType("def struct Z { x : R; }; Z", symbols),
			getCanonicalType(builder.parseType("def struct Z { x : T; }; Z", symbols))
		);

	}


	TEST(Utils, MinimizeRecursiveTypes) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		auto a = builder.tagTypeReference("A");
		auto b = builder.tagTypeReference("B");
		auto c = builder.tagTypeReference("C");
		auto d = builder.tagTypeReference("D");

		auto sA = builder.structRecord("A", { builder.field("i", basic.getInt4()) });
		auto sB = builder.structRecord("B", { builder.field("a", a), builder.field("c", c) });
		auto sC = builder.structRecord("C", { builder.field("a", a), builder.field("b", b) });
		auto sD = builder.structRecord("D", { builder.field("b", b), builder.field("c", c) });

		auto def = builder.tagTypeDefinition({
			{ a,sA },
			{ b,sB },
			{ c,sC },
			{ d,sD }
		});

		auto D = builder.tagType(d, def);

		// check that it has been properly assembled
		EXPECT_TRUE(checks::check(D).empty()) << checks::check(D);
		EXPECT_FALSE(D->isRecursive());
		EXPECT_EQ(4, D->getDefinition().size());

		// normalize the type
		auto list = minimizeRecursiveGroup(D->getDefinition());

		EXPECT_EQ(4, list.size());

		EXPECT_TRUE(list[a]);
		EXPECT_TRUE(list[b]);
		EXPECT_TRUE(list[c]);
		EXPECT_TRUE(list[d]);

		EXPECT_FALSE(list[a]->isRecursive());
		EXPECT_TRUE(list[b]->isRecursive());
		EXPECT_TRUE(list[c]->isRecursive());
		EXPECT_FALSE(list[d]->isRecursive());

		EXPECT_EQ(1, list[a]->getDefinition().size());
		EXPECT_EQ(2, list[b]->getDefinition().size());
		EXPECT_EQ(2, list[c]->getDefinition().size());
		EXPECT_EQ(1, list[d]->getDefinition().size());

		EXPECT_TRUE(checks::check(list[a]).empty()) << checks::check(list[a]);
		EXPECT_TRUE(checks::check(list[b]).empty()) << checks::check(list[b]);
		EXPECT_TRUE(checks::check(list[c]).empty()) << checks::check(list[c]);
		EXPECT_TRUE(checks::check(list[d]).empty()) << checks::check(list[d]);

	}

	TEST(Utils, MinimizeRecursiveFunctions) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto funType = builder.parseType("() -> int<4>").as<FunctionTypePtr>();

		auto a = builder.lambdaReference(funType, "A");
		auto b = builder.lambdaReference(funType, "B");
		auto c = builder.lambdaReference(funType, "C");
		auto d = builder.lambdaReference(funType, "D");

		auto lA = builder.lambda(funType, VariableList(), builder.returnStmt(builder.intLit(1)));
		auto lB = builder.lambda(funType, VariableList(), builder.returnStmt(builder.add(builder.callExpr(a),builder.callExpr(c))));
		auto lC = builder.lambda(funType, VariableList(), builder.returnStmt(builder.add(builder.callExpr(a), builder.callExpr(b))));
		auto lD = builder.lambda(funType, VariableList(), builder.returnStmt(builder.add(builder.callExpr(b), builder.callExpr(c))));

		auto def = builder.lambdaDefinition({
			{ a,lA },
			{ b,lB },
			{ c,lC },
			{ d,lD }
		});

		auto D = builder.lambdaExpr(d, def);

		// check that it has been properly assembled
		EXPECT_TRUE(checks::check(D).empty()) << checks::check(D);
		//EXPECT_FALSE(D->isRecursive());
		EXPECT_EQ(4, D->getDefinition().size());

		// normalize the type
		auto list = minimizeRecursiveGroup(D->getDefinition());

		EXPECT_EQ(4, list.size());

		EXPECT_TRUE(list[a]);
		EXPECT_TRUE(list[b]);
		EXPECT_TRUE(list[c]);
		EXPECT_TRUE(list[d]);

		EXPECT_FALSE(list[a]->isRecursive());
		EXPECT_TRUE(list[b]->isRecursive());
		EXPECT_TRUE(list[c]->isRecursive());
		EXPECT_FALSE(list[d]->isRecursive());

		EXPECT_EQ(1, list[a]->getDefinition().size());
		EXPECT_EQ(2, list[b]->getDefinition().size());
		EXPECT_EQ(2, list[c]->getDefinition().size());
		EXPECT_EQ(1, list[d]->getDefinition().size());

		EXPECT_TRUE(checks::check(list[a]).empty()) << checks::check(list[a]);
		EXPECT_TRUE(checks::check(list[b]).empty()) << checks::check(list[b]);
		EXPECT_TRUE(checks::check(list[c]).empty()) << checks::check(list[c]);
		EXPECT_TRUE(checks::check(list[d]).empty()) << checks::check(list[d]);
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
