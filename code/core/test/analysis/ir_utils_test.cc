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
#include "insieme/core/lang/static_vars.h"

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
		EXPECT_EQ("[0-2-1-2-2-1-3-1]", toString(getFreeVariableAddresses(call)));
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

	TEST(FreeVariables, VarInType) {
		NodeManager manager;
		IRBuilder builder(manager);

		// add some free variables
		std::map<string, NodePtr> symbols;
		symbols["v"] = builder.variable(manager.getLangBasic().getInt4(), 77);

		TypePtr type = builder.parseType("array<int<4>,#v>", symbols);
		ASSERT_TRUE(type);

		NodePtr call = builder.parseExpr("() -> array<int<4>,#v> { return ref_temp(type_lit(array<int<4>,#v>)); }()", symbols);

		NodePtr call2 = builder.parseExpr("(v0 : uint<inf>) -> ptr<int<4>> { var uint<inf> bla = v0; return ptr_from_array(ref_new(type_lit(array<int<4>,#bla>))); }(lit(\"5\":uint<inf>))");

		// check free variable
		EXPECT_EQ("[AP(v77)]", toString(getFreeVariables(type)));
		EXPECT_EQ("[AP(v77)]", toString(getFreeVariables(call)));
		EXPECT_EQ("[]", toString(getFreeVariables(call2)));
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
		EXPECT_EQ(utils::set::toSet<VariableSet>(builder.variable(int4, 0), builder.variable(int4, 77),
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

		TypePtr numericType = builder.numericType(27);
		EXPECT_EQ("[]", toString(getElementTypes(numericType)));

	}

	TEST(TypeUtils, isRefOf) {
		NodeManager nm;
		IRBuilder b(nm);

		auto& basic = b.getLangBasic();

		auto ir1 = b.parseType("ref<int<4>>");
		auto ir2 = b.parseType("int<4>");
		auto type = basic.getInt4();

		EXPECT_FALSE(isRefOf(nullptr, type));
		EXPECT_FALSE(isRefOf(ir2, type));
		EXPECT_TRUE(isRefOf(ir1, type));
	}

	TEST(TypeUtils, isZero) {
		NodeManager nm;
		IRBuilder b(nm);

		auto expr1 = b.parseExpr("0");
		auto expr2 = b.parseExpr("0u");
		auto expr3 = b.parseExpr("0ul");
		auto expr4 = b.parseExpr("0ull");
		auto expr5 = b.parseExpr("0l");
		auto expr6 = b.parseExpr("0ll");
		auto expr7 = b.parseExpr("0.0f");
		auto expr8 = b.parseExpr("lit(\"0\" : int<4>)");
		auto expr9 = b.parseExpr("3");

		EXPECT_TRUE(isZero(expr1));
		EXPECT_TRUE(isZero(expr2));
		EXPECT_TRUE(isZero(expr3));
		EXPECT_TRUE(isZero(expr4));
		EXPECT_TRUE(isZero(expr5));
		EXPECT_TRUE(isZero(expr6));
		EXPECT_TRUE(isZero(expr7));
		EXPECT_TRUE(isZero(expr8));
		EXPECT_FALSE(isZero(expr9));

	}

	TEST(TypeUtils, isStaticVar) {
		NodeManager nm;
		IRBuilder b(nm);

		auto expr1 = b.parseAddressesStatement("{ var ref<int<4>> a; $a$; }");
		auto e1 = expr1[0].getAddressedNode();

		// TODO: include more tests!! positive tests....

		EXPECT_FALSE(isStaticVar(e1.as<ExpressionPtr>()));
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

	TEST(IsMaterializationOf, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto t = [&](const std::string& code) {
			return builder.parseType(code);
		};

		auto isNotMaterializationOf = [](const TypePtr& a, const TypePtr& b) {
			return !isMaterializationOf(a,b);
		};

		// some positive checks
		EXPECT_PRED2(isMaterializationOf, t("ref<A>"), t("A"));
		EXPECT_PRED2(isMaterializationOf, t("ref<int<4>>"), t("int<4>"));
		EXPECT_PRED2(isMaterializationOf, t("ref<int<4>>"), t("int<2>"));
		EXPECT_PRED2(isMaterializationOf, t("ref<int<4>>"), t("uint<2>"));

		EXPECT_PRED2(isMaterializationOf, t("ref<'a>"), t("'a"));
		EXPECT_PRED2(isMaterializationOf, t("ref<int<'a>>"), t("int<'a>"));

		EXPECT_PRED2(isMaterializationOf, t("ref<A,f,f,plain>"), t("ref<A,t,f,cpp_ref>"));
		EXPECT_PRED2(isMaterializationOf, t("ref<A,f,f,plain>"), t("ref<A,f,f,cpp_rref>"));

		// some negative checks
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A>"), t("B"));

		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,t,f,plain>"), t("A"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,t,plain>"), t("A"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,t,t,plain>"), t("A"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,f,cpp_ref>"), t("A"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,f,cpp_rref>"), t("A"));

		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,t,f,plain>"), t("ref<A,t,f,plain>"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,f,plain>"), t("ref<A,t,f,plain>"));

		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,t,f,plain>"), t("ref<A,t,f,cpp_ref>"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,t,plain>"), t("ref<A,t,f,cpp_ref>"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,t,t,plain>"), t("ref<A,t,f,cpp_ref>"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,f,cpp_ref>"), t("ref<A,t,f,cpp_ref>"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<A,f,f,cpp_rref>"), t("ref<A,t,f,cpp_ref>"));

		EXPECT_PRED2(isNotMaterializationOf, t("ref<int<4>>"), t("uint<4>"));

		EXPECT_PRED2(isNotMaterializationOf, t("ref<'a>"), t("'b"));
		EXPECT_PRED2(isNotMaterializationOf, t("ref<int<'a>>"), t("int<'b>"));

		EXPECT_PRED2(isNotMaterializationOf, t("ref<'a>"), t("int<4>"));
	}

	TEST(IsMaterializingCall, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		const lang::BasicGenerator& gen = builder.getLangBasic();

		LiteralPtr zero = builder.literal(gen.getUInt4(), "0");
		VariablePtr x = builder.variable(builder.refType(gen.getUInt4(),false,false,core::lang::ReferenceType::Kind::CppReference), 3);

		ExpressionPtr call1 = builder.callExpr(builder.refType(gen.getBool()), gen.getUnsignedIntEq(), x, zero);
		EXPECT_TRUE(isMaterializingCall(call1));

		ExpressionPtr call2 = builder.callExpr(gen.getBool(), gen.getUnsignedIntEq(), x, zero);
		EXPECT_FALSE(isMaterializingCall(call2));

		auto call3 = builder.parseStmt(
			"def fun = (a : int<4>) -> int<4> { return a; };"
			"fun(1) materialize;"
		);
		EXPECT_TRUE(isMaterializingCall(call3));

		auto call4 = builder.parseStmt(
			"def fun = (a : int<4>) -> int<4> { return a; };"
			"fun(1);"
		);
		EXPECT_FALSE(isMaterializingCall(call4));

		auto call5 = builder.parseStmt(
			"def fun = (a : 'a) -> 'a { return a; };"
			"fun(1);"
		);
		EXPECT_FALSE(isMaterializingCall(call5));

		auto call6 = builder.parseStmt(
			"def fun = (a : 'a) -> 'a { return a; };"
			"fun(1) materialize;"
		);
		EXPECT_TRUE(isMaterializingCall(call6));
	}

	TEST(IsMaterializingCall, Pick) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto pick = builder.parseExpr("pick([1,2,3])");

		EXPECT_TRUE(pick);
		EXPECT_FALSE(isMaterializingCall(pick));

	}

	TEST(IsMaterializingDecl, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto buildDecl = [&](const string& type, const string& init) {
			return builder.declaration(
					builder.normalize(builder.parseType(type)),
					builder.normalize(builder.parseExpr(init))
			);
		};

		// basic & subtyping
		EXPECT_TRUE(isMaterializingDecl(buildDecl("ref<int<4>>", "2")));
		EXPECT_TRUE(isMaterializingDecl(buildDecl("ref<int<8>>", "3")));
		EXPECT_TRUE(isMaterializingDecl(buildDecl("ref<int<8>>", "4u")));

		// only mutable references
		EXPECT_TRUE(isMaterializingDecl(buildDecl("ref<int<4>,f,f,plain>", "5")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,t,plain>", "5")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,f,plain>", "5")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,t,plain>", "5")));

		// no type variable instantiation
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<'a>>", "6")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<'a>", "7")));

		// mapping functions to binds
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<()=>'b>", "()-> int<4> { return 0; }")));
		EXPECT_TRUE(isMaterializingDecl(buildDecl("ref<()=>int<4>>", "()-> int<4> { return 0; }")));

		// pointer qualifier addition is allowed
		EXPECT_TRUE(isMaterializingDecl(buildDecl("ref<ptr<int<4>,t,f>>", R"(lit("p":ptr<int<4>,f,f>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<ptr<'a,t,f>>", R"(lit("p":ptr<int<4>,f,f>))")));

		// classes and structs materializing by (1) construction, (2) init expressions, or (3) implicit constructor calls
		string structA = "def struct A { ctor () { } ctor (p : real<8>) { }  ctor (x : ref<int<4>>) { } }; ";
		EXPECT_TRUE(isMaterializingDecl(buildDecl(structA + "ref<A>", structA + "A::(ref_decl(type_lit(ref<A>)))")));
		EXPECT_TRUE(isMaterializingDecl(buildDecl(structA + "ref<A>", structA + "<ref<A>>(ref_decl(type_lit(ref<A>))){}")));

		// implicit conversion constructors are not allowed in declarations
		EXPECT_FALSE(isMaterializingDecl(buildDecl(structA + "ref<A>", structA + "4.0")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl(structA + "ref<A>", structA + "lit(\"X\":ref<int<4>>)")));

		// cases which are NOT materializing
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>>", R"(lit("a":ref<int<4>>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("int<4>", R"(42)")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,f,cpp_ref>", R"(42)")));

		// test const / volatile combinations -- TODO: rethink whether all implicit kind casts should be allowed here
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,f,plain>", R"(lit("a":ref<int<4>,f,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,f,plain>", R"(lit("a":ref<int<4>,f,t,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,f,plain>", R"(lit("a":ref<int<4>,t,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,f,plain>", R"(lit("a":ref<int<4>,t,t,plain>))")));

		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,t,plain>", R"(lit("a":ref<int<4>,f,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,t,plain>", R"(lit("a":ref<int<4>,f,t,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,t,plain>", R"(lit("a":ref<int<4>,t,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,f,t,plain>", R"(lit("a":ref<int<4>,t,t,plain>))")));

		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,f,plain>", R"(lit("a":ref<int<4>,f,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,f,plain>", R"(lit("a":ref<int<4>,f,t,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,f,plain>", R"(lit("a":ref<int<4>,t,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,f,plain>", R"(lit("a":ref<int<4>,t,t,plain>))")));

		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,t,plain>", R"(lit("a":ref<int<4>,f,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,t,plain>", R"(lit("a":ref<int<4>,f,t,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,t,plain>", R"(lit("a":ref<int<4>,t,f,plain>))")));
		EXPECT_FALSE(isMaterializingDecl(buildDecl("ref<int<4>,t,t,plain>", R"(lit("a":ref<int<4>,t,t,plain>))")));
	}

	namespace {
		bool readOnly(const StatementPtr& stmt, const VariablePtr& var) {
			return isReadOnly(stmt, var);
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
		EXPECT_TRUE(isSideEffectFree(builder.parseExpr(R"(5 + 3)", symbols)));
		EXPECT_TRUE(isSideEffectFree(builder.parseExpr(R"(5 + 3 - v)", symbols)));
		EXPECT_TRUE(notSideEffectFree(builder.parseExpr(R"(v = 5)", symbols)));
	}

	TEST(isSideEffectFree, Declarations) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto notSideEffectFree = [](const DeclarationPtr& decl) { return !isSideEffectFree(decl); };

		IRBuilder::EagerDefinitionMap symbols {
			{"S", builder.parseType("struct S { a: int<4>; b: int<4>; }") },
			{"fun", builder.literal("fun", builder.parseType("(ref<int<4>>) -> int<4>")) },
			{"v", builder.variable(builder.refType(mgr.getLangBasic().getInt4())) }
		};

		EXPECT_TRUE(isSideEffectFree(builder.parseStmt(R"(var ref<int<4>> v = 1;)").as<DeclarationStmtPtr>()->getDeclaration()));
		EXPECT_TRUE(isSideEffectFree(builder.parseStmt(R"(var ref<S> s = <ref<S>>(ref_decl(type_lit(ref<s>))){1,2};)", symbols).as<DeclarationStmtPtr>()->getDeclaration()));
		EXPECT_TRUE(notSideEffectFree(builder.parseStmt(R"(var ref<S> s = <ref<S>>(ref_decl(type_lit(ref<s>))){fun(v),2};)", symbols).as<DeclarationStmtPtr>()->getDeclaration()));
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
				merge_all();
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
				merge_all();
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
				var int y = 2;
			};
			unit main() {
				fun();
				fun();
			}
		)1N5P1RE");

		EXPECT_EQ(12, countInstances(prog, builder.parseType("int<4>")));
		EXPECT_EQ(2, countInstances(prog, builder.intLit(1)));
	}

	TEST(CountInstancesOfNodeType, Simple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto prog = builder.parseProgram(R"1N5P1RE(
			alias int = int<4>;
			def fun = () -> unit {
				var int x = 1;
				var int y = 2;
			};
			unit main() {
				fun();
				fun();
			}
		)1N5P1RE");

		EXPECT_EQ(3, countInstancesOfNodeType(prog, NT_CompoundStmt));
		EXPECT_EQ(3, countInstancesOfNodeType(prog, NT_LambdaExpr));
		EXPECT_EQ(16, countInstancesOfNodeType(prog, NT_Literal));
		EXPECT_EQ(1, countInstancesOfNodeType(prog, NT_LambdaExpr, true));
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

//		// check that the composed type is properly composed
//		EXPECT_TRUE(core::checks::check(partiallyUnrolled).empty()) << core::checks::check(partiallyUnrolled);
//
//		// make sure that standard properties hold
//		EXPECT_TRUE(partiallyUnrolled->isRecursive());
//
//		// check whether canonicalization is capable of reverting the partial unroll
//		EXPECT_EQ(recType, getCanonicalType(partiallyUnrolled));
//		EXPECT_EQ(recType, getCanonicalType(partiallyUnrolled->peel()));
//
//		std::map<string, NodePtr> symbols;
//		symbols["R"] = recType;
//		symbols["T"] = partiallyUnrolled->peel();
//
//		EXPECT_EQ(
//			builder.parseType("def struct Z { x : R; }; Z", symbols),
//			getCanonicalType(builder.parseType("def struct Z { x : T; }; Z", symbols))
//		);

	}


	TEST(Utils, MinimizeRecursiveTypes) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		auto a = builder.tagTypeReference("A");
		auto b = builder.tagTypeReference("B");
		auto c = builder.tagTypeReference("C");
		auto d = builder.tagTypeReference("D");


		auto sA = Struct::getStructWithFieldsAndNoDefaults(mgr, "A", { builder.field("i", basic.getInt4()) });
		auto sB = Struct::getStructWithFieldsAndNoDefaults(mgr, "B", { builder.field("a", a), builder.field("c", c) });
		auto sC = Struct::getStructWithFieldsAndNoDefaults(mgr, "C", { builder.field("a", a), builder.field("b", b) });
		auto sD = Struct::getStructWithFieldsAndNoDefaults(mgr, "D", { builder.field("b", b), builder.field("c", c) });

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
