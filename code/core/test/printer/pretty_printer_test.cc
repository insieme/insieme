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
	EXPECT_EQ(static_cast<unsigned>(PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::PRINT_ATTRIBUTES
	                                | PrettyPrinter::NO_EVAL_LAZY),
	          PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_EQ(static_cast<unsigned>(PrettyPrinter::OPTIONS_DETAIL | PrettyPrinter::PRINT_SINGLE_LINE), PrettyPrinter::OPTIONS_SINGLE_LINE);

	NodePtr ptr;

	PrettyPrinter printerA(ptr, PrettyPrinter::OPTIONS_DEFAULT);
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	PrettyPrinter printerB(ptr, PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_FALSE(printerB.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	PrettyPrinter printerC(ptr, PrettyPrinter::OPTIONS_SINGLE_LINE);
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_CASTS));
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
	EXPECT_EQ(SourceLocation(0, 0), it->first.first);
	EXPECT_EQ(SourceLocation(2, 1), it->first.second);

	++it;

	// int<4> type loc
	EXPECT_EQ(mgr.getLangBasic().getInt4(), it->second);
	EXPECT_EQ(SourceLocation(0, 5), it->first.first);
	EXPECT_EQ(SourceLocation(0, 11), it->first.second);

	++it;

	// 4 literal loc
	EXPECT_EQ(builder.numericType(builder.literal("4",builder.getLangBasic().getUIntInf())), it->second);
	EXPECT_EQ(SourceLocation(0, 9), it->first.first);
	EXPECT_EQ(SourceLocation(0, 10), it->first.second);

	++it;

	// variable loc
	EXPECT_EQ(iter, it->second);
	EXPECT_EQ(SourceLocation(0, 12), it->first.first);
	EXPECT_EQ(SourceLocation(0, 14), it->first.second);

	++it;

	// init value (1) loc
	EXPECT_EQ(forStmt->getStart(), it->second);
	EXPECT_EQ(SourceLocation(0, 17), it->first.first);
	EXPECT_EQ(SourceLocation(0, 18), it->first.second);

	++it;

	// for loop end condition (1) loc
	EXPECT_EQ(forStmt->getEnd(), it->second);
	EXPECT_EQ(SourceLocation(0, 22), it->first.first);
	EXPECT_EQ(SourceLocation(0, 23), it->first.second);

	++it;

	// for loop step (1) loc
	EXPECT_EQ(forStmt->getStep(), it->second);
	EXPECT_EQ(SourceLocation(0, 26), it->first.first);
	EXPECT_EQ(SourceLocation(0, 27), it->first.second);

	++it;

	// for loop body (compound) loc
	EXPECT_EQ(forStmt->getBody(), it->second);
	EXPECT_EQ(SourceLocation(0, 29), it->first.first);
	EXPECT_EQ(SourceLocation(2, 1), it->first.second);

	++it;

	// for loop body (lit) loc
	EXPECT_EQ(forStmt->getBody()->getStatements()[0], it->second);
	EXPECT_EQ(SourceLocation(1, 4), it->first.first);
	EXPECT_EQ(SourceLocation(1, 30), it->first.second);
}

TEST(PrettyPrinter, HiddenAttributes) {
	NodeManager manager;
	IRBuilder builder(manager);
	auto& ext = manager.getLangExtension<analysis::AttributeExtension>();

	analysis::AttributePtr a1 = ext.getUnordered();

	ExpressionPtr expr = builder.intLit(1);
	expr = analysis::addAttribute(expr, a1);

	//dumpText(expr);

	EXPECT_EQ("1", toString(PrettyPrinter(expr)));
	EXPECT_EQ("attr(1, [unordered])", toString(PrettyPrinter(expr, PrettyPrinter::OPTIONS_DETAIL)));
}

TEST(PrettyPrinter, This) {
	NodeManager nm;
	IRBuilder builder(nm);

	auto type0 = builder.parseType(""
										   "def struct A { "
										   "    a : int<4>;"
										   "    lambda f : () -> int<4> {"
										   "        return *a;"
										   "    }"
										   "}; A");
	PrettyPrinter printer0(type0, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	std::cout << printer0;

	auto type1 = builder.parseType(""
										   "def struct A { "
										   "    a : int<4>;"
										   "    lambda f : () -> int<4> {"
										   "        return *this.a;"
										   "    }"
										   "}; A");
	PrettyPrinter printer1(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	std::cout << printer1;


}

TEST(PrettyPrinter, Declarations) {
	NodeManager nm;
	IRBuilder builder(nm);

	auto type0 = builder.parseType("def struct A { }; A");
	PrettyPrinter printer0(type0, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	EXPECT_EQ(toString(printer0), ""
		"decl struct A;\n"
		"decl A::dtor();\n"
		"def struct A {\n"
		"    dtor () { }\n"
		"};\n"
		"A") << printer0;

	auto type2 = builder.parseType("def struct A { }; def struct B : [ A ] { }; B");
	PrettyPrinter printer2(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	EXPECT_EQ(toString(printer2), ""
		"decl struct B;\n"
		"decl struct A;\n"
		"decl B::dtor();\n"
		"decl A::dtor();\n"
		"def struct B: [ public A ] {\n"
		"    dtor () { }\n"
		"};\n"
		"def struct A {\n"
		"    dtor () { }\n"
		"};\n"
		"B") << printer2;


	auto type1 = builder.parseType("def struct A { "
										   "lambda f : () -> unit {} "
										   "virtual lambda g : (a : int<4>) -> unit {}"
										   "const lambda h : (a : ref<int<4>,f,f,plain>, b : int<8>) -> unit  {}"
										   "volatile lambda i : (a : int<4>) -> int<4> {return a;}"
										   "}; A");
	PrettyPrinter printer1(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	EXPECT_EQ(toString(printer1), ""
		"decl struct A;\n"
		"decl A::dtor();\n"
		"decl A::f() -> unit;\n"
		"decl A::g(ref<int<4>,f,f,plain>) -> unit;\n"
		"decl A::h(ref<ref<int<4>,f,f,plain>,f,f,plain>,ref<int<8>,f,f,plain>) -> unit;\n"
		"decl A::i(ref<int<4>,f,f,plain>) -> int<4>;\n"
		"def struct A {\n"
		"    dtor () { }\n"
		"    function f : () -> unit { }\n"
		"    virtual function g : (v36 : ref<int<4>,f,f,plain>) -> unit { }\n"
		"    const function h : (v38 : ref<ref<int<4>,f,f,plain>,f,f,plain>,v39 : ref<int<8>,f,f,plain>) -> unit { }\n"
		"    volatile function i : (v41 : ref<int<4>,f,f,plain>) -> int<4> {\n"
		"        return  *v41;\n"
		"    }\n"
		"};\n"
		"A") << printer1;


	auto type3 = builder.parseType("def struct A { "
										   "function f : () -> unit {} "
										   "lambda j : () -> unit {} "
										   "virtual function g : (a : ref<int<4>,f,f,plain>) -> unit {}"
										   "const function h : (a : ref<int<4>,f,f,plain>, b : ref<int<8>,f,f,plain>) -> unit  {}"
										   "volatile function i : (a : ref<int<4>,f,f,plain>) -> int<4> {return *a;}"
										   "}; A");
	PrettyPrinter printer3(type3, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	EXPECT_EQ(toString(printer3), ""
		"decl struct A;\n"
		"decl A::dtor();\n"
		"decl A::f() -> unit;\n"
		"decl A::j() -> unit;\n"
		"decl A::g(ref<int<4>,f,f,plain>) -> unit;\n"
		"decl A::h(ref<int<4>,f,f,plain>,ref<int<8>,f,f,plain>) -> unit;\n"
		"decl A::i(ref<int<4>,f,f,plain>) -> int<4>;\n"
		"def struct A {\n"
		"    dtor () { }\n"
		"    function f : () -> unit { }\n"
		"    function j : () -> unit { }\n"
		"    virtual function g : (v57 : ref<int<4>,f,f,plain>) -> unit { }\n"
		"    const function h : (v59 : ref<int<4>,f,f,plain>,v60 : ref<int<8>,f,f,plain>) -> unit { }\n"
		"    volatile function i : (v62 : ref<int<4>,f,f,plain>) -> int<4> {\n"
		"        return  *v62;\n"
		"    }\n"
		"};\n"
		"A") << printer3;

	auto type4 = builder.parseStmt("def struct A {}; def f = ()->unit {}; {f();}");
	PrettyPrinter printer4(type4, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	EXPECT_EQ("decl f : () -> unit;\ndef f = () -> unit { };\n{\n    f();\n}", toString(printer4)) << printer4;

	auto type5 = builder.parseStmt("def struct A {}; def f = ()->int<4> {return 1;}; def g = ()->int<4> {return f();}; {f();}");
	PrettyPrinter printer5(type5, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
								  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
								  | PrettyPrinter::NO_LIST_SUGAR
								  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
								  | PrettyPrinter::PRINT_DERIVED_IMPL);

	EXPECT_EQ("decl f : () -> int<4>;\ndef f = () -> int<4> {\n    return 1;\n};\n{\n    f();\n}",toString(printer5)) << printer5;

}

TEST(PrettyPrinter, StructSuperTypes) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr classA = builder.structType(toVector(builder.field("a", builder.genericType("A"))));
	EXPECT_EQ("struct {\n    a : A;\n    dtor (){ }\n}", toString(PrettyPrinter(classA)));

	TypePtr classB = builder.structType(toVector(classA), toVector(builder.field("b", builder.genericType("B"))));
	EXPECT_EQ("struct : [ public struct {\n    a : A;\n    dtor (){ }\n} ] {\n    b : B;\n    dtor (){ }\n}", toString(PrettyPrinter(classB)));

	TypePtr classC = builder.structType(toVector(builder.parent(true, classB)), toVector(builder.field("c", builder.genericType("C"))));
	EXPECT_EQ(
	    "struct : [ virtual public struct : [ public struct {\n    a : A;\n    dtor (){ }\n} ] {\n    b : B;\n    dtor (){ }\n} ] {\n    c : C;\n    dtor (){ }\n}",
	    toString(PrettyPrinter(classC)));
}

TEST(PrettyPrinter, Parentheses) {
	NodeManager manager;
	IRBuilder builder(manager);

	ExpressionPtr A = builder.intLit(1);
	ExpressionPtr B = builder.intLit(2);
	ExpressionPtr C = builder.intLit(3);
	ExpressionPtr D = builder.intLit(4);
	ExpressionPtr E = builder.intLit(5);

	ExpressionPtr funA = builder.add(A, B);
	ExpressionPtr funB = builder.mul(funA, C);
	ExpressionPtr funC = builder.sub(D, E);
	ExpressionPtr funD = builder.div(funB, funC);

	EXPECT_EQ("(1+2)*3/(4-5)", toString(PrettyPrinter(funD)));
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
	TypePtr funF = builder.functionType(toVector(objC, typeA, typeB), typeR, FK_VIRTUAL_MEMBER_FUNCTION);

	EXPECT_EQ("(A,B) -> R", toString(PrettyPrinter(funA)));
	EXPECT_EQ("(A,B) => R", toString(PrettyPrinter(funB)));
//	EXPECT_EQ("ctor C::(A, B)", toString(PrettyPrinter(funC)));
//	EXPECT_EQ("~C::()", toString(PrettyPrinter(funD)));
//	EXPECT_EQ("method C::(A, B) -> R", toString(PrettyPrinter(funE)));
//	EXPECT_EQ("method C::(A, B) ~> R", toString(PrettyPrinter(funF)));
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
	FunctionTypePtr funE = builder.functionType(toVector(objC, typeA, typeB), typeR, FK_VIRTUAL_MEMBER_FUNCTION);

	StatementPtr body = builder.compoundStmt();

	VariablePtr varO = builder.variable(builder.refType(objC), 0);
	VariablePtr varA = builder.variable(builder.refType(typeA), 1);
	VariablePtr varB = builder.variable(builder.refType(typeB), 2);
	VariablePtr varC = builder.variable(builder.refType(typeC), 3);
	VariablePtr varR = builder.variable(builder.refType(typeR), 4);

	LambdaExprPtr lambdaA = builder.lambdaExpr(funA, toVector(varO, varA, varB), body);
	LambdaExprPtr lambdaB = builder.lambdaExpr(funB, toVector(varO, varA, varB), body);
	LambdaExprPtr lambdaC = builder.lambdaExpr(funC, toVector(varO), body);
	LambdaExprPtr lambdaD = builder.lambdaExpr(funD, toVector(varO, varA, varB), body);
	LambdaExprPtr lambdaE = builder.lambdaExpr(funE, toVector(varO, varA, varB), body);

	EXPECT_EQ("decl fun000 : (ref<ref<C,f,f,plain>,f,f,plain>, ref<A,f,f,plain>, ref<B,f,f,plain>) -> R;\ndef fun000 = (v0 : ref<ref<C,f,f,plain>,f,f,plain>, v1 : ref<A,f,f,plain>, v2 : ref<B,f,f,plain>) -> R { };\nfun000", toString(PrettyPrinter(lambdaA, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
//	EXPECT_EQ("ctor C v0 :: (ref<A,f,f,plain> v1, ref<B,f,f,plain> v2) { }", toString(PrettyPrinter(lambdaB, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
//	EXPECT_EQ("~C v0 :: () { }", toString(PrettyPrinter(lambdaC, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
	EXPECT_EQ("decl fun000 : (ref<ref<C,f,f,plain>,f,f,plain>, ref<A,f,f,plain>, ref<B,f,f,plain>) ~> R;\ndef fun000 = (v0 : ref<ref<C,f,f,plain>,f,f,plain>, v1 : ref<A,f,f,plain>, v2 : ref<B,f,f,plain>) ~> R { };\nfun000", toString(PrettyPrinter(lambdaE, PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));
}

TEST(PrettyPrinter, DerivedLiterals) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	// create a function
	auto type = builder.structType();
	auto var = builder.variable(builder.refType(type));
	auto val = builder.literal("x", type);

	auto fun = builder.normalize(builder.lambdaExpr(builder.getLangBasic().getUnit(), toVector(var), builder.compoundStmt()));
	auto call = builder.normalize(builder.callExpr(fun, val));

	EXPECT_FALSE(lang::isDerived(fun));

	EXPECT_EQ("decl fun000 : (ref<struct {\n    dtor (){ }\n},f,f,plain>) -> unit;\ndef fun000 = (v1 : ref<struct {\n    dtor (){ }\n},f,f,plain>) -> unit { };\nfun000(x)", toString(PrettyPrinter(call, PrettyPrinter::PRINT_DERIVED_IMPL)));

	// mark it derived
	lang::markAsDerived(fun, "id");
	EXPECT_TRUE(lang::isDerived(fun));

	EXPECT_EQ("decl id : (ref<struct {\n    dtor (){ }\n},f,f,plain>) -> unit;\ndef id = (v1 : ref<struct {\n    dtor (){ }\n},f,f,plain>) -> unit { };\nid(x)", toString(PrettyPrinter(call, PrettyPrinter::PRINT_DERIVED_IMPL)));

	// without derived interception
	EXPECT_EQ("id(x)", toString(PrettyPrinter(call)));
}

TEST(PrettyPrinter, JustOutermostScope) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = mgr.getLangBasic();

	DeclarationStmtPtr a = builder.declarationStmt(builder.variable(gen.getInt4()));

	StatementPtr stmt = builder.normalize(builder.parseStmt(R"1N5P1RE(

			decl lfun : (ref<int<4>>)->int<4>;
			def fun = (arg : int<4>)->int<4> { return arg + 1; };
			def rfun = (arg : ref<int<4>>)->int<4> { return *arg;};
		{
			var ref<int<4>> a;
			var ref<int<4>> b;
			var ref<int<4>> c;
			var ref<int<4>> d;
			var ref<int<4>> e;
			var ref<int<4>> f;
			var ref<int<4>> g;
			{
				a = 7;
				fun(*b);
				rfun(c);
				fun(fun(*d));
				fun(rfun(e));
				lfun(f);
				rfun(ref_var_init(lfun(g)));
			}
		}
	)1N5P1RE"));
	EXPECT_TRUE(stmt);

	std::string res = ""
			"decl fun : (ref<int<4>,f,f,plain>) -> int<4>;\n"
			"decl rfun : (ref<ref<int<4>,f,f,plain>,f,f,plain>) -> int<4>;\n"
			"def fun = (v1 : ref<int<4>,f,f,plain>) -> int<4> {\n"
			"    return  *v1+1;\n"
			"};\n"
			"def rfun = (v1 : ref<ref<int<4>,f,f,plain>,f,f,plain>) -> int<4> {\n"
			"    return  * *v1;\n"
			"};\n"
			"{\n"
			"    var ref<int<4>,f,f,plain> v0 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v1 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v2 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v3 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v4 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v5 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v6 = ref_var(type_lit(int<4>));\n"
			"    {\n"
			"        v0 = 7;\n"
			"        fun( *v1);\n"
			"        rfun(v2);\n"
			"        fun(fun( *v3));\n"
			"        fun(rfun(v4));\n"
			"        lfun(v5);\n"
			"        rfun(ref_var_init(lfun(v6)));\n"
			"    };\n"
			"}";

	EXPECT_EQ(res, toString(PrettyPrinter(stmt, PrettyPrinter::PRINT_DEREFS))) << toString(PrettyPrinter(stmt, PrettyPrinter::PRINT_DEREFS));
	std::string res2 = ""
			"{var ref<int<4>,f,f,plain> v0 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v1 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v2 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v3 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v4 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v5 = ref_var(type_lit(int<4>));\n"
			"    var ref<int<4>,f,f,plain> v6 = ref_var(type_lit(int<4>));\n"
			"    {\n"
			"        v0 = 7;\n"
			"        fun( *v1);\n"
			"        rfun(v2);\n"
			"        fun(fun( *v3));\n"
			"        fun(rfun(v4));\n"
			"        lfun(v5);\n"
			"        rfun(ref_var_init(lfun(v6)));\n"
			"    };\n"
			"}";

	EXPECT_EQ(res2, toString(PrettyPrinter(stmt, PrettyPrinter::JUST_LOCAL_CONTEXT))) << toString(PrettyPrinter(stmt, PrettyPrinter::JUST_LOCAL_CONTEXT));
}
