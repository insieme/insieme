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

#include <gtest/gtest.h>

#include "insieme/analysis/datalog/code_properties.h"

#include "insieme/core/ir_builder.h"

#include "insieme/driver/integration/tests.h"


namespace insieme {
namespace analysis {
namespace datalog {

	using namespace core;

	TEST(CodeProperties, TMP) {
		core::NodeManager mgr;
		IRBuilder builder(mgr);

		auto t = builder.parseExpr(
				"() -> int<4> { "
				"	() -> bool {"
				"		return false; "
				"	};"
				"	return 1; "
				"	return 2; "
				"	() -> int<4> {"
				"		return 1; "
				"	};"
				"}"
		);

		visitDepthFirstOnce(t, [](const core::LambdaPtr& lambda) {
			std::cout << "Running for " << dumpColor(lambda) << "\n";
			auto list = performExitPointAnalysis(lambda);
			std::cout << " \t" << list << "\n";
		});
	}

	TEST(CodeProperties, LargerCode) {
		using namespace driver::integration;

		core::NodeManager mgr;
		IRBuilder builder(mgr);

		for (auto name : {"loop_transform", "pyramids", "pendulum", "mqap", "loops", "transpose"}) {
			IntegrationTestCaseOpt testCase = getCase(name);
			core::ProgramPtr code = testCase.get().load(mgr);
			EXPECT_TRUE(code);
			EXPECT_TRUE(getTopLevelNodes(code));
		}
	}

	TEST(CodeProperties, IsPolymorph) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_FALSE(isPolymorph(builder.parseType("bool")));
		EXPECT_FALSE(isPolymorph(builder.parseType("char")));
		EXPECT_FALSE(isPolymorph(builder.parseType("int")));
		EXPECT_FALSE(isPolymorph(builder.parseType("uint")));
		EXPECT_FALSE(isPolymorph(builder.parseType("string")));

		EXPECT_TRUE(isPolymorph(builder.parseType("'a")));

		EXPECT_FALSE(isPolymorph(builder.parseType("(bool)")));
		EXPECT_FALSE(isPolymorph(builder.parseType("(bool,int<4>)")));

		EXPECT_TRUE(isPolymorph(builder.parseType("('a)")));
		EXPECT_TRUE(isPolymorph(builder.parseType("('a,bool)")));

		EXPECT_FALSE(isPolymorph(builder.parseType("int<4>")));
		EXPECT_FALSE(isPolymorph(builder.parseType("ref<int<4>>")));
		EXPECT_TRUE(isPolymorph(builder.parseType("array<'a,'b>")));
		EXPECT_FALSE(isPolymorph(builder.parseType("(int<4>)->bool")));
		EXPECT_FALSE(isPolymorph(builder.parseType("(string, int<4>)->uint<4>")));

	}

	TEST(CodeProperties, TopLevelTerm) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("('a)")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{var int<4> a = 1; if (a > 0) { a+1; }}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("int<4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<int<4>>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct { x : int<4>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct B; def struct B { x : int<4>; }; B")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct B; def struct B { x : ref<B>; }; B")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct data; def struct data { x : ptr<data>; }; data")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct A; decl struct B; def struct A { x : ref<B>; }; def struct B { x : ref<A>; }; B")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct A; decl struct B; def struct A { x : ptr<B>; }; def struct B { x : ptr<A>; }; A")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct A; decl struct B; def struct A { x : ref<A>; }; def struct B { y : ref<A>; }; B")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("def struct A { lambda retA = () -> A { return *this; } }; A")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ while(true) { continue; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ while(true) { break; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ for(int<4> i = 1 .. 10 ) { continue; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("decl f : (int<4>)->int<4>;"
			                         "def f = (x : int<4>)->int<4> {"
			                         "	return (x==0)?1:f(x-1)*x;"
			                         "}; f")));

	}

	TEST(CodeProperties, Types) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("int<4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("someweirdname<>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("'a")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("'a...")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("'a<>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("'a...<>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("vector<int<4>, 4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("vector<'a, 4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<'a,f,f,plain>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<'a,f,t,plain>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<'a,t,f,plain>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<'a,t,t,plain>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<'a,f,f,cpp_ref>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<'a,f,f,cpp_rref>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ptr<'a>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ptr<'a,f,f>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ptr<'a,t,f>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ptr<'a,f,t>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ptr<'a,t,t>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct { a : int<4>; b : int<5>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct name { a : int<4>; b : int<5>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let papa = t<11> in struct name : [papa] { a : int<4>; b : int<5>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let papa = t<11> in struct name : [private papa] { a : int<4>; b : int<5>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let papa = t<11> in struct name : [protected papa] { a : int<4>; b : int<5>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let papa = t<11> in struct name : [public papa] { a : int<4>; b : int<5>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let int = int<4> in int")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("rf<int<4>>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("() -> unit")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("(int<4>) -> int<4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("( int<4> , rf<int<4>>) -> int<4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("( int<4> , rf<int<4>>) => int<4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("(ary<'elem,'n>, vector<uint<8>,'n>) -> 'elem")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let class = struct name { a : int<4>; b : int<5>; } in "
		                          "class::()->int<4> ")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let class = struct name { a : int<4>; b : int<5>; } in "
		                          "class::()~>int<4> ")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let class = struct name { a : int<4>; b : int<5>; } in "
		                          "~class::()")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("let class = struct name { a : int<4>; b : int<5>; } in "
		                          "class::()")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct C { field : int<4>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("(rf<ary<rf<ary<struct{int : int<4>; float : real<4>; },1>>,1>>,"
		                          " rf<ary<rf<ary<real<4>,1>>,1>>,"
		                          " rf<ary<uint<8>,1>>)")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  b : int<4>;"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "  ctor (a : int<4>) { }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  dtor () { }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  dtor virtual () { }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {" //multiple functions with the same name
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  lambda f = (a : int<4>) -> int<4> { return 1; }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  lambda g = (b : int<4>) -> int<4> { return b; }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  const lambda b = () -> int<4> { return 1; }"
		                          "  volatile lambda c = () -> int<4> { return 1; }"
		                          "  volatile const lambda d = (a : int<4>) -> int<4> { return 1; }"
		                          "  const volatile lambda e = (a : int<4>) -> int<4> { return 1; }"
		                          "  virtual const lambda f = () -> int<4> { return 1; }"
		                          "  virtual volatile lambda g = () -> int<4> { return 1; }"
		                          "  virtual volatile const lambda h = (a : int<4>) -> int<4> { return 1; }"
		                          "  virtual const volatile lambda i = (a : int<4>) -> int<4> { return 1; }"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  pure virtual b : () -> int<4>"
		                          "  pure virtual const c : () -> int<4>"
		                          "  pure virtual volatile d : () -> int<4>"
		                          "  pure virtual volatile const e : (int<4>) -> int<4>"
		                          "  pure virtual const volatile f : (int<4>) -> int<4>"
		                          "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "  ctor (a : int<4>) { }"
		                          "  dtor () { }"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  virtual const volatile lambda g = () -> int<4> { return 1; }"
		                          "  pure virtual h : () -> int<4>"
		                          "}")));
	}

	TEST(CodeProperties, Expressions) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("true?1:0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("type_lit(1)")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1u")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1l")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1ul")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1ll")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1ull")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1.0f")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1.0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("2.3E-5f")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("2.0E+0")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 + 3")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 - 0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 * 0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 / 0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 | 0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 & 0")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 ^ 0")));

		// precedence
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 + 0 * 5")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 * 0 + 5")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(1.0)")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("((1.0))")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("((1.0) + 4.0)")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("1 + 2 * 3")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(1 + 2) * 3")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("def struct x { a : int<4>; b : int<4>; }; <ref<x>> { 4, 5 }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("def struct x { a : int<4>; }; <ref<x>> { 4 }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("def struct x { }; <ref<x>> { }")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("def union uni { a : int<4>; lambda f = ()->unit {} }; uni")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("def union uni { a : int<4>; lambda f = ()->unit {} }; { var ref<uni,f,f,plain> a; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("def union uni { a : int<4>; lambda f = ()->unit {} }; { <ref<uni>> { 4 }; }")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(_ : 'a) -> bool { return true; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(x : 'a) -> 'a { return x+CAST('a) 3; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(x : 'a) => x+CAST('a) 3")));

		// return type deduction
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("() => { return true; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(x : bool) => { if(x) { return true; } else { return false; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(x : bool) => { if(x) { return 1; } else { return -5; } }")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("type_lit(int<4>)")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("decl f : ()->unit;"
		                                "let f = ()->unit { 5; ()->unit { f(); } (); } in f")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(v : int<4>, exp : int<4>) -> int<4> { "
		                                "	let one = (_ : int<4>)=>4; "
		                                "	let two = (x : int<4>)=>x+exp; "
		                                "    return one(two(exp));"
		                                "}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("(v : int<4>, exp : int<4>) -> int<4> { "
		                                "	let one = (_ : int<4>)-> int<4> { return 4;  };"
		                                "	let two = (x : int<4>)-> int<4> { return x+5; };"
		                                "    return one(two(exp));"
		                                "}")));
	}

	TEST(CodeProperties, Statements) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1u;")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1.0f;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1.0;")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 + 3;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 - 0;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 * 0;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 / 0;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 | 0;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 & 0;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("1 ^ 0;")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("(1.0);")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("((1.0));")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ var int<4> x = 0; x+1; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ var ref<int<4>,f,f,plain> x; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ auto x = 0; x+1; }")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if ( true ) {}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if ( true ) {} else {}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if ( true ) { if ( false ) { } else { } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if ( true ) { if ( false ) { } else { } } else { } ")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if( false ) { return 0; } else { return 1+2; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if( false ) { return 0; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("if(1 != 0) { return 0; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("while ( true ) { 1+1; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("while ( false ) { 1+1; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("while ( false || true ) { 1+1; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("for ( int<4> it = 1 .. 3) { 1+1; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("for ( int<4> it = 1 .. 3: 2) { 1+1; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("for ( int<4> it = 1 .. 3: 2) { 1+1; }")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("switch (2) { case 1: { 1; } case 2: { 2; } }")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ }")));


		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("alias type = struct a { a : int<4>; b : int<8>; };"
		                               "{"
		                               "    var ref<type,f,f,plain> variable;"
		                               "    var ref<rf<type>,f,f,plain> var2;"
		                               "    auto var3 = var2;"
		                               "}")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("alias class = struct name { a : int<2>; };"
		                               "alias collection = array<class, 10>;"
		                               "{"
		                               "    var ref<collection,f,f,plain> x;"
		                               "    var int<2> y = CAST(int<2>) 5;"
		                               "    x[5].a = y;"
		                               "}")));

		// return statements may also declare a variable which can be used in the return expression itself
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("def struct A { a : int<4>; };"
		                               "def foo = () -> A {"
		                               "  return var ref<A> v0 = A::(v0);"
		                               "};"
		                               "foo();")));
	}

	TEST(CodeProperties, TryCatch) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("<ref<int<4>>> { 3 }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("throw true;")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("try {} catch (v1 : bool) {v1;} catch (v2 : int<4>) {v2;}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseProgram("int main() { return 0; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("spawn 14")));
	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

