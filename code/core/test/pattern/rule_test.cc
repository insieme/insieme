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

#include "insieme/core/pattern/rule.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/variable.h"

namespace insieme {
namespace core {
namespace pattern {

namespace p = pattern;
namespace g = pattern::generator;

using namespace generator;

TEST(Rule, Identity) {
	Rule rule;

	EXPECT_EQ("_ -> root", toString(rule));

	TreePtr a = makeTree('a');
	TreePtr b = makeTree('b');
	TreePtr c = makeTree(a, b, a, b);

	EXPECT_EQ(a, rule.applyTo(a));
	EXPECT_EQ(b, rule.applyTo(b));
	EXPECT_EQ(c, rule.applyTo(c));
}


TEST(Rule, Replace) {
	TreePattern pattern;
	TreeGenerator generator;
	Rule rule;

	TreePtr a = makeTree('a');
	TreePtr b = makeTree('b');
	TreePtr c = makeTree('c');


	rule = Rule(p::any, g::atom(a));
	EXPECT_EQ("_ -> a", toString(rule));

	EXPECT_EQ(a, rule.applyTo(a));
	EXPECT_EQ(a, rule.applyTo(b));
	EXPECT_EQ(a, rule.applyTo(c));
}

TEST(Rule, Reorder) {
	TreePtr a = makeTree('a');
	TreePtr b = makeTree('b');

	TreePattern pattern;
	TreeGenerator generator;

	pattern = p::node(0, p::single(p::var("x")) << p::single(p::var("y")));
	generator = g::node(0, g::single(g::var<tree_target>("y")) << g::single(g::var<tree_target>("x")));

	EXPECT_EQ("(0|$x,$y)", toString(pattern));
	EXPECT_EQ("(0|$y,$x)", toString(generator));

	Rule rule(pattern, generator);

	TreePtr in = makeTree(a, b);
	TreePtr out = rule.applyTo(in);

	EXPECT_EQ("(a,b)", toString(in));
	EXPECT_EQ("(b,a)", toString(out));

	// something that shouldn't work ...
	in = makeTree(a, b, a);
	EXPECT_FALSE((bool)rule.applyTo(in));
}


TEST(Rule, Reverse) {
	TreePtr a = makeTree('a');
	TreePtr b = makeTree('b');
	TreePtr c = makeTree('c');

	TreePattern pattern;
	TreeGenerator generator;

	pattern = p::node(0, *(p::var("x")));
	generator = g::node(0, g::forEach("y", g::reverse(g::varExpr<tree_target>("x")), g::var<tree_target>("y")));

	EXPECT_EQ("(0|[$x]*)", toString(pattern));
	EXPECT_EQ("(0|for y in reverse($x) get $y)", toString(generator));

	Rule rule(pattern, generator);

	TreePtr in = makeTree(a, b);
	TreePtr out = rule.applyTo(in);

	EXPECT_EQ("(a,b)", toString(in));
	EXPECT_EQ("(b,a)", toString(out));

	EXPECT_EQ("(c,b,a)", toString(rule.applyTo(makeTree(a, b, c))));

	EXPECT_EQ("(a,c,b,a)", toString(rule.applyTo(makeTree(a, b, c, a))));
}


TEST(Rule, OpReplacement) {
	using namespace generator;

	core::NodeManager mgr;
	IRBuilder builder(mgr);

	core::NodePtr code = builder.normalize(builder.parseExpr("1+2"));
	ASSERT_EQ("int_add(1, 2)", toString(*code));

	// create a set of tree variables
	Variable typeF = "typeF";
	Variable typeR = "typeR";
	Variable arg1 = "arg1";
	Variable arg2 = "arg2";

	auto pattern = irp::callExpr(typeR, irp::literal(typeF, "int_add"), arg1 << arg2);
	auto generator = irg::callExpr(typeR, irg::literal(typeF, "int_sub"), arg1 << arg2);

	// check that the pattern is right
	ASSERT_TRUE(pattern.matchPointer(code));

	// create and apply a rule
	Rule rule(pattern, generator);
	auto out = rule(code);
	ASSERT_TRUE(out);
	EXPECT_EQ("int_sub(1, 2)", toString(*out));
}


TEST(Rule, MultiplyAndAdd) {
	using namespace generator;

	core::NodeManager mgr;
	IRBuilder builder(mgr);

	auto mul = builder.getLangBasic().getSignedIntMul();
	auto add = builder.getLangBasic().getSignedIntAdd();
	auto mad = builder.parseExpr("lit(\"mad\" : (int<'a>,int<'a>,int<'a>) -> int<'a>)");

	Variable t = "t";
	Variable argt = "argt";
	Variable a = "a";
	Variable b = "b";
	Variable c = "c";

	auto argp = [&](const p::TreePattern& arg) { return irp::declaration(argt,arg); };
	auto argg = [&](const p::TreeGenerator& arg) { return irg::declaration(argt,arg); };

	auto p = irp::callExpr(t, add, argp(irp::callExpr(mul, argp(a) << argp(b))) << argp(c));
	auto g = irg::callExpr(t, mad, argg(a) << argg(b) << argg(c));

	auto c1 = builder.parseExpr("(1*2)+3");
	ASSERT_TRUE(p.matchPointer(c1));

	Rule rule(p, g);
	EXPECT_EQ("mad(1, 2, 3)", toString(*rule(c1)));

	// now nested
	Variable trg("i", p);
	auto p2 = aT(trg);
	auto g2 = g::substitute(g::root, trg, g);

	auto c2 = builder.normalize(builder.parseStmt(
			"{"
			"	var int<4> a = 12;"
			"	var int<4> b = 14;"
			"	1+2;"
			"	1*2+3;"
			"	12 * (2*3+1) + (a*b+3);"
			"	var int<4> c = a * b + 123;"
			"}"
	));

	ASSERT_TRUE(p2.matchPointer(c2));

	Rule r2(p2, g2);
	NodePtr res = r2.fixpoint(c2);

	EXPECT_EQ("{int<4> v0 = 12; int<4> v1 = 14; int_add(1, 2); mad(1, 2, 3); mad(12, mad(2, 3, 1), mad(v0, v1, 3)); int<4> v2 = mad(v0, v1, 123);}",
	          toString(*res));
}


TEST(Rule, EmptyCompoundElemination) {
	ListVariable xs = "x";
	ListVariable ys = "y";

	auto r = Rule(irp::compoundStmt(xs << irp::compoundStmt() << ys), irg::compoundStmt(xs << ys));

	NodeManager mgr;
	IRBuilder builder(mgr);

	auto a = builder.intLit(1);

	auto c = builder.compoundStmt(a, builder.compoundStmt(), a, builder.compoundStmt(builder.compoundStmt()),
	                              builder.compoundStmt(builder.compoundStmt(builder.compoundStmt())), builder.compoundStmt(builder.compoundStmt(), a),
	                              builder.compoundStmt(), a);

	EXPECT_EQ("{1; {}; 1; {{};}; {{{};};}; {{}; 1;}; {}; 1;}", toString(*c));
	EXPECT_EQ("{1; 1; {{};}; {{{};};}; {{}; 1;}; 1;}", toString(*r.fixpoint(c)));
	EXPECT_EQ("{1; 1; {1;}; 1;}", toString(*r.fixpointNested(c)));
}


TEST(Rule, VarDeref) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	const auto& ext = mgr.getLangExtension<lang::ReferenceExtension>();

	Variable x;

	auto argp = [&](const p::TreePattern& arg) { return irp::declaration(p::any,arg); };

	auto r = Rule(irp::callExpr(ext.getRefDeref(), argp(irp::callExpr((irp::atom(ext.getRefTempInit()) | irp::atom(ext.getRefNewInit())), argp(x)))), x);

	auto a = builder.intLit(1);
	auto b = builder.deref(builder.refTemp(a));
	auto c = builder.deref(builder.refNew(a));

	EXPECT_EQ("ref_deref(rec ref_temp_init.{ref_temp_init=fun(ref<'a,f,f,plain> v0) {ref<'a,f,f,plain> v1 = rec ref_temp.{ref_temp=fun(ref<type<'a>,f,f,plain> v0) "
			  "{return ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<'a>); ref_assign(v1, ref_deref(v0)); return v1;}}(1))",
			  toString(*b));
	EXPECT_EQ("ref_deref(rec ref_new_init.{ref_new_init=fun(ref<'a,f,f,plain> v0) {ref<'a,f,f,plain> v1 = rec ref_new.{ref_new=fun(ref<type<'a>,f,f,plain> v0) "
			  "{return ref_alloc(ref_deref(v0), mem_loc_heap);}}(type<'a>); ref_assign(v1, ref_deref(v0)); return v1;}}(1))",
			  toString(*c));
	EXPECT_EQ("1", toString(*r.fixpoint(b)));
	EXPECT_EQ("1", toString(*r.fixpoint(c)));
	EXPECT_EQ("1", toString(*r.fixpointNested(b)));
	EXPECT_EQ("1", toString(*r.fixpointNested(c)));
}

} // end namespace pattern
} // end namespace core
} // end namespace insieme
