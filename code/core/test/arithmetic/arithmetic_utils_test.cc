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

#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/parser/ir_parse.h"
namespace insieme {
namespace core {
namespace arithmetic {

using namespace insieme::core::checks;


TEST(ArithmeticUtilsTest, fromIR) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	TypePtr type = builder.getLangBasic().getInt4();

	LiteralPtr zero = builder.intLit(0);
	LiteralPtr one = builder.intLit(1);
	LiteralPtr two = builder.intLit(2);
	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);
	VariablePtr varC = builder.variable(type, 3);

	// test constants
	EXPECT_EQ("0", toString(toFormula(zero)));
	EXPECT_EQ("1", toString(toFormula(one)));
	EXPECT_EQ("2", toString(toFormula(two)));

	// test for zero
	EXPECT_TRUE(toFormula(zero).isZero());
	EXPECT_TRUE(toFormula(one).isOne());

	EXPECT_FALSE(toFormula(zero).isOne());
	EXPECT_FALSE(toFormula(one).isZero());


	// test variables
	EXPECT_EQ("v1", toString(toFormula(varA)));

	// test incorrect expression
	EXPECT_THROW(toFormula(builder.stringLit("hello")), NotAFormulaException);

	// test add operations
	ExpressionPtr tmp;
	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Add), varA, one);
	EXPECT_EQ("v1+1", toString(toFormula(tmp)));

	// also test multiplication
	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Mul), varB, tmp);
	EXPECT_EQ("v1*v2+v2", toString(toFormula(tmp)));

	// and subtraction
	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Sub), tmp, varC);
	EXPECT_EQ("v1*v2+v2-v3", toString(toFormula(tmp)));

}

bool empty(const MessageList& list) {
	return list.empty();
}

TEST(ArithmeticTest, toIR) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr type = builder.getLangBasic().getInt4();

	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);
	VariablePtr varC = builder.variable(type, 3);

	auto all = checks::getFullCheck();

	Formula f = varA + varB;
	EXPECT_EQ("v1+v2", toString(f));
	EXPECT_EQ("int.add(v1, v2)", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));

	f = 2*varA + varB;
	EXPECT_EQ("2*v1+v2", toString(f));
	EXPECT_EQ("int.add(int.mul(2, v1), v2)", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));

	// test varA^2
	f = 2*varA*varA + varB*varA;
	EXPECT_EQ("2*v1^2+v1*v2", toString(f));
	EXPECT_EQ("int.add(int.mul(2, int.mul(v1, v1)), int.mul(v1, v2))", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));

	Product one;
	f = one / varA;
	EXPECT_EQ("v1^-1", toString(f));
	EXPECT_EQ("int.div(1, v1)", toString(*toIR(manager, f)));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));
	EXPECT_THROW(toFormula(toIR(manager, f)), NotAFormulaException);		// not convertible since integer division not supported

	f = one / (varA*varA*varA);
	EXPECT_EQ("v1^-3", toString(f));
	EXPECT_EQ("int.div(1, int.mul(int.mul(v1, v1), v1))", toString(*toIR(manager, f)));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));
	EXPECT_THROW(toFormula(toIR(manager, f)), NotAFormulaException);		// not convertible since integer division not supported

	f = varA + varA*varA - varB*varB - one/(varA*varB) + varB - varC;
	EXPECT_EQ("v1^2+v1-v1^-1*v2^-1-v2^2+v2-v3", toString(f));
	EXPECT_EQ("int.sub(int.add(int.sub(int.sub(int.add(int.mul(v1, v1), v1), int.mul(int.div(1, v1), int.div(1, v2))), int.mul(v2, v2)), v2), v3)", toString(*toIR(manager, f)));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));
	EXPECT_THROW(toFormula(toIR(manager, f)), NotAFormulaException);		// not convertible since integer division not supported
}

TEST(ArithmeticTest, nonVariableValues) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	TypePtr type = builder.getLangBasic().getInt4();

	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);

	vector<NamedTypePtr> entries;
	entries.push_back(core::NamedType::get(manager, builder.stringValue("a"), type));
	entries.push_back(core::NamedType::get(manager, builder.stringValue("b"), type));
	TypePtr type2 = builder.structType(entries);

	VariablePtr varX = builder.variable(type2, 3);
	ExpressionPtr xa = builder.accessMember(varX, "a");
	ExpressionPtr xb = builder.accessMember(varX, "b");

	auto all = checks::getFullCheck();

	EXPECT_EQ("v3", toString(*varX));
	EXPECT_EQ("v3.a", toString(printer::PrettyPrinter(xa, printer::PrettyPrinter::NO_LET_BINDINGS)));
	EXPECT_EQ("v3.b", toString(printer::PrettyPrinter(xb, printer::PrettyPrinter::NO_LET_BINDINGS)));

	// -- build formulas using subscripts --

	ExpressionPtr tmp;
	Formula f;

	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Add), varA, xa);

	f= toFormula(tmp);
	EXPECT_EQ("v1+v3.a", toString(f));
	EXPECT_EQ("int.add(v1, composite.member.access(v3, a, int<4>))", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));
	EXPECT_PRED1(empty, check(toIR(manager,f), all));


}

TEST (ArithmeticTest, fromIRExpr) {
	NodeManager mgr;
	parse::IRParser parser(mgr);
	// from expr: int.add(int.add(int.mul(int.mul(0, 4), 4), int.mul(0, 4)), v112)
	auto expr = parser.parseExpression("((((0 * 4) * 4) + (0 * 4)) + int<4>:v1)");

	EXPECT_EQ("int.add(int.add(int.mul(int.mul(0, 4), 4), int.mul(0, 4)), v1)", toString(*expr));

	auto f = toFormula(expr);
	EXPECT_EQ("v1", toString(f));

	// add some devision
	expr = parser.parseExpression("((1 * 4)/2)");
	EXPECT_EQ("int.div(int.mul(1, 4), 2)", toString(*expr));

	f = toFormula(expr);
	EXPECT_EQ("2", toString(f));


	// some more complex stuff
	mgr.setNextFreshID(2);
	expr = parser.parseExpression("(((15/2) * int<4>:x) / ((20/6) * int<4>:x))");
	EXPECT_EQ("int.div(int.mul(int.div(15, 2), v2), int.mul(int.div(20, 6), v2))", toString(*expr));

	f = toFormula(expr);
	EXPECT_EQ("2", toString(f));


	// test support of division by 1
	expr = parser.parseExpression("((12 * int<4>:x) / 1)");
	EXPECT_EQ("int.div(int.mul(12, v3), 1)", toString(*expr));
	f = toFormula(expr);
	EXPECT_EQ("12*v3", toString(f));

	// test support for division by -1
	expr = parser.parseExpression("((4 * int<4>:x) / -1)");
	EXPECT_EQ("int.div(int.mul(4, v4), -1)", toString(*expr));
	f = toFormula(expr);
	EXPECT_EQ("-4*v4", toString(f));
}


TEST(ConstraintTest, fromAndToIR) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto var = builder.variable(mgr.getLangBasic().getInt4(), 0);

	auto f = Formula(var);
	EXPECT_EQ("v0", toString(f));

	auto one = Formula(1);
	auto two = Formula(2);

	// some simple formula
	Constraint c = f > one && f < two;
	EXPECT_EQ("(!(v0-1 <= 0) and !(-v0+2 <= 0))", toString(c));

	EXPECT_EQ("AP(bool.and(bool.not(int.le(int.sub(v0, 1), 0)), bind(){rec v5.{v5=fun(int<4> v0) {return bool.not(int.le(int.add(int.mul(-1, v0), 2), 0));}}(v0)}))", toString(toIR(mgr, c)));
	EXPECT_EQ(c, toConstraint(toIR(mgr, c)));

	// some valid formula
	c = f > one || f <= one;
	EXPECT_EQ("true", toString(c));

	EXPECT_EQ("AP(true)", toString(toIR(mgr, c)));
	EXPECT_EQ(c, toConstraint(toIR(mgr, c)));

}



TEST(ArithmeticTest, FormulaValueExtraction) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );

	Formula f = Formula(2) + v1 + v2*5 - (Product(v1)^2); 
	
	// extract the variables on this formula
	ValueSet&& vl = f.extractValues();
	EXPECT_EQ(2u, vl.size());
}

TEST(ArithmeticTest, ConstraintValueExtraction) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );

	Constraint c = eq(Formula(2) + v1 + v2*5 - (Product(v1)^2), 0);
	
	// extract the variables on this formula
	ValueSet&& vl = c.extractValues();
	EXPECT_EQ(2u, vl.size());
}

TEST(ArithmeticTest, ConstraintPtrValueExtraction) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );

	Constraint c1 = eq(Formula(2) + v1 + (Product(v1)^2), 0);
	Constraint c2 = (Formula(3) + (v2^3) < 0);

	Constraint c = c1 or !c2;
	
	// extract the variables on this formula
	ValueSet&& vl = c.extractValues();
	EXPECT_EQ(2u, vl.size());
}

TEST(ArithmeticTest, PiecewiseValueExtraction) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	using utils::ConstraintType;
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v3 = builder.variable( mgr.getLangBasic().getInt4() );

	Piecewise pw;

	pw = pw + Piecewise( v1 + (v1^2) >= 0, 3+4-v1 );
	pw = pw + Piecewise( v1 + (v1^2) < 0 and ne((v2^2),0), 3+v3-v1 );

	// extract the variables on this formula
	ValueSet&& vl = pw.extractValues();
	EXPECT_EQ(3u, vl.size());
}

TEST(ArithmeticTest, Replacement) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );

	Formula f = 2 + v1 + v2*5 - (v1^2); 
	EXPECT_EQ("-v1^2+v1+5*v2+2", toString(f));

	ValueReplacementMap vrm;
	vrm[v1] = v2^3;

	f = f.replace(vrm);
	EXPECT_EQ("-v2^6+v2^3+5*v2+2", toString(f));

	ValueSet&& vl = f.extractValues();
	EXPECT_EQ(1u, vl.size());
	EXPECT_EQ(Value(v2), *vl.begin());

	f = -2 - v1 + (v1^2); 
	vrm[v1] = Formula(1)/2;

	f = f.replace(vrm);
	EXPECT_EQ("-9/4", toString(f));

	EXPECT_TRUE(f.isConstant());
	int64_t val = f.getConstantValue();

	EXPECT_EQ(-2, val);
}

TEST(ArithmeticTest, InequalityReplacement) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );

	Formula f = 2 + v1 + v2*5 - (v1^2); 
	Inequality c(f);
	EXPECT_EQ("-v1^2+v1+5*v2+2 <= 0", toString(c));

	ValueReplacementMap vrm;
	vrm[v1] = 3;

	Inequality c2 = c.replace(vrm);
	EXPECT_EQ("5*v2-4 <= 0", toString(c2));

	vrm[v2] = 2;
	Inequality c3 = c.replace(vrm);
	EXPECT_EQ("1 <= 0", toString(c3));

	EXPECT_TRUE(c3.isConstant());
	EXPECT_TRUE(c3.isUnsatisfiable());
}

TEST(ArithmeticTest, ConstraintReplacement) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );

	Formula f = -2 - v1 + (v1^2); 
	Constraint c1 = (f <= 0);
	EXPECT_EQ("(v1^2-v1-2 <= 0)", toString(c1));

	Constraint c2 = eq(v1+(v2^3), 0);
	EXPECT_EQ("(v1+v2^3 <= 0 and -v1-v2^3 <= 0)", toString(c2));

	Constraint comb = c1 and c2;
	EXPECT_EQ("(v1^2-v1-2 <= 0 and v1+v2^3 <= 0 and -v1-v2^3 <= 0)", toString(comb));

	{
		ValueReplacementMap vrm;
		vrm[v1] = 3;
		Constraint comb2 = comb.replace(vrm);
		EXPECT_EQ( "false", toString(comb2) );
		EXPECT_TRUE(comb2.isConstant());
	}
	{
		ValueReplacementMap vrm;
		vrm[v1] = 1;
		Constraint comb2 = comb.replace(vrm);
		EXPECT_EQ( "(v2^3+1 <= 0 and -v2^3-1 <= 0)", toString(comb2) );
		EXPECT_FALSE(comb2.isConstant());
	}
}	

TEST(ArithmeticTest, PiecewiseValueReplacement) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	using utils::ConstraintType;
	
	VariablePtr v1 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v2 = builder.variable( mgr.getLangBasic().getInt4() );
	VariablePtr v3 = builder.variable( mgr.getLangBasic().getInt4() );

	Piecewise pw;
	pw = pw + Piecewise( v1 + (v1^2) >= 0, 3+4-v1 );
	pw = pw + Piecewise( v1 + (v1^2) < 0 and ne((v2^2),0), 3+v3-v1);

//	std::cout << pw << std::endl;

	ValueReplacementMap vrm;
	vrm[v1] = 3;
	vrm[v2] = 2;
	
	pw = pw.replace(vrm);
	
//	std::cout << pw << std::endl;

	EXPECT_TRUE(pw.isFormula());
	EXPECT_EQ(pw.toFormula(), 4);
}

TEST(ArithmeticTest, CastBug_001) {
	// The IR expression
	// 		int.sub(v3, cast<uint<4>>(10))
	// cannot be converted into a formula.
	//
	// Reason:
	// Fix:
	//

	NodeManager mgr;
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	// reconstruct expression
	VariablePtr v1 = builder.variable( basic.getInt4() );
	ExpressionPtr ten = builder.castExpr(basic.getUInt4(), builder.intLit(10));
//	ExpressionPtr expr = builder.callExpr(basic.getInt4(), basic.getSignedIntSub(), v1, ten);
	ExpressionPtr expr = builder.sub(v1, ten);

	EXPECT_EQ("int.sub(v1, cast<uint<4>>(10))", toString(*expr));

	// convert to formula
	Formula f = toFormula(expr);
}


TEST(ArithmeticTest, PiecewiseToIRAndBack) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	using utils::ConstraintType;

	Formula v1 = Formula(builder.variable( mgr.getLangBasic().getInt4()));
	Formula v2 = Formula(builder.variable( mgr.getLangBasic().getInt4()));

	auto all = checks::getFullCheck();

	Piecewise pw;
	EXPECT_EQ("0 -> if true", toString(pw));
	EXPECT_EQ("0", toString(*toIR(mgr, pw)));
	EXPECT_PRED1(empty, check(toIR(mgr,pw), all));

	pw += Piecewise( v1 + v2 <= 0, 3+4-v1 );
	EXPECT_EQ("-v1+7 -> if (v1+v2 <= 0); 0 -> if (!(v1+v2 <= 0))", toString(pw));
	EXPECT_EQ("rec v0.{v0=fun(bool v1, (()=>'b) v2, (()=>'b) v3) {if(v1) {return v2();} else {return v3();};}}(int.le(int.add(v1, v2), 0), bind(){rec v8.{v8=fun(int<4> v1) {return int.add(int.mul(-1, v1), 7);}}(v1)}, rec v7.{v7=fun() {return 0;}})", toString(*toIR(mgr, pw)));
	EXPECT_PRED1(empty, check(toIR(mgr,pw), all));

	pw += Piecewise( v2 <= 0, v2);
	EXPECT_PRED1(empty, check(toIR(mgr,pw), all));
	// apply type checker

}

TEST(ArithmeticTest, SelectToFormula) {

	// something like select(1,2,int.lt) should be convertible to a formula

	NodeManager mgr;
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	auto a = builder.intLit(1);
	auto b = builder.intLit(1);

	// check whether it is convertible
	EXPECT_EQ(toFormula(a), toFormula(builder.select(a, b, basic.getSignedIntLt())));

	// the following should not be convertible
	auto v = builder.variable(basic.getInt4(),1);
	EXPECT_THROW(toFormula(builder.select(a,v,basic.getSignedIntLt())), NotAFormulaException);
}

TEST(ArithmeticTest, NestedSelectToPiecewise) {

	// something like select(select(1,2,int.lt),select(2,1,int.lt),int.lt) should be convertible to a piecewise

	NodeManager mgr;
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	auto a = builder.intLit(1);
	auto b = builder.intLit(2);
	auto c = builder.intLit(3);
	auto lt = basic.getSignedIntLt();

	// check whether it is convertible
	EXPECT_EQ(toPiecewise(a), toPiecewise(builder.select(builder.select(a,b,lt), c, lt)));

	// the following should be also convertable
	auto v1 = builder.variable(basic.getInt4(),1);
	auto v2 = builder.variable(basic.getInt4(),2);
	auto v3 = builder.variable(basic.getInt4(),3);

	EXPECT_EQ("v1 -> if (!(-v1+v2 <= 0) and !(-v1+v3 <= 0)); v3 -> if (!(-v1+v2 <= 0) and -v1+v3 <= 0) or (-v1+v2 <= 0 and -v2+v3 <= 0); v2 -> if (-v1+v2 <= 0 and !(-v2+v3 <= 0))", toString(toPiecewise(builder.select(builder.select(v1,v2,lt),v3,lt))));
	EXPECT_EQ("v1 -> if (!(-v2+v3 <= 0) and !(-v1+v2 <= 0)) or (-v2+v3 <= 0 and !(-v1+v3 <= 0)); v2 -> if (!(-v2+v3 <= 0) and -v1+v2 <= 0); v3 -> if (-v2+v3 <= 0 and -v1+v3 <= 0)", toString(toPiecewise(builder.select(v1,builder.select(v2,v3,lt),lt))));
}


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

