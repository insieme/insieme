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
 *
 */
#include <gtest/gtest.h>

#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/printer/pretty_printer.h"

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

	TEST(ArithmeticUtilsTest, extendedLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt16();

		LiteralPtr ull = builder.literal("100ull", type);
		LiteralPtr ll = builder.literal("100ll", type);
		LiteralPtr l = builder.literal("100l", type);

		// test constants
		EXPECT_EQ("100", toString(toFormula(ull)));
		EXPECT_EQ("100", toString(toFormula(ll)));
		EXPECT_EQ("100", toString(toFormula(l)));
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
		EXPECT_EQ("int_add(v1, v2)", toString(*toIR(manager, f)));
		EXPECT_EQ(f, toFormula(toIR(manager, f)));
		EXPECT_EQ(toIR(manager, f), toIR(manager, toFormula(toIR(manager, f))));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));

		f = 2 * varA + varB;
		EXPECT_EQ("2*v1+v2", toString(f));
		EXPECT_EQ("int_add(int_mul(2, v1), v2)", toString(*toIR(manager, f)));
		EXPECT_EQ(f, toFormula(toIR(manager, f)));
		EXPECT_EQ(toIR(manager, f), toIR(manager, toFormula(toIR(manager, f))));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));

		// test varA^2
		f = 2 * varA * varA + varB * varA;
		EXPECT_EQ("2*v1^2+v1*v2", toString(f));
		EXPECT_EQ("int_add(int_mul(2, int_mul(v1, v1)), int_mul(v1, v2))", toString(*toIR(manager, f)));
		EXPECT_EQ(f, toFormula(toIR(manager, f)));
		EXPECT_EQ(toIR(manager, f), toIR(manager, toFormula(toIR(manager, f))));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));

		Product one;
		f = one / varA;
		EXPECT_EQ("v1^-1", toString(f));
		EXPECT_EQ("int_div(1, v1)", toString(*toIR(manager, f)));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));
		EXPECT_THROW(toFormula(toIR(manager, f)), NotAFormulaException); // not convertible since integer division not supported

		f = one / (varA * varA * varA);
		EXPECT_EQ("v1^-3", toString(f));
		EXPECT_EQ("int_div(1, int_mul(int_mul(v1, v1), v1))", toString(*toIR(manager, f)));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));
		EXPECT_THROW(toFormula(toIR(manager, f)), NotAFormulaException); // not convertible since integer division not supported

		f = varA + varA * varA - varB * varB - one / (varA * varB) + varB - varC;
		EXPECT_EQ("v1^2+v1-v1^-1*v2^-1-v2^2+v2-v3", toString(f));
		EXPECT_EQ("int_sub(int_add(int_sub(int_sub(int_add(int_mul(v1, v1), v1), int_mul(int_div(1, v1), int_div(1, v2))), int_mul(v2, v2)), v2), v3)",
		          toString(*toIR(manager, f)));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));
		EXPECT_THROW(toFormula(toIR(manager, f)), NotAFormulaException); // not convertible since integer division not supported
	}

	TEST(ArithmeticTest, nonVariableValues) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr type = builder.getLangBasic().getInt4();

		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);

		vector<FieldPtr> entries;
		entries.push_back(core::Field::get(manager, builder.stringValue("a"), type));
		entries.push_back(core::Field::get(manager, builder.stringValue("b"), type));
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

		f = toFormula(tmp);
		EXPECT_EQ("v1+v3.a", toString(f));
		EXPECT_EQ("int_add(v1, composite_member_access(v3, a, type<int<4>>))", toString(*toIR(manager, f)));
		EXPECT_EQ(f, toFormula(toIR(manager, f)));
		EXPECT_EQ(toIR(manager, f), toIR(manager, toFormula(toIR(manager, f))));
		EXPECT_PRED1(empty, check(toIR(manager, f), all));
	}

	TEST(ArithmeticTest, fromIRExpr) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string, NodePtr> symbols;
		symbols["v1"] = builder.variable(builder.parseType("int<4>"),1);

		// from expr: int_add(int_add(int__ul(int_mul(0, 4), 4), int_mul(0, 4)), v112)
		auto expr = builder.parseExpr("0 * 4 * 4 + (0 * 4) + v1", symbols);

		EXPECT_EQ("int_add(int_add(int_mul(int_mul(0, 4), 4), int_mul(0, 4)), v1)", toString(*expr));

		auto f = toFormula(expr);
		EXPECT_EQ("v1", toString(f));

		// add some devision
		expr = builder.parseExpr("(1 * 4)/2");
		EXPECT_EQ("int_div(int_mul(1, 4), 2)", toString(*expr));

		f = toFormula(expr);
		EXPECT_EQ("2", toString(f));

		symbols.clear();
		symbols["x"] = builder.variable(builder.parseType("int<4>"), 6);

		// some more complex stuff
		mgr.setNextFreshID(2);
		expr = builder.parseExpr("((15/2) * x) / ((20/6) * x)", symbols);
		EXPECT_EQ("int_div(int_mul(int_div(15, 2), v6), int_mul(int_div(20, 6), v6))", toString(*expr));

		f = toFormula(expr);
		EXPECT_EQ("2", toString(f));


		// test support of division by 1
		expr = builder.parseExpr("(12 * x) / 1", symbols);
		EXPECT_EQ("int_div(int_mul(12, v6), 1)", toString(*expr));
		f = toFormula(expr);
		EXPECT_EQ("12*v6", toString(f));

		// test support for division by -1
		expr = builder.parseExpr("(4 * x) / -1", symbols);
		EXPECT_EQ("int_div(int_mul(4, v6), -1)", toString(*expr));
		f = toFormula(expr);
		EXPECT_EQ("-4*v6", toString(f));
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

		EXPECT_EQ("rec bool_and.{bool_and=fun(ref<bool,f,f,plain> v0, ref<(()=>bool),f,f,plain> v1) {if(ref_deref(v0)) {return ref_deref(v1)();} else {}; "
			      "return false;}}(rec bool_not.{bool_not=fun(ref<bool,f,f,plain> v0) {if(ref_deref(v0)) {return false;} else {return true;};}}(int_le(int_sub(v0, 1), 0)), bind(){rec _.{_=fun(ref<int<4>,f,f,plain> v0) {return "
			      "rec bool_not.{bool_not=fun(ref<bool,f,f,plain> v0) {if(ref_deref(v0)) {return false;} else {return true;};}}(int_le(int_add(int_mul(-1, ref_deref(v0)), 2), 0));}}(v0)})",
			      toString(*builder.normalize(toIR(mgr, c))));
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

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());

		Formula f = Formula(2) + v1 + v2 * 5 - (Product(v1) ^ 2);

		// extract the variables on this formula
		ValueSet&& vl = f.extractValues();
		EXPECT_EQ(2u, vl.size());
	}

	TEST(ArithmeticTest, ConstraintValueExtraction) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());

		Constraint c = eq(Formula(2) + v1 + v2 * 5 - (Product(v1) ^ 2), 0);

		// extract the variables on this formula
		ValueSet&& vl = c.extractValues();
		EXPECT_EQ(2u, vl.size());
	}

	TEST(ArithmeticTest, ConstraintPtrValueExtraction) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());

		Constraint c1 = eq(Formula(2) + v1 + (Product(v1) ^ 2), 0);
		Constraint c2 = (Formula(3) + (v2 ^ 3) < 0);

		Constraint c = c1 or !c2;

		// extract the variables on this formula
		ValueSet&& vl = c.extractValues();
		EXPECT_EQ(2u, vl.size());
	}

	TEST(ArithmeticTest, PiecewiseValueExtraction) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		using utils::ConstraintType;

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v3 = builder.variable(mgr.getLangBasic().getInt4());

		Piecewise pw;

		pw = pw + Piecewise(v1 + (v1 ^ 2) >= 0, 3 + 4 - v1);
		pw = pw + Piecewise(v1 + (v1 ^ 2) < 0 and ne((v2 ^ 2), 0), 3 + v3 - v1);

		// extract the variables on this formula
		ValueSet&& vl = pw.extractValues();
		EXPECT_EQ(3u, vl.size());
	}

	TEST(ArithmeticTest, Replacement) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());

		Formula f = 2 + v1 + v2 * 5 - (v1 ^ 2);
		EXPECT_EQ("-v1^2+v1+5*v2+2", toString(f));

		ValueReplacementMap vrm;
		vrm[v1] = v2 ^ 3;

		f = f.replace(vrm);
		EXPECT_EQ("-v2^6+v2^3+5*v2+2", toString(f));

		ValueSet&& vl = f.extractValues();
		EXPECT_EQ(1u, vl.size());
		EXPECT_EQ(Value(v2), *vl.begin());

		f = -2 - v1 + (v1 ^ 2);
		vrm[v1] = Formula(1) / 2;

		f = f.replace(vrm);
		EXPECT_EQ("-9/4", toString(f));

		EXPECT_TRUE(f.isConstant());
		int64_t val = f.getConstantValue();

		EXPECT_EQ(-2, val);
	}

	TEST(ArithmeticTest, InequalityReplacement) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());

		Formula f = 2 + v1 + v2 * 5 - (v1 ^ 2);
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

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());

		Formula f = -2 - v1 + (v1 ^ 2);
		Constraint c1 = (f <= 0);
		EXPECT_EQ("(v1^2-v1-2 <= 0)", toString(c1));

		Constraint c2 = eq(v1 + (v2 ^ 3), 0);
		EXPECT_EQ("(v1+v2^3 <= 0 and -v1-v2^3 <= 0)", toString(c2));

		Constraint comb = c1 and c2;
		EXPECT_EQ("(v1^2-v1-2 <= 0 and v1+v2^3 <= 0 and -v1-v2^3 <= 0)", toString(comb));

		{
			ValueReplacementMap vrm;
			vrm[v1] = 3;
			Constraint comb2 = comb.replace(vrm);
			EXPECT_EQ("false", toString(comb2));
			EXPECT_TRUE(comb2.isConstant());
		}
		{
			ValueReplacementMap vrm;
			vrm[v1] = 1;
			Constraint comb2 = comb.replace(vrm);
			EXPECT_EQ("(v2^3+1 <= 0 and -v2^3-1 <= 0)", toString(comb2));
			EXPECT_FALSE(comb2.isConstant());
		}
	}

	TEST(ArithmeticTest, PiecewiseValueReplacement) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		using utils::ConstraintType;

		VariablePtr v1 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v2 = builder.variable(mgr.getLangBasic().getInt4());
		VariablePtr v3 = builder.variable(mgr.getLangBasic().getInt4());

		Piecewise pw;
		pw = pw + Piecewise(v1 + (v1 ^ 2) >= 0, 3 + 4 - v1);
		pw = pw + Piecewise(v1 + (v1 ^ 2) < 0 and ne((v2 ^ 2), 0), 3 + v3 - v1);

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
		// 		int_sub(v3, cast<uint<4>>(10))
		// cannot be converted into a formula.
		//
		// Reason:
		// Fix:
		//

		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		// reconstruct expression
		VariablePtr v1 = builder.variable(basic.getInt4());
		ExpressionPtr ten = builder.castExpr(basic.getUInt4(), builder.intLit(10));
		//	ExpressionPtr expr = builder.callExpr(basic.getInt4(), basic.getSignedIntSub(), v1, ten);
		ExpressionPtr expr = builder.sub(v1, ten);

		EXPECT_EQ("int_sub(v1, cast<uint<4>>(10))", toString(*expr));

		// convert to formula
		Formula f = toFormula(expr);
	}


	TEST(ArithmeticTest, PiecewiseToIRAndBack) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		using utils::ConstraintType;

		Formula v1 = Formula(builder.variable(mgr.getLangBasic().getInt4()));
		Formula v2 = Formula(builder.variable(mgr.getLangBasic().getInt4()));

		auto all = checks::getFullCheck();

		Piecewise pw;
		EXPECT_EQ("0 -> if true", toString(pw));
		EXPECT_EQ("0", toString(*toIR(mgr, pw)));
		EXPECT_PRED1(empty, check(toIR(mgr, pw), all));

		pw += Piecewise(v1 + v2 <= 0, 3 + 4 - v1);
		EXPECT_EQ("-v1+7 -> if (v1+v2 <= 0); 0 -> if (!(v1+v2 <= 0))", toString(pw));
		EXPECT_EQ("rec ite.{ite=fun(ref<bool,f,f,plain> v0, ref<(()=>'b),f,f,plain> v1, ref<(()=>'b),f,f,plain> v2) {if(ref_deref(v0)) {return "
			      "ref_deref(v1)();} else {return ref_deref(v2)();};}}(int_le(int_add(v1, v2), 0), bind(){rec _.{_=fun(ref<int<4>,f,f,plain> v0) {return "
			      "int_add(int_mul(-1, ref_deref(v0)), 7);}}(v1)}, rec _.{_=fun() {return 0;}})",
			      toString(*builder.normalize(toIR(mgr, pw))));
		EXPECT_PRED1(empty, check(toIR(mgr, pw), all));

		pw += Piecewise(v2 <= 0, v2);
		EXPECT_PRED1(empty, check(toIR(mgr, pw), all));
		// apply type checker
	}

	TEST(ArithmeticTest, SelectToFormula) {
		// something like select(1,2,int_lt) should be convertible to a formula

		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		auto a = builder.intLit(1);
		auto b = builder.intLit(1);

		// check whether it is convertible
		EXPECT_EQ(toFormula(a), toFormula(builder.select(a, b, basic.getSignedIntLt())));

		// the following should not be convertible
		auto v = builder.variable(basic.getInt4(), 1);
		EXPECT_THROW(toFormula(builder.select(a, v, basic.getSignedIntLt())), NotAFormulaException);
	}

	TEST(ArithmeticTest, NestedSelectToPiecewise) {
		// something like select(select(1,2,int_lt),select(2,1,int_lt),int_lt) should be convertible to a piecewise

		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		auto a = builder.intLit(1);
		auto b = builder.intLit(2);
		auto c = builder.intLit(3);
		auto lt = basic.getSignedIntLt();

		// check whether it is convertible
		EXPECT_EQ(toPiecewise(a), toPiecewise(builder.select(builder.select(a, b, lt), c, lt)));

		// the following should be also convertable
		auto v1 = builder.variable(basic.getInt4(), 1);
		auto v2 = builder.variable(basic.getInt4(), 2);
		auto v3 = builder.variable(basic.getInt4(), 3);

		EXPECT_EQ("v1 -> if (!(-v1+v2 <= 0) and !(-v1+v3 <= 0)); v3 -> if (!(-v1+v2 <= 0) and -v1+v3 <= 0) or (-v1+v2 <= 0 and -v2+v3 <= 0); v2 -> if (-v1+v2 "
		          "<= 0 and !(-v2+v3 <= 0))",
		          toString(toPiecewise(builder.select(builder.select(v1, v2, lt), v3, lt))));
		EXPECT_EQ("v1 -> if (!(-v2+v3 <= 0) and !(-v1+v2 <= 0)) or (-v2+v3 <= 0 and !(-v1+v3 <= 0)); v2 -> if (!(-v2+v3 <= 0) and -v1+v2 <= 0); v3 -> if "
		          "(-v2+v3 <= 0 and -v1+v3 <= 0)",
		          toString(toPiecewise(builder.select(v1, builder.select(v2, v3, lt), lt))));
	}


} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
