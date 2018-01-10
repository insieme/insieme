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

#include <gtest/gtest.h>

#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/types/subtype_constraints.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace types {

	using namespace utils::set;


	TEST(TypeVariableConstraints, Solving) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypeVariablePtr var = builder.typeVariable("a");
		TypePtr typeA = builder.genericType("A");
		TypePtr typeB = builder.genericType("B");

		// test empty set of constraints
		SubTypeConstraints constraints;
		auto res = constraints.solve();
		EXPECT_TRUE(res);
		EXPECT_EQ("{}", toString(*res));

		// test equality type
		constraints.addEqualsConstraint(var, typeA);
		res = constraints.solve();
		EXPECT_TRUE(res);
		EXPECT_EQ("{'a->A}", toString(*res));

		// test same type again (idempotent)
		constraints.addEqualsConstraint(var, typeA);
		res = constraints.solve();
		EXPECT_TRUE(res);
		EXPECT_EQ("{'a->A}", toString(*res));

		// add another type to the equality constraints => unsolvable
		constraints.addEqualsConstraint(var, typeB);
		res = constraints.solve();
		EXPECT_FALSE(res);

		// clear constraints and test sub-type constraints
		constraints.clear();
		constraints.addSubtypeConstraint(var, basic.getInt4());
		res = constraints.solve();
		EXPECT_TRUE(res);
		EXPECT_EQ("{'a->int<4>}", toString(*res));

		constraints.addSubtypeConstraint(var, basic.getUInt4());
		res = constraints.solve();
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->uint<2>}", toString(*res)); }

		if(res) { EXPECT_PRED2(isSubTypeOf, res->applyTo(manager, var), basic.getInt4()); }
		if(res) { EXPECT_PRED2(isSubTypeOf, res->applyTo(manager, var), basic.getUInt4()); }
		if(res) { EXPECT_EQ(res->applyTo(manager, var), getBiggestCommonSubType(basic.getInt4(), basic.getUInt4())); }

		// add a final sub-type constraint
		constraints.addSubtypeConstraint(var, basic.getString());
		res = constraints.solve();
		EXPECT_FALSE(res);


		// clear constraints and test super-type constraints
		constraints.clear();
		constraints.addSubtypeConstraint(basic.getInt4(), var);
		res = constraints.solve();
		EXPECT_TRUE(res);
		EXPECT_EQ("{'a->int<4>}", toString(*res));

		constraints.addSubtypeConstraint(basic.getUInt4(), var);
		res = constraints.solve();
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->int<8>}", toString(*res)); }

		if(res) { EXPECT_PRED2(isSubTypeOf, basic.getInt4(), res->applyTo(manager, var)); }
		if(res) { EXPECT_PRED2(isSubTypeOf, basic.getUInt4(), res->applyTo(manager, var)); }
		if(res) { EXPECT_EQ(res->applyTo(manager, var), getSmallestCommonSuperType(basic.getInt4(), basic.getUInt4())); }


		// clear and use combined constraints
		constraints.clear();
		constraints.addSubtypeConstraint(var, basic.getInt4());
		constraints.addSubtypeConstraint(basic.getInt4(), var);
		res = constraints.solve();
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->int<4>}", toString(*res)); }


		// clear and add unsatisfiable constraints
		constraints.clear();
		constraints.addSubtypeConstraint(basic.getInt4(), var);
		constraints.addSubtypeConstraint(var, basic.getFloat());
		res = constraints.solve();
		EXPECT_FALSE(res);


		// test unsatisfiable flag
		constraints.clear();
		EXPECT_TRUE(constraints.isSatisfiable());
		EXPECT_TRUE(constraints.solve());

		constraints.addEqualsConstraint(var, basic.getInt4());
		EXPECT_TRUE(constraints.isSatisfiable());
		EXPECT_TRUE(constraints.solve());

		constraints.makeUnsatisfiable();
		EXPECT_FALSE(constraints.isSatisfiable());
		EXPECT_FALSE(constraints.solve());

		constraints.addEqualsConstraint(var, basic.getFloat());
		EXPECT_FALSE(constraints.solve());

		constraints.clear();
		EXPECT_TRUE(constraints.isSatisfiable());
		EXPECT_TRUE(constraints.solve());
	}

	TEST(TypeVariableConstraints, ConstraintCombinations) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr uint2 = basic.getUInt2();
		TypePtr uint4 = basic.getUInt4();
		TypePtr uint8 = basic.getUInt8();

		TypeVariablePtr var = builder.typeVariable("x");

		SubTypeConstraints constraints;
		constraints.addSubtypeConstraint(uint2, var);
		constraints.addSubtypeConstraint(var, uint8);

		auto res = constraints.solve();
		EXPECT_TRUE(res);
		if(res) {
			auto substitute = res->applyTo(var);
			EXPECT_EQ(uint2, substitute);
			EXPECT_PRED2(isSubTypeOf, uint2, substitute);
			EXPECT_PRED2(isSubTypeOf, substitute, uint8);
		}

		// try reverse direction
		constraints.clear();
		constraints.addSubtypeConstraint(uint8, var);
		constraints.addSubtypeConstraint(var, uint2);
		EXPECT_FALSE(constraints.solve());

		// add a equality constraint
		constraints.clear();
		constraints.addSubtypeConstraint(uint2, var);
		constraints.addSubtypeConstraint(var, uint8);
		constraints.addEqualsConstraint(var, uint4);
		res = constraints.solve();

		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(uint4, res->applyTo(var)); }
	}

	TEST(TypeVariableConstraints, EqualSuperSubTypeConstraintsBug) {
		// The Bug:
		//		the constraint [ x < {AP(uint<4>),AP(int<4>)} && x = {} && x > {AP(uint<4>),AP(int<4>)} ]
		//		seems to be solvable.
		//
		// Reason:
		//		The super-type constraints have been interpreted as sub-type constraints when both have been present.
		//
		// Fix:
		//		The super-type constraints are now correctly considered


		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypeVariablePtr var = builder.typeVariable("x");

		TypePtr int4 = basic.getInt4();
		TypePtr uint4 = basic.getUInt4();

		SubTypeConstraints constraints;
		constraints.addSubtypeConstraint(int4, var);
		constraints.addSubtypeConstraint(uint4, var);
		constraints.addSubtypeConstraint(var, int4);
		constraints.addSubtypeConstraint(var, uint4);

		auto res = constraints.solve();
		EXPECT_FALSE(res);
		if(res) {
			EXPECT_EQ("", toString(*res));

			EXPECT_PRED2(isSubTypeOf, res->applyTo(var), int4);
			EXPECT_PRED2(isSubTypeOf, res->applyTo(var), uint4);
			EXPECT_PRED2(isSubTypeOf, int4, res->applyTo(var));
			EXPECT_PRED2(isSubTypeOf, uint4, res->applyTo(var));
		}
	}

	TEST(TypeVariableDeduction, getTypeVariableInstantiation) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		// create some types
		TypePtr varA = builder.typeVariable("a");
		TypePtr varB = builder.typeVariable("b");

		TypePtr int4 = basic.getInt4();
		TypePtr uint4 = basic.getUInt4();
		TypePtr int8 = basic.getInt8();
		TypePtr uint8 = basic.getUInt8();

		TypePtr typeA = builder.genericType("A");
		TypePtr typeB = builder.genericType("B");

		// test passing int4 to int4 - simple
		auto res = getTypeVariableInstantiation(manager, toVector(int4), toVector(int4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{}", toString(*res)); }


		// now: pass an integer to a variable parameter
		res = getTypeVariableInstantiation(manager, toVector(varA), toVector(int4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->int<4>}", toString(*res)); }

		// ... should also work with beta
		res = getTypeVariableInstantiation(manager, toVector(varB), toVector(int4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'b->int<4>}", toString(*res)); }

		// ... but not the other way around!!
		res = getTypeVariableInstantiation(manager, toVector(int4), toVector(varA));
		EXPECT_FALSE(res);

		// test with multiple variables
		res = getTypeVariableInstantiation(manager, toVector(varA, varB), toVector(int4, uint4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(int4, res->applyTo(varA)); }
		if(res) { EXPECT_EQ(uint4, res->applyTo(varB)); }

		// test with multiple variables and multiple occurrences
		res = getTypeVariableInstantiation(manager, toVector(varA, varB, varA), toVector(int4, uint4, uint4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(int8, res->applyTo(varA)); }
		if(res) { EXPECT_EQ(uint4, res->applyTo(varB)); }

		// passing incompatible types
		// different family names
		res = getTypeVariableInstantiation(manager, toVector(typeA), toVector(typeB));
		EXPECT_FALSE(res);

		res = getTypeVariableInstantiation(manager, toVector(int4), toVector(uint4));
		EXPECT_FALSE(res);

		// different int-type parameter parameter
		res = getTypeVariableInstantiation(manager, toVector(int4), toVector(int8));
		EXPECT_FALSE(res);

		// pass a sub-type to a super type
		res = getTypeVariableInstantiation(manager, toVector(uint8), toVector(uint4));
		EXPECT_TRUE(res);
	}

	TEST(TypeVariableDeduction, vectorsAndArrays) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		// create some types
		TypePtr varA = builder.typeVariable("a");
		TypePtr varB = builder.typeVariable("b");

		TypePtr int4 = basic.getInt4();
		TypePtr uint4 = basic.getUInt4();
		TypePtr vectorInt12 = builder.parseType("vector<int<4>,12u>");
		TypePtr vectorInt14 = builder.parseType("vector<int<4>,14u>");
		TypePtr vectorUInt12 = builder.parseType("vector<uint<4>,12u>");
		TypePtr vectorUInt14 = builder.parseType("vector<uint<4>,14u>");
		TypePtr vectorGen12 = builder.parseType("vector<'a,12u>");
		;
		TypePtr vectorIntA = builder.parseType("vector<int<4>,'l>");
		TypePtr vectorGenA = builder.parseType("vector<'a,'l>");

		TypePtr array1Int = builder.arrayType(int4);
		TypePtr array2Int = builder.arrayType(int4, builder.uintLit(2));
		TypePtr arrayAInt = builder.parseType("array<int<4>,'a>");

		// OK - now, lets test something difficult - pass a vector to an alpha
		auto res = getTypeVariableInstantiation(manager, toVector(varA), toVector(vectorInt12));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->vector<int<4>,12u>}", toString(*res)); }

		// not the other way around ...
		res = getTypeVariableInstantiation(manager, toVector(vectorInt12), toVector(varA));
		EXPECT_FALSE(res);

		// ... now with a vector with a generic variable (even the same)
		res = getTypeVariableInstantiation(manager, toVector(varA), toVector(vectorGen12));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->vector<'a,12u>}", toString(*res)); }

		// ... now with a vector with a generic variable and generic size (even the same)
		res = getTypeVariableInstantiation(manager, toVector(varA), toVector(vectorGenA));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->vector<'a,'l>}", toString(*res)); }


		// Let's test two arguments (same type)
		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(int4, int4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->int<4>}", toString(*res)); }

		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(int4, uint4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->int<8>}", toString(*res)); }

		// ... now - let's test two vectors of different size
		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(vectorInt12, vectorInt14));
		EXPECT_FALSE(res);

		// test it with int-type parameter
		res = getTypeVariableInstantiation(manager, toVector(vectorIntA), toVector(vectorInt12));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'l->12u}", toString(*res)); }

		// test vectors with different, yet related sub-types (should fail)
		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(vectorInt12, vectorUInt12));
		EXPECT_FALSE(res);

		// different type and different size
		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(vectorInt12, vectorUInt14));
		EXPECT_FALSE(res);


		// different variables however should work
		res = getTypeVariableInstantiation(manager, toVector(varA, varB), toVector(vectorInt12, vectorUInt14));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(vectorInt12, res->applyTo(varA)); }
		if(res) { EXPECT_EQ(vectorUInt14, res->applyTo(varB)); }

		// also with multiple occurrences
		res = getTypeVariableInstantiation(manager, toVector(varA, varB, varA), toVector(vectorInt12, vectorUInt14, vectorInt14));
		EXPECT_FALSE(res);


		// check for variable array dimensions
		res = getTypeVariableInstantiation(manager, toVector(arrayAInt), toVector(array2Int));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ("{'a->2u}", toString(*res)); }

		// check for vector being passed ot an generic array
		res = getTypeVariableInstantiation(manager, toVector(arrayAInt), toVector(vectorInt12));
		EXPECT_FALSE(res);
	}

	TEST(TypeVariableDeduction, FunctionTypes) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr int4 = basic.getInt4();
		TypePtr uint4 = basic.getUInt4();

		TypePtr varA = builder.typeVariable("a");
		TypePtr varB = builder.typeVariable("b");

		TypePtr funII = builder.functionType(toVector(int4), int4);
		TypePtr funIU = builder.functionType(toVector(int4), uint4);
		TypePtr funUI = builder.functionType(toVector(uint4), int4);
		TypePtr funUU = builder.functionType(toVector(uint4), uint4);

		TypePtr funRes = builder.functionType(toVector(basic.getUInt2()), basic.getInt8());
		EXPECT_EQ(getSmallestCommonSuperType(funII, funUU), funRes);

		TypePtr funAA = builder.functionType(toVector(varA), varA);
		TypePtr funAB = builder.functionType(toVector(varA), varB);

		// ------------------------------- all in one variable ---------------------------

		// pass full function to one variable
		auto res = getTypeVariableInstantiation(manager, toVector(varA), toVector(funII));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(funII, res->applyTo(varA)); }

		// pass two different functions to the same variable
		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(funII, funUU));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(builder.functionType(toVector(basic.getUInt2()), basic.getInt8()), res->applyTo(varA)); }

		// pass two different functions to the same variable
		res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(funII, funUU));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(funRes, res->applyTo(varA)); }


		// --------------------------- functions with structure -------------------------

		res = getTypeVariableInstantiation(manager, toVector(funAA), toVector(funII));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(int4, res->applyTo(varA)); }

		res = getTypeVariableInstantiation(manager, toVector(funAA, funAA), toVector(funII, funUU));
		EXPECT_FALSE(res);
		if(res) { EXPECT_EQ("", toString(*res)); }

		res = getTypeVariableInstantiation(manager, toVector(funAB, funAB), toVector(funII, funUU));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(getBiggestCommonSubType(int4, uint4), res->applyTo(varA)); }
		if(res) { EXPECT_EQ(getSmallestCommonSuperType(int4, uint4), res->applyTo(varB)); }


		// TODO: test with multiple arguments
		TypePtr funIII = builder.functionType(toVector(int4, int4), int4);
	}

	TEST(TypeVariableDeduction, VectorFunctionTypes) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		// test a function accepting an arbitrary array being an argument of another call

		TypePtr int4 = basic.getInt4();
		TypePtr array = builder.parseType("array<int<4>,'l>");
		TypePtr arg = builder.functionType(toVector(array), int4);
		EXPECT_EQ("((array<int<4>,'l>)->int<4>)", toString(*arg));

		TypePtr vector = builder.parseType("array<int<4>,12>");
		TypePtr param = builder.functionType(toVector(vector), int4);
		EXPECT_EQ("((array<int<4>,12>)->int<4>)", toString(*param));

		auto res = getTypeVariableInstantiation(manager, toVector(param), toVector(arg));
		EXPECT_TRUE(res);
	}


	TEST(TypeVariableDeduction, ReductionBug) {
		// The problem:
		// 		within the expression "vector.reduction(v3956, 1, uint.mul)"
		//		a function of type ((vector<'elem,#l>,'res,(('elem,'res)->'res))->'res)
		//		is called using [vector<uint<4>,3>,uint<4>,((uint<'a>,uint<'a>)->uint<'a>))]
		//		The inference is not working ...
		//

		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr uint4 = basic.getUInt4();
		TypePtr vector = builder.parseType("array<uint<4>,3>");

		CallExprPtr call = builder.callExpr(
				builder.literal("fun", builder.parseType("(array<'a,'n>, 'b, ('b, 'a) -> 'b) -> 'b")),
				toVector<ExpressionPtr>(builder.literal(vector, "x"), builder.literal(uint4, "1"), basic.getUnsignedIntMul()));

		auto res = getTypeVariableInstantiation(manager, call);
		EXPECT_TRUE(res);
	}


	TEST(TypeVariableDeduction, ArgumentBasedTypeConstraints) {
		// The following scenario should be tested:
		//		function type: ('a, ('a -> 'r)) -> 'r
		//		parameter: uint<4> and uint<'a> -> uint<'a>

		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr alpha = builder.typeVariable("a");
		TypePtr rho = builder.typeVariable("r");
		TypePtr genFun = builder.functionType(alpha, rho);

		TypePtr funType = builder.functionType(toVector(alpha, genFun), rho);

		TypePtr uint4 = basic.getUInt4();
		TypePtr uintA = basic.getUIntGen();
		TypePtr id = builder.functionType(uintA, uintA);


		auto res = getTypeVariableInstantiation(manager, toVector(alpha, genFun), toVector(uint4, id));
		EXPECT_TRUE(res);
	}

	TEST(TypeVariableDeduction, PureIntTypeVariableConstraintsBug) {
		// The Problem:
		//		expected: (int<'a>,int<'a>), actual: (int<4>,int<2>) => unable to deduce that 'a = 4


		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr int2 = basic.getInt2();
		TypePtr int4 = basic.getInt4();

		TypePtr intA = basic.getIntGen();

		auto res = getTypeVariableInstantiation(manager, toVector(intA, intA), toVector(int4, int2));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(int4, res->applyTo(intA)); }

		// also test the reverse direction
		res = getTypeVariableInstantiation(manager, toVector(intA, intA), toVector(int2, int4));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(int4, res->applyTo(intA)); }
	}

	// ---------------------------------------------------------------------------------------------------
	// ---------------------------------------------------------------------------------------------------
	// ---------------------------------------------------------------------------------------------------


	bool unifyable(const TypePtr& typeA, const TypePtr& typeB) {
		return isUnifyable(typeA, typeB);
	}

	bool notUnifable(const TypePtr& typeA, const TypePtr& typeB) {
		return !isUnifyable(typeA, typeB);
	}

	bool matchable(const TypePtr& pattern, const TypePtr& type) {
		return getTypeVariableInstantiation(pattern->getNodeManager(), pattern, type);
	}

	bool notMatchable(const TypePtr& pattern, const TypePtr& type) {
		return !matchable(pattern, type);
	}

	TEST(TypeVariableDeduction, Matching) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create some types to "play"
		TypeVariablePtr varA = builder.typeVariable("A");
		TypeVariablePtr varB = builder.typeVariable("B");

		TypePtr constType = builder.genericType("constType");

		TypePtr genTypeA = builder.genericType("type", toVector<TypePtr>(varA));
		TypePtr genTypeB = builder.genericType("type", toVector<TypePtr>(varB));

		TypePtr specializedType = builder.genericType("type", toVector<TypePtr>(constType));

		TypePtr genIntTypeA = builder.parseType("type<'a>");
		TypePtr genIntTypeB = builder.parseType("type<'b>");
		TypePtr specIntType = builder.parseType("type<123>");


		// case: one side is a variable
		EXPECT_PRED2(matchable, varA, constType);
		EXPECT_PRED2(notMatchable, constType, varA);
		EXPECT_PRED2(unifyable, varA, constType);
		EXPECT_PRED2(unifyable, constType, varA);

		// case: both sides are variables
		EXPECT_PRED2(matchable, varA, varB);
		EXPECT_PRED2(matchable, varB, varA);
		EXPECT_PRED2(unifyable, varA, varB);
		EXPECT_PRED2(unifyable, varB, varA);

		// more complex case: type wit
		EXPECT_PRED2(matchable, genTypeA, specializedType);
		EXPECT_PRED2(notMatchable, specializedType, genTypeA);
		EXPECT_PRED2(matchable, genTypeB, specializedType);
		EXPECT_PRED2(notMatchable, specializedType, genTypeB);

		EXPECT_PRED2(unifyable, genTypeA, specializedType);
		EXPECT_PRED2(unifyable, specializedType, genTypeA);
		EXPECT_PRED2(unifyable, genTypeB, specializedType);
		EXPECT_PRED2(unifyable, specializedType, genTypeB);

		// case: int type parameter
		EXPECT_PRED2(matchable, genIntTypeA, specIntType);
		EXPECT_PRED2(notMatchable, specIntType, genIntTypeA);
		EXPECT_PRED2(matchable, genIntTypeB, specIntType);
		EXPECT_PRED2(notMatchable, specIntType, genIntTypeB);

		EXPECT_PRED2(unifyable, genIntTypeA, specIntType);
		EXPECT_PRED2(unifyable, specIntType, genIntTypeA);
		EXPECT_PRED2(unifyable, genIntTypeB, specIntType);
		EXPECT_PRED2(unifyable, specIntType, genIntTypeB);

		// check result of matching process
		auto unifier = getTypeVariableInstantiation(manager, genTypeA, specializedType);
		EXPECT_TRUE(unifier);
		EXPECT_EQ(*unifier->applyTo(manager, genTypeA), *unifier->applyTo(manager, specializedType));
	}


	TEST(TypeVariableDeduction, SubTyping) {
		NodeManager manager;
		IRBuilder builder(manager);

		// check sub-type relation between int and uint

		TypePtr int1 = manager.getLangBasic().getInt1();
		TypePtr int2 = manager.getLangBasic().getInt2();
		TypePtr int4 = manager.getLangBasic().getInt4();
		TypePtr int8 = manager.getLangBasic().getInt8();
		TypePtr intInf = manager.getLangBasic().getIntInf();

		TypePtr uint1 = manager.getLangBasic().getUInt1();
		TypePtr uint2 = manager.getLangBasic().getUInt2();
		TypePtr uint4 = manager.getLangBasic().getUInt4();
		TypePtr uint8 = manager.getLangBasic().getUInt8();
		TypePtr uintInf = manager.getLangBasic().getUIntInf();

		EXPECT_PRED2(matchable, int8, int4);

		EXPECT_PRED2(matchable, intInf, int4);
		EXPECT_PRED2(notMatchable, int4, intInf);

		// check some cases
		EXPECT_PRED2(matchable, int1, int1);
		EXPECT_PRED2(matchable, int2, int2);
		EXPECT_PRED2(matchable, int4, int4);
		EXPECT_PRED2(matchable, int8, int8);
		EXPECT_PRED2(matchable, intInf, intInf);


		EXPECT_PRED2(matchable, int2, int1);
		EXPECT_PRED2(notMatchable, int1, int2);

		EXPECT_PRED2(matchable, int4, int2);
		EXPECT_PRED2(notMatchable, int2, int4);

		EXPECT_PRED2(matchable, int8, int4);
		EXPECT_PRED2(notMatchable, int4, int8);

		EXPECT_PRED2(matchable, intInf, int8);
		EXPECT_PRED2(notMatchable, int8, intInf);


		EXPECT_PRED2(matchable, uint1, uint1);
		EXPECT_PRED2(matchable, uint2, uint2);
		EXPECT_PRED2(matchable, uint4, uint4);
		EXPECT_PRED2(matchable, uint8, uint8);
		EXPECT_PRED2(matchable, uintInf, uintInf);

		EXPECT_PRED2(matchable, uint2, uint1);
		EXPECT_PRED2(notMatchable, uint1, uint2);

		EXPECT_PRED2(matchable, uint4, uint2);
		EXPECT_PRED2(notMatchable, uint2, uint4);

		EXPECT_PRED2(matchable, uint8, uint4);
		EXPECT_PRED2(notMatchable, uint4, uint8);

		EXPECT_PRED2(matchable, uintInf, uint8);
		EXPECT_PRED2(notMatchable, uint8, uintInf);

		// cross signed / unsigned tests
		EXPECT_PRED2(matchable, int8, uint4);
		EXPECT_PRED2(notMatchable, int8, uint8);

		EXPECT_PRED2(matchable, int2, uint1);
		EXPECT_PRED2(notMatchable, int4, uint8);

		EXPECT_PRED2(matchable, intInf, uintInf);
		EXPECT_PRED2(notMatchable, uintInf, intInf);


		// generic cases
		TypePtr intGen = manager.getLangBasic().getIntGen();
		TypePtr uintGen = manager.getLangBasic().getUIntGen();

		EXPECT_PRED2(matchable,intGen,int1);
		EXPECT_PRED2(matchable,intGen,int2);
		EXPECT_PRED2(matchable,intGen,int4);
		EXPECT_PRED2(matchable,intGen,int8);
		EXPECT_PRED2(matchable,intGen,intInf);

		EXPECT_PRED2(matchable,uintGen,uint1);
		EXPECT_PRED2(matchable,uintGen,uint2);
		EXPECT_PRED2(matchable,uintGen,uint4);
		EXPECT_PRED2(matchable,uintGen,uint8);
		EXPECT_PRED2(matchable,uintGen,uintInf);

		// check that unsigned can be passed to signed
		EXPECT_PRED2(matchable,intGen,uint1);
		EXPECT_PRED2(matchable,intGen,uint2);
		EXPECT_PRED2(matchable,intGen,uint4);
		EXPECT_PRED2(matchable,intGen,uint8);
		EXPECT_PRED2(matchable,intGen,uintInf);

		// but signed can not be passed to unsigned
		EXPECT_PRED2(notMatchable,uintGen,int1);
		EXPECT_PRED2(notMatchable,uintGen,int2);
		EXPECT_PRED2(notMatchable,uintGen,int4);
		EXPECT_PRED2(notMatchable,uintGen,int8);
		EXPECT_PRED2(notMatchable,uintGen,intInf);
	}

	TEST(TypeVariableDeduction, VectorMatchingBug) {
		// The Bug:
		//		When matching vectors of different size to parameters ('a,'a), the matching is successful - which it should not.
		//
		// The reason:
		//		The Vector type was not recognized as a generic type and the integer type parameters have been ignored.
		//
		// The fix:
		//		The check for generic types is no longer conducted via the node type token. It is now using a dynamic cast.
		//

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr alpha = builder.typeVariable("a");
		FunctionTypePtr funType = builder.functionType(toVector(alpha, alpha), alpha);

		EXPECT_EQ("(('a,'a)->'a)", toString(*funType));

		TypePtr elem = builder.genericType("A");
		TypePtr vectorA = builder.parseType("array<A,12>");
		TypePtr vectorB = builder.parseType("array<A,14>");

		auto match = getTypeVariableInstantiation(manager, toVector(alpha, alpha), toVector(vectorB, vectorA));
		EXPECT_FALSE(match);

		match = getTypeVariableInstantiation(manager, toVector(alpha, alpha), toVector(vectorB, vectorA));
		EXPECT_FALSE(match);

		match = getTypeVariableInstantiation(manager, toVector(alpha, alpha), toVector(vectorA, vectorA));
		EXPECT_TRUE(match);
		if(match) { EXPECT_EQ("{'a->array<A,12>}", toString(*match)); }

		match = getTypeVariableInstantiation(manager, toVector(alpha, alpha), toVector(vectorB, vectorB));
		EXPECT_TRUE(match);
		if(match) { EXPECT_EQ("{'a->array<A,14>}", toString(*match)); }
	}


	TEST(TypeVariableDeduction, IntTypeParamVariableNameBug) {
		// The Bug:
		//		Invoking pointwise using a generic function type produces esthetically incorrect int type parameter names.
		//
		// The reason:
		//		The renaming of the arguments was not reverted after a matching has been found. This step is actually
		//		not necessary, however, it makes the picking of type variables within the result more predictable.
		//
		// The fix:
		//		This is now done.
		//

		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		auto op = basic.getUnsignedIntAdd();
		auto fun = builder.literal("fun", builder.parseType("(('elem1, 'elem2) => 'res) -> (vector<'elem1,'l>, vector<'elem2,'l>) => vector<'res, 'l>"));
		auto call = builder.callExpr(fun, op);

		EXPECT_EQ("((uint<'a>,uint<'a>)->uint<'a>)", toString(*op->getType()));
		EXPECT_EQ("((vector<uint<'a>,'l>,vector<uint<'a>,'l>)=>vector<uint<'a>,'l>)", toString(*call->getType()));
	}

	TEST(TypeVariableDeduction, PlainFunctionTest) {
		// To be tested:
		//		a function A -> B is a subtype of A => B
		//		hence: passing a value x : A -> B to a function
		//			accepting a parameter p : A => B should be allowed
		//
		//		On the other hand, passing y : A => B to q : A -> B is
		//			not allowed.

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");

		TypePtr funA = builder.functionType(toVector(A), B, FK_PLAIN);
		TypePtr funB = builder.functionType(toVector(A), B, FK_CLOSURE);

		// create a plain and a standard function type
		EXPECT_EQ("((A)->B)", toString(*funA));
		EXPECT_EQ("((A)=>B)", toString(*funB));

		// passing funA to funB => should work
		auto res = getTypeVariableInstantiation(manager, toVector(funB), toVector(funA));
		EXPECT_TRUE(res);

		// passing funB to funA => should not work
		res = getTypeVariableInstantiation(manager, toVector(funA), toVector(funB));
		EXPECT_FALSE(res);

		// also test type variable deduction
		TypePtr alpha = builder.typeVariable("a");
		TypePtr beta = builder.typeVariable("b");
		TypePtr funG = builder.functionType(toVector(alpha), beta, FK_CLOSURE);

		EXPECT_EQ("(('a)=>'b)", toString(*funG));

		res = getTypeVariableInstantiation(manager, toVector(funG), toVector(funA));
		ASSERT_TRUE(res);
		EXPECT_EQ("A", toString(*res->applyTo(alpha)));
		EXPECT_EQ("B", toString(*res->applyTo(beta)));
	}

	TEST(TypeVariableDeduction, ReductionType) {
		/**
		 * There was an error in the type deduction of a call to the reduction operator.
		 * This should be investigated here
		 */

		NodeManager manager;
		IRBuilder builder(manager);

		FunctionTypePtr funType = builder.parseType("(ref<array<'a,1>>,uint<8>,('b,'a)->'b,'b)->'b").as<FunctionTypePtr>();
		EXPECT_TRUE(funType);

		vector<TypePtr> args = {builder.parseType("ref<array<'a,1>>"), builder.parseType("uint<8>"), builder.parseType("('b,'a)->'b"), builder.parseType("'b")};

		auto res = getTypeVariableInstantiation(manager, funType, args);
		EXPECT_TRUE(res);

		EXPECT_EQ("'a", toString(*res->applyTo(builder.typeVariable("a"))));
		EXPECT_EQ("'b", toString(*res->applyTo(builder.typeVariable("b"))));
	}

	TEST(TypeVariableDeduction, RenamingIssue) {
		/**
		 * This test fails if getFreshVariable (type variable renamer) returns e.g. "vN"
		 * -> due to conflict with v2 in definition of ptr_ne
		 */

		NodeManager manager;
		IRBuilder builder(manager);

		auto ir = builder.parseProgram(R"(
			def struct IMP_C {
				mC : ptr<unit>;
				ctor function () {
					ptr_ne(*(this).mC, *(this).mC);
				}
			};
			int<4> function IMP_main (){
				var ref<IMP_C,f,f,plain> v54 = IMP_C::(ref_decl(type_lit(ref<IMP_C,f,f,plain>)));
				return 0;
			}
		)");

		ASSERT_TRUE(ir);

		EXPECT_TRUE(checks::check(ir).empty());
	}

	TEST(TypeVariableDeduction, LambdaConversionBug) {
		/**
		 * This test failed since the parameters of function types nested in generic types have not
		 * been considered for creating type constraints.
		 */
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto funType = builder.parseType("('l, type<('a...) => 'b>) -> ('a...) => 'b").as<FunctionTypePtr>();

		EXPECT_EQ("{'a#1->B,'b->X,'l->A|a->['a#1]}",toString(*getTypeVariableInstantiation(mgr,funType,{
			builder.parseType("A"),
			builder.parseType("type<(B)=>X>")
		})));

		EXPECT_EQ("{'b->X,'l->A|a->[]}",toString(*getTypeVariableInstantiation(mgr,funType,{
			builder.parseType("A"),
			builder.parseType("type<()=>X>")
		})));

		EXPECT_EQ("{'a#1->B,'a#2->C,'b->X,'l->A|a->['a#1,'a#2]}",toString(*getTypeVariableInstantiation(mgr,funType,{
			builder.parseType("A"),
			builder.parseType("type<(B,C)=>X>")
		})));


	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
