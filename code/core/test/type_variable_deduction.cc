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

#include "insieme/core/ast_builder.h"

#include "insieme/core/analysis/type_variable_deduction.h"

namespace insieme {
namespace core {
namespace analysis {

using namespace utils::set;



TEST(TypeVariableConstraints, Solving) {

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

	TypeVariablePtr var = builder.typeVariable("a");
	TypePtr typeA = builder.genericType("A");
	TypePtr typeB = builder.genericType("B");

	// test empty set of constraints
	TypeVariableConstraints constraints;
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
	EXPECT_EQ("{'a->uint<2>}", toString(*res));

	EXPECT_PRED2(isSubTypeOf, res->applyTo(manager, var), basic.getInt4());
	EXPECT_PRED2(isSubTypeOf, res->applyTo(manager, var), basic.getUInt4());
	EXPECT_EQ(res->applyTo(manager, var), getBiggestCommonSubType(basic.getInt4(), basic.getUInt4()));

	// add a final sub-type constraint
	constraints.addSubtypeConstraint(var, basic.getString());
	res = constraints.solve();
	EXPECT_FALSE(res);


	// clear constraints and test super-type constraints
	constraints.clear();
	constraints.addSupertypeConstraint(var, basic.getInt4());
	res = constraints.solve();
	EXPECT_TRUE(res);
	EXPECT_EQ("{'a->int<4>}", toString(*res));

	constraints.addSupertypeConstraint(var, basic.getUInt4());
	res = constraints.solve();
	EXPECT_TRUE(res);
	EXPECT_EQ("{'a->int<8>}", toString(*res));

	EXPECT_PRED2(isSubTypeOf, basic.getInt4(), res->applyTo(manager, var));
	EXPECT_PRED2(isSubTypeOf, basic.getUInt4(), res->applyTo(manager, var));
	EXPECT_EQ(res->applyTo(manager, var), getSmallestCommonSuperType(basic.getInt4(), basic.getUInt4()));


	// clear and use combined constraints
	constraints.clear();
	constraints.addSubtypeConstraint(var, basic.getInt4());
	constraints.addSupertypeConstraint(var, basic.getInt4());
	res = constraints.solve();
	EXPECT_TRUE(res);
	EXPECT_EQ("{'a->int<4>}", toString(*res));


	// clear and add unsatisfiable constraints
	constraints.clear();
	constraints.addSupertypeConstraint(var, basic.getInt4());
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
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

	TypePtr uint2 = basic.getUInt2();
	TypePtr uint4 = basic.getUInt4();
	TypePtr uint8 = basic.getUInt8();

	TypeVariablePtr var = builder.typeVariable("x");

	TypeVariableConstraints constraints;
	constraints.addSupertypeConstraint(var, uint2);
	constraints.addSubtypeConstraint(var, uint8);

	auto res = constraints.solve();
	EXPECT_TRUE(res);
	if (res) {
		auto substitute = res->applyTo(var);
		EXPECT_EQ(uint8, substitute);
		EXPECT_PRED2(isSubTypeOf, uint2, substitute);
		EXPECT_PRED2(isSubTypeOf, substitute, uint8);
	}

	// try reverse direction
	constraints.clear();
	constraints.addSupertypeConstraint(var, uint8);
	constraints.addSubtypeConstraint(var, uint2);
	EXPECT_FALSE(constraints.solve());

	// add a equality constraint
	constraints.clear();
	constraints.addSupertypeConstraint(var, uint2);
	constraints.addSubtypeConstraint(var, uint8);
	constraints.addEqualsConstraint(var, uint4);
	res = constraints.solve();

	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(uint4, res->applyTo(var));
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
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

	TypeVariablePtr var = builder.typeVariable("x");

	TypePtr int4 = basic.getInt4();
	TypePtr uint4 = basic.getUInt4();

	TypeVariableConstraints constraints;
	constraints.addSupertypeConstraint(var, int4);
	constraints.addSupertypeConstraint(var, uint4);
	constraints.addSubtypeConstraint(var, int4);
	constraints.addSubtypeConstraint(var, uint4);

	auto res = constraints.solve();
	EXPECT_FALSE(res);
	if (res) {
		EXPECT_EQ("", toString(*res));

		EXPECT_PRED2(isSubTypeOf, res->applyTo(var), int4);
		EXPECT_PRED2(isSubTypeOf, res->applyTo(var), uint4);
		EXPECT_PRED2(isSubTypeOf, int4, res->applyTo(var));
		EXPECT_PRED2(isSubTypeOf, uint4, res->applyTo(var));
	}
}

TEST(TypeVariableDeduction, getTypeVariableInstantiation) {

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

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
	if (res) EXPECT_EQ("{}", toString(*res));


	// now: pass an integer to a variable parameter
	res = getTypeVariableInstantiation(manager, toVector(varA), toVector(int4));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'a->int<4>}", toString(*res));

	// ... should also work with beta
	res = getTypeVariableInstantiation(manager, toVector(varB), toVector(int4));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'b->int<4>}", toString(*res));

	// ... but not the other way around!!
	res = getTypeVariableInstantiation(manager, toVector(int4), toVector(varA));
	EXPECT_FALSE(res);

	// test with multiple variables
	res = getTypeVariableInstantiation(manager, toVector(varA, varB), toVector(int4, uint4));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(int4, res->applyTo(varA));
	if (res) EXPECT_EQ(uint4, res->applyTo(varB));

	// test with multiple variables and multiple occurrences
	res = getTypeVariableInstantiation(manager, toVector(varA, varB, varA), toVector(int4, uint4, uint4));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(int8, res->applyTo(varA));
	if (res) EXPECT_EQ(uint4, res->applyTo(varB));

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
	res = getTypeVariableInstantiation(manager, toVector(uint8) , toVector(uint4));
	EXPECT_TRUE(res);

}

TEST(TypeVariableDeduction, vectorsAndArrays) {

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

	// create some types
	TypePtr varA = builder.typeVariable("a");
	TypePtr varB = builder.typeVariable("b");

	TypePtr int4 = basic.getInt4();
	TypePtr uint4 = basic.getUInt4();
	TypePtr vectorInt12 = builder.vectorType(int4, builder.concreteIntTypeParam(12));
	TypePtr vectorInt14 = builder.vectorType(int4, builder.concreteIntTypeParam(14));
	TypePtr vectorUInt12 = builder.vectorType(uint4, builder.concreteIntTypeParam(12));
	TypePtr vectorUInt14 = builder.vectorType(uint4, builder.concreteIntTypeParam(14));
	TypePtr vectorGen12 = builder.vectorType(varA, builder.concreteIntTypeParam(12));
	TypePtr vectorIntA  = builder.vectorType(int4, builder.variableIntTypeParam('l'));
	TypePtr vectorGenA  = builder.vectorType(varA, builder.variableIntTypeParam('l'));

	TypePtr array1Int = builder.arrayType(int4);
	TypePtr array2Int = builder.arrayType(int4, builder.concreteIntTypeParam(2));
	TypePtr arrayAInt = builder.arrayType(int4, builder.variableIntTypeParam('a'));

	// OK - now, lets test something difficult - pass a vector to an alpha
	auto res = getTypeVariableInstantiation(manager, toVector(varA), toVector(vectorInt12));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'a->vector<int<4>,12>}", toString(*res));

	// not the other way around ...
	res = getTypeVariableInstantiation(manager, toVector(vectorInt12), toVector(varA));
	EXPECT_FALSE(res);

	// ... now with a vector with a generic variable (even the same)
	res = getTypeVariableInstantiation(manager, toVector(varA), toVector(vectorGen12));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'a->vector<'a,12>}", toString(*res));


	// Let's test two arguments (same type)
	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(int4, int4));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'a->int<4>}", toString(*res));

	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(int4, uint4));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'a->int<8>}", toString(*res));

	// ... now - let's test two vectors of different size
	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(vectorInt12, vectorInt14));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{'a->array<int<4>,1>}", toString(*res));

	// test it with int-type parameter
	res = getTypeVariableInstantiation(manager, toVector(vectorIntA), toVector(vectorInt12));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{#l->12}", toString(*res));

	// test vectors with different, yet related sub-types (should fail)
	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(vectorInt12, vectorUInt12));
	EXPECT_FALSE(res);

	// different type and different size
	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(vectorInt12, vectorUInt14));
	EXPECT_FALSE(res);


	// different variables however should work
	res = getTypeVariableInstantiation(manager, toVector(varA, varB), toVector(vectorInt12, vectorUInt14));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(vectorInt12, res->applyTo(varA));
	if (res) EXPECT_EQ(vectorUInt14, res->applyTo(varB));

	// also with multiple occurrences
	res = getTypeVariableInstantiation(manager, toVector(varA, varB, varA), toVector(vectorInt12, vectorUInt14, vectorInt14));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(array1Int, res->applyTo(varA));
	if (res) EXPECT_EQ(vectorUInt14, res->applyTo(varB));


	// check for variable array dimensions
	res = getTypeVariableInstantiation(manager, toVector(arrayAInt), toVector(array2Int));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{#a->2}", toString(*res));

	// check for vector being passed ot an generic array
	res = getTypeVariableInstantiation(manager, toVector(arrayAInt), toVector(vectorInt12));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{#a->1}", toString(*res));

}

TEST(TypeVariableDeduction, intTypeParams) {

	NodeManager manager;
	ASTBuilder builder(manager);

	IntTypeParamPtr varA = builder.variableIntTypeParam('a');
	IntTypeParamPtr varB = builder.variableIntTypeParam('b');
	IntTypeParamPtr const1 = builder.concreteIntTypeParam(1);
	IntTypeParamPtr const2 = builder.concreteIntTypeParam(2);

	TypePtr typeA = builder.genericType("T", TypeList(), toVector(varA));
	TypePtr typeB = builder.genericType("T", TypeList(), toVector(varB));
	TypePtr type1 = builder.genericType("T", TypeList(), toVector(const1));
	TypePtr type2 = builder.genericType("T", TypeList(), toVector(const2));
	TypePtr typeAA = builder.genericType("T", TypeList(), toVector(varA, varA));
	TypePtr typeAB = builder.genericType("T", TypeList(), toVector(varA, varB));
	TypePtr type11 = builder.genericType("T", TypeList(), toVector(const1, const1));
	TypePtr type12 = builder.genericType("T", TypeList(), toVector(const1, const2));


	// most straight forward test
	auto res = getTypeVariableInstantiation(manager, toVector(typeA), toVector(type1));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ("{#a->1}", toString(*res));

	// test with two variables
	res = getTypeVariableInstantiation(manager, toVector(typeA, typeB), toVector(type1, type2));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(const1, res->applyTo(varA));
	if (res) EXPECT_EQ(const2, res->applyTo(varB));


	// test with two times the same variable - same value (should be OK)
	res = getTypeVariableInstantiation(manager, toVector(typeA, typeA), toVector(type1, type1));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(const1, res->applyTo(varA));

	res = getTypeVariableInstantiation(manager, toVector(typeA, typeA), toVector(type2, type2));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(const2, res->applyTo(varA));


	// test with two times the same variable - different values (should be an ERROR)
	res = getTypeVariableInstantiation(manager, toVector(typeA, typeA), toVector(type1, type2));
	EXPECT_FALSE(res);

	res = getTypeVariableInstantiation(manager, toVector(typeA, typeB), toVector(type1, type2));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(const1, res->applyTo(varA));
	if (res) EXPECT_EQ(const2, res->applyTo(varB));

	// multiple occurrences
	res = getTypeVariableInstantiation(manager, toVector(typeAA, typeA), toVector(type11, type1));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(const1, res->applyTo(varA));

	res = getTypeVariableInstantiation(manager, toVector(typeAA, typeA), toVector(type11, type2));
	EXPECT_FALSE(res);

	// multiple occurrences
	res = getTypeVariableInstantiation(manager, toVector(typeAA, typeA), toVector(type12, type1));
	EXPECT_FALSE(res);

	res = getTypeVariableInstantiation(manager, toVector(typeAB, typeA), toVector(type11, type1));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(const1, res->applyTo(varA));
	if (res) EXPECT_EQ(const1, res->applyTo(varB));
}

TEST(TypeVariableDeduction, FunctionTypes) {

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

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
	if (res) EXPECT_EQ(funII, res->applyTo(varA));

	// pass two different functions to the same variable
	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(funII, funUU));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(builder.functionType(toVector(basic.getUInt2()), basic.getInt8()), res->applyTo(varA));

	// pass two different functions to the same variable
	res = getTypeVariableInstantiation(manager, toVector(varA, varA), toVector(funII, funUU));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(funRes, res->applyTo(varA));


	// --------------------------- functions with structure -------------------------

	res = getTypeVariableInstantiation(manager, toVector(funAA), toVector(funII));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(int4, res->applyTo(varA));

	res = getTypeVariableInstantiation(manager, toVector(funAA, funAA), toVector(funII, funUU));
	EXPECT_FALSE(res);
	if (res) EXPECT_EQ("", toString(*res));

	res = getTypeVariableInstantiation(manager, toVector(funAB, funAB), toVector(funII, funUU));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(getBiggestCommonSubType(int4, uint4), res->applyTo(varA));
	if (res) EXPECT_EQ(getSmallestCommonSuperType(int4, uint4), res->applyTo(varB));


	// TODO: test with multiple arguments
	TypePtr funIII = builder.functionType(toVector(int4, int4), int4);

}

TEST(TypeVariableDeduction, VectorFunctionTypes) {

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

	// test a function accepting an arbitrary array being an argument of another call

	IntTypeParamPtr varL = builder.variableIntTypeParam('l');
	TypePtr int4 = basic.getInt4();
	TypePtr array = builder.arrayType(int4, varL);
	TypePtr arg = builder.functionType(toVector(array), int4);
	EXPECT_EQ("((array<int<4>,#l>)->int<4>)", toString(*arg));

	TypePtr vector = builder.vectorType(int4, builder.concreteIntTypeParam(12));
	TypePtr param = builder.functionType(toVector(vector), int4);
	EXPECT_EQ("((vector<int<4>,12>)->int<4>)", toString(*param));

	auto res = getTypeVariableInstantiation(manager, toVector(param), toVector(arg));
	EXPECT_TRUE(res);

}


TEST(TypeVariableDeduction, ReductionBug) {

	// The problem:
	// 		within the expression "vector.reduction(v3956, 1, uint.mul)"
	//		a function of type ((vector<'elem,#l>,'res,(('elem,'res)->'res))->'res)
	//		is called using [vector<uint<4>,3>,uint<4>,((uint<#a>,uint<#a>)->uint<#a>))]
	//		The inference is not working ...
	//

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.basic;

	TypePtr uint4 = basic.getUInt4();
	TypePtr vector = builder.vectorType(uint4, builder.concreteIntTypeParam(3));

	CallExprPtr call = builder.callExpr(basic.getVectorReduction(),
			toVector<ExpressionPtr>(builder.literal(vector, "x"), builder.literal(uint4, "1"), basic.getUnsignedIntMul()));

	auto res = getTypeVariableInstantiation(manager, call);
//	EXPECT_TRUE(res);
}


} // end namespace analysis
} // end namespace core
} // end namespace insieme

