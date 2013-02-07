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

#include <boost/functional/hash.hpp>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/type_utils.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/checks/type_checks.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {

using namespace checks;
using namespace utils::set;

bool unifyable(const TypePtr& typeA, const TypePtr& typeB) {
	return isUnifyable(typeA, typeB);
}

bool notUnifable(const TypePtr& typeA, const TypePtr& typeB) {
	return !isUnifyable(typeA, typeB);
}

TEST(TypeUtils, Substitution) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypeVariablePtr varA = builder.typeVariable("A");
	TypeVariablePtr varB = builder.typeVariable("B");

	VariableIntTypeParamPtr paramVarX = builder.variableIntTypeParam('x');
	VariableIntTypeParamPtr paramVarY = builder.variableIntTypeParam('y');

	TypePtr constType = builder.genericType("constType");
	IntTypeParamPtr constParam = builder.concreteIntTypeParam(15);

	TypePtr typeA = builder.genericType("type", toVector<TypePtr>(varA), toVector<IntTypeParamPtr>(paramVarX));
	TypePtr typeB = builder.genericType("type", toVector<TypePtr>(varA, varB), toVector<IntTypeParamPtr>(paramVarX, paramVarY));
	TypePtr typeC = builder.genericType("type", toVector<TypePtr>(typeB, varB), toVector<IntTypeParamPtr>(paramVarY, paramVarY));


	EXPECT_EQ("'A", toString(*varA));
	EXPECT_EQ("'B", toString(*varB));
	EXPECT_EQ("#x", toString(*paramVarX));
	EXPECT_EQ("#y", toString(*paramVarY));

	EXPECT_EQ("constType", toString(*constType));
	EXPECT_EQ("15", toString(*constParam));

	EXPECT_EQ("type<'A,#x>", toString(*typeA));
	EXPECT_EQ("type<'A,'B,#x,#y>", toString(*typeB));
	EXPECT_EQ("type<type<'A,'B,#x,#y>,'B,#y,#y>", toString(*typeC));

	// test empty substitution
	auto all = toVector<TypePtr>(varA, varB, typeA, typeB, typeC);
	Substitution identity;
	for_each(all, [&](const TypePtr& cur) {
		EXPECT_EQ(cur, identity.applyTo(manager, cur));
	});

	// test one variable replacement
	Substitution substitution(varA, varB);
	EXPECT_EQ(varB, substitution.applyTo(manager, varA));
	EXPECT_EQ(varB, substitution.applyTo(manager, varB));

	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'B,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'B,'B,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'B,'B,#x,#y>,'B,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// test one variable replacement
	substitution = Substitution(varA, constType);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,'B,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,'B,#x,#y>,'B,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// test one int type parameter replacement
	substitution = Substitution(paramVarX, paramVarY);
	EXPECT_EQ(paramVarY, substitution.applyTo(paramVarX));
	EXPECT_EQ(paramVarY, substitution.applyTo(paramVarY));

	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A,#y>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,'B,#y,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,'B,#y,#y>,'B,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// test one int type parameter replacement
	substitution = Substitution(paramVarY, constParam);
	EXPECT_EQ(paramVarX, substitution.applyTo(paramVarX));
	EXPECT_EQ(constParam, substitution.applyTo(paramVarY));

	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,'B,#x,15>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,'B,#x,15>,'B,15,15>", toString(*substitution.applyTo(manager, typeC)));


	// add replacement for variable B
	substitution = Substitution(varA, constType);
	substitution.addMapping(varB, typeA);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,#x>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,type<'A,#x>,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,type<'A,#x>,#x,#y>,type<'A,#x>,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// override replacement for second variable
	substitution.addMapping(varB, typeB);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,'B,#x,#y>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,type<'A,'B,#x,#y>,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,type<'A,'B,#x,#y>,#x,#y>,type<'A,'B,#x,#y>,#y,#y>", toString(*substitution.applyTo(manager, typeC)));


	// remove one mapping
	substitution.remMappingOf(varA);
	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,'B,#x,#y>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,type<'A,'B,#x,#y>,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,type<'A,'B,#x,#y>,#x,#y>,type<'A,'B,#x,#y>,#y,#y>", toString(*substitution.applyTo(manager, typeC)));


	// test substitution composition
	Substitution subA(varA, typeB);

	Substitution subB(varB, constType);
	subB.addMapping(paramVarX, constParam);


	EXPECT_EQ("{AP('A)=AP(type<'A,'B,#x,#y>)}", toString(subA.getMapping()));
	EXPECT_EQ("{}", toString(subA.getIntTypeParamMapping()));
	EXPECT_EQ("{AP('B)=AP(constType)}", toString(subB.getMapping()));
	EXPECT_EQ("{AP(#x)=AP(15)}", toString(subB.getIntTypeParamMapping()));

	Substitution combinedAA = Substitution::compose(manager, subA, subA);
	Substitution combinedAB = Substitution::compose(manager, subA, subB);
	Substitution combinedBA = Substitution::compose(manager, subB, subA);
	Substitution combinedBB = Substitution::compose(manager, subB, subB);

	EXPECT_PRED2(containsSubString, toString(combinedAB.getMapping()), "AP('A)=AP(type<'A,constType,15,#y>)");
	EXPECT_PRED2(containsSubString, toString(combinedAB.getMapping()), "AP('B)=AP(constType)");
	EXPECT_PRED2(containsSubString, toString(combinedBA.getMapping()), "AP('A)=AP(type<'A,'B,#x,#y>)");
	EXPECT_PRED2(containsSubString, toString(combinedBA.getMapping()), "AP('B)=AP(constType)");
	EXPECT_EQ("{AP('B)=AP(constType)}", toString(combinedBB.getMapping()));

	EXPECT_EQ("{}", toString(combinedAA.getIntTypeParamMapping()));
	EXPECT_EQ("{AP(#x)=AP(15)}", toString(combinedAB.getIntTypeParamMapping()));
	EXPECT_EQ("{AP(#x)=AP(15)}", toString(combinedBA.getIntTypeParamMapping()));
	EXPECT_EQ("{AP(#x)=AP(15)}", toString(combinedBB.getIntTypeParamMapping()));
}

TEST(TypeUtils, Unification) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypeVariablePtr varA = builder.typeVariable("A");
	TypeVariablePtr varB = builder.typeVariable("B");

	TypePtr constType = builder.genericType("constType");

	TypePtr typeA = builder.genericType("type", toVector<TypePtr>(varA));
	TypePtr typeB = builder.genericType("type", toVector<TypePtr>(varA, varB));
	TypePtr typeC = builder.genericType("type", toVector<TypePtr>(typeB, varB));


	// simple case - unify the same
	EXPECT_TRUE(isUnifyable(varA, varA));
	EXPECT_TRUE(isUnifyable(constType, constType));
	EXPECT_TRUE(isUnifyable(typeA, typeA));

	// unify two variables
	EXPECT_TRUE(isUnifyable(varA, varB));
	auto res = unify(manager, varA, varB);
	EXPECT_TRUE(res);
	if (res) {
		EXPECT_FALSE(res->getMapping().empty());
	}

	Substitution sub = *res;
	EXPECT_EQ("'B", toString(*sub.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*sub.applyTo(manager, varB)));


	// large example
	TypePtr varX = builder.typeVariable("x");
	TypePtr varY = builder.typeVariable("y");
	TypePtr varZ = builder.typeVariable("z");
	TypePtr varU = builder.typeVariable("u");

	{
		TypePtr termGY = builder.genericType("g", toVector(varY));
		TypePtr termA = builder.genericType("f", toVector(varX, termGY, varX));

		TypePtr termGU = builder.genericType("g", toVector(varU));
		TypePtr termHU = builder.genericType("h", toVector(varU));
		TypePtr termB = builder.genericType("f", toVector(varZ, termGU, termHU));

		EXPECT_EQ("f<'x,g<'y>,'x>", toString(*termA));
		EXPECT_EQ("f<'z,g<'u>,h<'u>>", toString(*termB));

		ASSERT_PRED2(unifyable, termA, termB);

		auto unifyingMap = *unify(manager, termA, termB);
		EXPECT_EQ("f<h<'u>,g<'u>,h<'u>>", toString(*unifyingMap.applyTo(manager, termA))) << unifyingMap;
		EXPECT_EQ("f<h<'u>,g<'u>,h<'u>>", toString(*unifyingMap.applyTo(manager, termB)));
		EXPECT_EQ(unifyingMap.applyTo(manager, termA), unifyingMap.applyTo(manager, termB));
	}

	{
		TypePtr termArrayY = builder.genericType("array", toVector(varY));
		TypePtr termA = builder.tupleType(toVector(varX, termArrayY, varX));

		TypePtr termArrayU = builder.genericType("array", toVector(varU));
		TypePtr termSetU = builder.genericType("vector", toVector(varU));
		TypePtr termB = builder.tupleType(toVector(varZ, termArrayU, termSetU));

		EXPECT_EQ("('x,array<'y>,'x)", toString(*termA));
		EXPECT_EQ("('z,array<'u>,vector<'u>)", toString(*termB));

		EXPECT_PRED2(unifyable, termA, termB);
		auto unifyingMap = *unify(manager, termA, termB);
		EXPECT_EQ("(vector<'u>,array<'u>,vector<'u>)", toString(*unifyingMap.applyTo(manager, termA)));
		EXPECT_EQ("(vector<'u>,array<'u>,vector<'u>)", toString(*unifyingMap.applyTo(manager, termB)));
		EXPECT_EQ(unifyingMap.applyTo(manager, termA), unifyingMap.applyTo(manager, termB));
	}
}


TEST(TypeUtils, IntParamUnification) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr typeAx = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParamPtr>(VariableIntTypeParam::get(manager, 'x')));
	TypePtr typeA3 = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParamPtr>(ConcreteIntTypeParam::get(manager, 3)));
	TypePtr typeA4 = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParamPtr>(ConcreteIntTypeParam::get(manager, 4)));

	EXPECT_PRED2(unifyable, typeA3, typeAx);
	EXPECT_PRED2(notUnifable, typeA3, typeA4);

	Substitution res = *unify(manager, typeA3, typeAx);
	EXPECT_EQ(*res.applyTo(manager, typeA3), *res.applyTo(manager, typeAx));
	EXPECT_EQ(*typeA3, *res.applyTo(manager, typeAx));
}

typedef utils::set::PointerSet<TypeVariablePtr> TypeVariableSet;
typedef utils::set::PointerSet<IntTypeParamPtr> ParamSet;

TypeVariableSet getTypeVariables(const TypePtr& ptr) {
	TypeVariableSet res;
	visitDepthFirstOnce(ptr, [&res](const NodePtr& node) {
		if (node->getNodeType() == NT_TypeVariable) {
			res.insert(static_pointer_cast<const TypeVariable>(node));
		}
	}, true);
	return res;
}

ParamSet getParamVariables(const TypePtr& ptr) {
	ParamSet res;
	visitDepthFirstOnce(ptr, [&res](const NodePtr& node) {
		if (node->getNodeType() == NT_VariableIntTypeParam) {
			res.insert(static_pointer_cast<const IntTypeParam>(node));
		}
	}, true);
	return res;
}


TEST(TypeUtils, ReturnTypeDeduction) {

	NodeManager manager;
	IRBuilder builder(manager);

	// some variables and types
	TypePtr varA = TypeVariable::get(manager, "a");
	TypePtr varB = TypeVariable::get(manager, "b");

	TypePtr typeA = GenericType::get(manager, "typeA");
	TypePtr typeB = GenericType::get(manager, "typeB");

	TypePtr genA = GenericType::get(manager, "type", toVector<TypePtr>(varA));
	TypePtr genB = GenericType::get(manager, "type", toVector<TypePtr>(varB));

	TypePtr genSpecA = GenericType::get(manager, "type", toVector<TypePtr>(typeA));
	TypePtr genSpecB = GenericType::get(manager, "type", toVector<TypePtr>(typeB));

	// test some functions
	FunctionTypePtr funType;

	// a simple case
	funType = FunctionType::get(manager, toVector<TypePtr>(varA), varA);
	EXPECT_EQ("(('a)->'a)", toString(*funType));
	EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(typeA))));


	funType = FunctionType::get(manager, toVector<TypePtr>(varA, varB), varA);
	EXPECT_EQ("(('a,'b)->'a)", toString(*funType));
	EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(typeA, typeB))));
	EXPECT_EQ("typeB", toString(*deduceReturnType(funType, toVector<TypePtr>(typeB, typeA))));


	funType = FunctionType::get(manager, toVector<TypePtr>(genA, varA), varA);
	EXPECT_EQ("((type<'a>,'a)->'a)", toString(*funType));
	EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(genSpecA, typeA))));
	EXPECT_EQ("typeB", toString(*deduceReturnType(funType, toVector<TypePtr>(genSpecB, typeB))));
	EXPECT_EQ("'a", toString(*deduceReturnType(funType, toVector<TypePtr>(genA, varA))));

	// test invalid call
	funType = FunctionType::get(manager, toVector<TypePtr>(typeA), typeA);
	EXPECT_EQ("unit", toString(*deduceReturnType(funType, toVector<TypePtr>(typeB))));
	EXPECT_THROW(tryDeduceReturnType(funType, toVector<TypePtr>(typeB)), ReturnTypeDeductionException);


//	// make a call requiring sub-type deduction
//	funType = FunctionType::get(manager, toVector<TypePtr>(varA, varA), varA);
//	EXPECT_EQ("(('a,'a)->'a)", toString(*funType));
//
//	TypePtr vectorTypeA = VectorType::get(manager, typeA, builder.concreteIntTypeParam(12));
//	TypePtr vectorTypeB = VectorType::get(manager, typeA, builder.concreteIntTypeParam(14));
//	EXPECT_EQ("array<typeA,1>", toString(*deduceReturnType(funType, toVector(vectorTypeA, vectorTypeB))));
}

TypeSet getSuperTypes(const TypePtr& type) {
	return type->getNodeManager().getLangBasic().getDirectSuperTypesOf(type);
}

TEST(TypeUtils, IntUintSuperTypes) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	TypePtr int1 = basic.getInt1();
	TypePtr int2 = basic.getInt2();
	TypePtr int4 = basic.getInt4();
	TypePtr int8 = basic.getInt8();
	TypePtr intI = basic.getIntInf();
	TypePtr intG = basic.getIntGen();

	TypePtr uint1 = basic.getUInt1();
	TypePtr uint2 = basic.getUInt2();
	TypePtr uint4 = basic.getUInt4();
	TypePtr uint8 = basic.getUInt8();
	TypePtr uintI = basic.getUIntInf();
	TypePtr uintG = basic.getUIntGen();

	// check each super-type relation
	EXPECT_EQ(toSet<TypeSet>(int2), getSuperTypes(int1));
	EXPECT_EQ(toSet<TypeSet>(int4), getSuperTypes(int2));
	EXPECT_EQ(toSet<TypeSet>(int8), getSuperTypes(int4));
	EXPECT_EQ(toSet<TypeSet>(intI), getSuperTypes(int8));
	EXPECT_EQ(toSet<TypeSet>(), getSuperTypes(intI));
	EXPECT_EQ(toSet<TypeSet>(), getSuperTypes(intG));

	EXPECT_EQ(toSet<TypeSet>(int2, uint2), getSuperTypes(uint1));
	EXPECT_EQ(toSet<TypeSet>(int4, uint4), getSuperTypes(uint2));
	EXPECT_EQ(toSet<TypeSet>(int8, uint8), getSuperTypes(uint4));
	EXPECT_EQ(toSet<TypeSet>(uintI), getSuperTypes(uint8));
	EXPECT_EQ(toSet<TypeSet>(intI), getSuperTypes(uintI));
	EXPECT_EQ(toSet<TypeSet>(), getSuperTypes(intG));

}

TEST(TypeUtils, RealSuperTypes) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	TypePtr real4 = basic.getFloat();
	TypePtr real8 = basic.getDouble();
	TypePtr realI = basic.getRealInf();
	TypePtr realG = basic.getRealGen();

	// check each super-type relation
	EXPECT_EQ(toSet<TypeSet>(real8), getSuperTypes(real4));
	EXPECT_EQ(toSet<TypeSet>(realI), getSuperTypes(real8));
	EXPECT_EQ(toSet<TypeSet>(), getSuperTypes(realI));
	EXPECT_EQ(toSet<TypeSet>(), getSuperTypes(realG));

}

bool isNotSubTypeOf(const TypePtr& a, const TypePtr& b) {
	return !isSubTypeOf(a,b);
}

TEST(TypeUtils, IsSubTypeOf) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	TypePtr int2 = basic.getInt2();
	TypePtr int8 = basic.getInt8();
	TypePtr uint2 = basic.getUInt2();
	TypePtr uint8 = basic.getUInt8();
	TypePtr real8 = basic.getDouble();
	TypePtr intI = basic.getIntInf();
	TypePtr uintI = basic.getUIntInf();

	EXPECT_PRED2(isSubTypeOf, int2, int2);
	EXPECT_PRED2(isSubTypeOf, int8, int8);

	EXPECT_PRED2(isSubTypeOf, int2, int8);
	EXPECT_PRED2(isSubTypeOf, uint2, int8);
	EXPECT_PRED2(isSubTypeOf, int2, intI);
	EXPECT_PRED2(isSubTypeOf, uint2, intI);

	EXPECT_PRED2(isNotSubTypeOf, int2, uint2);
	EXPECT_PRED2(isNotSubTypeOf, uint2, int2);
	EXPECT_PRED2(isNotSubTypeOf, real8, intI);
	EXPECT_PRED2(isNotSubTypeOf, int2, uintI);

	// check some vector types
	TypePtr elemA = builder.genericType("A");
	TypePtr elemB = builder.genericType("B");

	TypePtr vecA2 = builder.vectorType(elemA, builder.concreteIntTypeParam(2));
	TypePtr vecA4 = builder.vectorType(elemA, builder.concreteIntTypeParam(4));
	TypePtr vecB2 = builder.vectorType(elemB, builder.concreteIntTypeParam(2));
	TypePtr vecB4 = builder.vectorType(elemB, builder.concreteIntTypeParam(4));

	TypePtr arrA1 = builder.arrayType(elemA);
	TypePtr arrA2 = builder.arrayType(elemA, builder.concreteIntTypeParam(2));
	TypePtr arrB1 = builder.arrayType(elemB);
	TypePtr arrB2 = builder.arrayType(elemB, builder.concreteIntTypeParam(2));

	EXPECT_PRED2(isSubTypeOf, vecA2, vecA2);
	EXPECT_PRED2(isSubTypeOf, vecA4, vecA4);
	EXPECT_PRED2(isSubTypeOf, arrA1, arrA1);
	EXPECT_PRED2(isSubTypeOf, arrA2, arrA2);

	EXPECT_PRED2(isSubTypeOf, vecA2, arrA1);
	EXPECT_PRED2(isSubTypeOf, vecA4, arrA1);

	EXPECT_PRED2(isNotSubTypeOf, vecA2, arrA2);
	EXPECT_PRED2(isNotSubTypeOf, vecA2, arrB1);
}

//TEST(TypeUtils, IsSubTypeOfTypeVariable) {
//
//	NodeManager manager;
//	IRBuilder builder(manager);
//	const lang::BasicGenerator& basic = manager.getLangBasic();
//
//	TypePtr typeA = builder.genericType("A");
//	TypePtr int2 = basic.getInt2();
//
//	TypePtr varA = builder.typeVariable("a");
//	TypePtr varB = builder.typeVariable("b");
//
//	EXPECT_PRED2(isSubTypeOf, varA, varA);
//	EXPECT_PRED2(isSubTypeOf, varA, varB);
//	EXPECT_PRED2(isSubTypeOf, varB, varA);
//	EXPECT_PRED2(isSubTypeOf, varB, varB);
//
//	EXPECT_PRED2(isSubTypeOf, int2, varA);
//	EXPECT_PRED2(isSubTypeOf, varA, int2);
//
//	EXPECT_PRED2(isSubTypeOf, int2, varA);
//	EXPECT_PRED2(isSubTypeOf, varA, int2);
//
//	EXPECT_PRED2(isSubTypeOf, typeA, varA);
//	EXPECT_PRED2(isSubTypeOf, varA, typeA);
//}

TEST(TypeUtils, IsSubTypeOfFunctionType) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	TypePtr int1 = basic.getInt1();
	TypePtr int2 = basic.getInt2();
	TypePtr int4 = basic.getInt4();
	TypePtr int8 = basic.getInt8();


	TypePtr funA = builder.functionType(toVector<TypePtr>(), int2);
	TypePtr funB = builder.functionType(toVector<TypePtr>(), int8);

	EXPECT_EQ("(()->int<2>)", toString(*funA));
	EXPECT_EQ("(()->int<8>)", toString(*funB));

	EXPECT_PRED2(isSubTypeOf, funA, funB);
	EXPECT_PRED2(isNotSubTypeOf, funB, funA);
	EXPECT_PRED2(isSubTypeOf, funA, funA);
	EXPECT_PRED2(isSubTypeOf, funB, funB);

	funA = builder.functionType(toVector(int4), int2);
	funB = builder.functionType(toVector(int2), int2);

	EXPECT_EQ("((int<4>)->int<2>)", toString(*funA));
	EXPECT_EQ("((int<2>)->int<2>)", toString(*funB));

	EXPECT_PRED2(isSubTypeOf, funA, funB);
	EXPECT_PRED2(isNotSubTypeOf, funB, funA);
	EXPECT_PRED2(isSubTypeOf, funA, funA);
	EXPECT_PRED2(isSubTypeOf, funB, funB);


	funA = builder.functionType(toVector(int4,int2,int8), int4);
	funB = builder.functionType(toVector(int4,int1,int4), int8);

	EXPECT_EQ("((int<4>,int<2>,int<8>)->int<4>)", toString(*funA));
	EXPECT_EQ("((int<4>,int<1>,int<4>)->int<8>)", toString(*funB));

	EXPECT_PRED2(isSubTypeOf, funA, funB);
	EXPECT_PRED2(isNotSubTypeOf, funB, funA);
	EXPECT_PRED2(isSubTypeOf, funA, funA);
	EXPECT_PRED2(isSubTypeOf, funB, funB);

	// -- plain function types vs. closure types

	funA = builder.functionType(toVector(int4), int2, true);
	funB = builder.functionType(toVector(int4), int2, false);

	EXPECT_EQ("((int<4>)->int<2>)", toString(*funA));
	EXPECT_EQ("((int<4>)=>int<2>)", toString(*funB));

	EXPECT_PRED2(isSubTypeOf, funA, funB);
	EXPECT_PRED2(isNotSubTypeOf, funB, funA);
	EXPECT_PRED2(isSubTypeOf, funA, funA);
	EXPECT_PRED2(isSubTypeOf, funB, funB);


	funA = builder.functionType(toVector(int4), int2, true);
	funB = builder.functionType(toVector(int2), int2, false);

	EXPECT_EQ("((int<4>)->int<2>)", toString(*funA));
	EXPECT_EQ("((int<2>)=>int<2>)", toString(*funB));

	EXPECT_PRED2(isSubTypeOf, funA, funB);
	EXPECT_PRED2(isNotSubTypeOf, funB, funA);
	EXPECT_PRED2(isSubTypeOf, funA, funA);
	EXPECT_PRED2(isSubTypeOf, funB, funB);
}

TEST(TypeUtils, IsSubTypeOfClassType) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;

	// create a type hierarchy
	TypePtr A = builder.parseType("struct { int a; }");
	symbols["A"] = A;

	TypePtr B = builder.parseType("struct : A { int b; }", symbols);
	symbols["B"] = B;

	TypePtr C = builder.parseType("struct : B { int c; }", symbols);
	symbols["C"] = C;

	TypePtr D = builder.parseType("D");
	symbols["D"] = D;

	TypePtr E = builder.parseType("E : D", symbols);
	symbols["E"] = E;

	TypePtr F = builder.parseType("F : B, E", symbols);

	// now check the relations

	// reflexive
	EXPECT_PRED2(isSubTypeOf, A, A);
	EXPECT_PRED2(isSubTypeOf, B, B);
	EXPECT_PRED2(isSubTypeOf, C, C);
	EXPECT_PRED2(isSubTypeOf, D, D);
	EXPECT_PRED2(isSubTypeOf, E, E);

	// direct relations
	EXPECT_PRED2(isSubTypeOf, B, A);
	EXPECT_PRED2(isSubTypeOf, C, B);
	EXPECT_PRED2(isSubTypeOf, E, D);
	EXPECT_PRED2(isSubTypeOf, F, B);
	EXPECT_PRED2(isSubTypeOf, F, E);

	// indirect relations
	EXPECT_PRED2(isSubTypeOf, C, A);
	EXPECT_PRED2(isSubTypeOf, F, A);
	EXPECT_PRED2(isSubTypeOf, F, D);

	// non-existing sub-type relations (subset)
	EXPECT_PRED2(isNotSubTypeOf, A, B);
	EXPECT_PRED2(isNotSubTypeOf, A, C);
	EXPECT_PRED2(isNotSubTypeOf, D, E);

}


TEST(TypeUtils, JoinMeetTypeComputation) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	// construct some types to test the mechanisms
	TypePtr int4 = basic.getInt4();
	TypePtr uint4 = basic.getUInt4();

	EXPECT_EQ(int4, getSmallestCommonSuperType(int4, int4));
	EXPECT_EQ(int4, getBiggestCommonSubType(int4, int4));

	TypePtr join = getSmallestCommonSuperType(int4, uint4);
	EXPECT_EQ("int<8>", toString(*join));
	TypePtr meet = getBiggestCommonSubType(int4, uint4);
	EXPECT_EQ("uint<2>", toString(*meet));

	EXPECT_PRED2(isSubTypeOf, int4, join);
	EXPECT_PRED2(isSubTypeOf, uint4, join);

	EXPECT_PRED2(isSubTypeOf, meet, int4);
	EXPECT_PRED2(isSubTypeOf, meet, uint4);

	// test with vectors
	TypePtr vectorA = builder.vectorType(int4, builder.concreteIntTypeParam(12));
	TypePtr vectorB = builder.vectorType(int4, builder.concreteIntTypeParam(14));

	join = getSmallestCommonSuperType(vectorA, vectorB);
	EXPECT_TRUE(join);
	EXPECT_EQ("AP(array<int<4>,1>)", toString(join));
	EXPECT_FALSE(getBiggestCommonSubType(vectorA, vectorB));

	EXPECT_EQ(vectorA, getSmallestCommonSuperType(vectorA, vectorA));
	EXPECT_EQ(vectorA, getBiggestCommonSubType(vectorA, vectorA));

	// test some functions
	TypePtr funA = builder.functionType(toVector(int4), int4);
	TypePtr funB = builder.functionType(toVector(uint4), uint4);

	join = getSmallestCommonSuperType(funA, funB);
	EXPECT_EQ("((uint<2>)->int<8>)", toString(*join));
	meet = getBiggestCommonSubType(funA, funB);
	EXPECT_EQ("((int<8>)->uint<2>)", toString(*meet));

	EXPECT_PRED2(isSubTypeOf, funA, join);
	EXPECT_PRED2(isSubTypeOf, funB, join);

	EXPECT_PRED2(isSubTypeOf, meet, funA);
	EXPECT_PRED2(isSubTypeOf, meet, funB);

}


TEST(TypeUtils, VariableSubstitutionBug) {

	// The basis of this Test case is the following type checker error:
	//		MSG: Invalid return type -
	// 				expected: uint<a>
	// 				actual:   uint<4>
	// 				function type: ((vector<'elem,l>,'res,(('elem,'res)->'res))->'res)
	//
	// This error occurs when the function is invoked using a literal as
	// its second argument and a generic integer operation is it's last.
	// The expected return type should be consistent with the typeo of the
	// second argument.

	// reconstruct test case
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr intType = manager.getLangBasic().getUInt4();
	TypePtr vectorType = builder.vectorType(intType, builder.concreteIntTypeParam(8));
	TypePtr funType = builder.parseType("(vector<'elem,#l>,'res,('elem,'res)->'res)->'res");
	EXPECT_TRUE(funType);

	EXPECT_EQ(NT_VectorType, vectorType->getNodeType());
	EXPECT_EQ(NT_VectorType, static_pointer_cast<const FunctionType>(funType)->getParameterTypes()[0]->getNodeType());

	LiteralPtr fun = Literal::get(manager, funType, "fun");
	LiteralPtr vector = Literal::get(manager, vectorType, "x");
	LiteralPtr zero = Literal::get(manager, intType, "0");
	LiteralPtr op = manager.getLangBasic().getUnsignedIntAdd();

	ExpressionPtr call = builder.callExpr(intType, fun, vector, zero, op);

	// run check
	CheckPtr callCheck = make_check<checks::CallExprTypeCheck>();
	auto res = check(call, callCheck);

	// there shouldn't be any errors
	EXPECT_TRUE(res.empty());
}

TEST(TypeUtils, ReturnTypeBug) {

	// MSG: Invalid return type
	//		- expected: vector<'res,#l>, actual: vector<uint<4>,3>
	//		- function type: ((vector<'elem,#l>,vector<'elem,#l>)->vector<'res,#l>)
	//
	// => occurs in conjunction with the vector.pointwise operator

	// build a pointwise sum ...
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr uint4 = manager.getLangBasic().getUInt4();
	ExpressionPtr add = manager.getLangBasic().getOperator(uint4, lang::BasicGenerator::Add);
	ExpressionPtr pointwise = builder.callExpr(manager.getLangBasic().getVectorPointwise(), add);

//	EXPECT_EQ("", toString(*add->getType()));
//	EXPECT_EQ("", toString(*pointwise->getType()));

}

TEST(TypeUtils, AutoTypeInference_ArrayInitCall) {

	// The Bug:
	// 		Unable to deduce return type for call to function of type
	//		(('elem,uint<8>)->array<'elem,1>) using arguments
	//		ref<struct<top:ref<array<ref<rec 'elem.{'elem=struct<value:ref<int<4>>,next:ref<array<ref<'elem>,1>>>}>,1>>>>, uint<8>

	// The reason:
	// 		within the element type the same variable 'elem is used as within the
	//		function type => leading to a mess

	// The fix:
	//		before trying to match the given arguments to the function parameters
	//		all type variables are replaced by fresh variables.

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	// get element type
	TypePtr elementType = builder.genericType("Set", toVector<TypePtr>(builder.typeVariable("elem")));

	// create the call
	ExpressionPtr element = builder.getTypeLiteral(elementType);
	ExpressionPtr size = builder.literal(basic.getUInt8(), "15");
	ExpressionPtr res = builder.callExpr(basic.getArrayCreate1D(), element, size);

	// check infered type
	TypePtr resType = builder.arrayType(elementType, builder.concreteIntTypeParam(1));
	EXPECT_EQ(*resType, *res->getType());

}


TEST(TypeUtils, ListUnification) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr typeA = builder.genericType("A");
	TypePtr typeB = builder.genericType("B");

	TypePtr varX = builder.typeVariable("X");
	TypePtr varY = builder.typeVariable("Y");

	TypePtr genAX = builder.genericType("T", toVector(typeA, varX));
	TypePtr genXA = builder.genericType("T", toVector(varX, typeA));

	TypePtr genYY = builder.genericType("T", toVector(varY, varY));


	auto res = unifyAll(manager, toVector(typeA, typeB));
	EXPECT_FALSE(res);

	EXPECT_TRUE(unify(manager, varX, varY));
	res = unifyAll(manager, toVector(varX, varY));
	EXPECT_TRUE(res);

	res = unifyAll(manager, toVector(genAX, genXA));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(res->applyTo(genAX), res->applyTo(genXA));

	res = unifyAll(manager, toVector(genAX, genXA, genYY));
	EXPECT_TRUE(res);
	if (res) EXPECT_EQ(res->applyTo(genAX), res->applyTo(genXA));
	if (res) EXPECT_EQ(res->applyTo(genAX), res->applyTo(genYY));

	EXPECT_FALSE(unifyAll(manager, toVector(genAX, genXA, varX)));
}

TEST(TypeUtils, ListUnificationBug1) {

	// The Problem:
	//		the list [AP('a),AP(uint<4>),AP(int<4>)] can apparently be unified to [AP(uint<4>)]
	// 		=> this should not be the case
	//
	// Cause of the problem:
	//		After each step, the current unifier was not applied on the next type / the current unified type.
	//
	// Fix:
	//		This is now down.

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	TypePtr varA = builder.typeVariable("a");
	TypePtr uint4 = basic.getUInt4();
	TypePtr int4 = basic.getInt4();

	auto list = toVector(varA, uint4, int4);
	EXPECT_EQ("[AP('a),AP(uint<4>),AP(int<4>)]", toString(list));

	auto res = unifyAll(manager, list);
	EXPECT_FALSE(res);
}

TEST(TypeUtils, IdenticalIntTypeParameterVariables) {

	//
	// The Problem:
	//		The Type T<#a,#a> can apparently be unified with T<1,2> using {#a->1}
	//
	// Cause of the problem:
	//		The substitution derived from the first parameter (#a->1) is not applied to the
	//		remaining parameters of the same type.
	//
	// Fix:
	//		Now the substitution is applied.
	//

	NodeManager manager;
	IRBuilder builder(manager);

	IntTypeParamPtr varA = builder.variableIntTypeParam('a');
	TypePtr gen = builder.genericType("T", toVector<TypePtr>(), toVector(varA, varA));
	EXPECT_EQ("T<#a,#a>", toString(*gen));

	IntTypeParamPtr param1 = builder.concreteIntTypeParam(1);
	IntTypeParamPtr param2 = builder.concreteIntTypeParam(2);
	TypePtr concrete = builder.genericType("T", toVector<TypePtr>(), toVector(param1, param2));
	EXPECT_EQ("T<1,2>", toString(*concrete));

	// this should not work ...
	auto unifier = unify(manager, gen, concrete);
	EXPECT_FALSE(unifier);
	if (unifier) EXPECT_EQ("", toString(*unifier));

	// ... but this should
	concrete = builder.genericType("T", toVector<TypePtr>(), toVector(param1, param1));
	EXPECT_TRUE(unify(manager, gen, concrete));
	EXPECT_TRUE(unify(manager, concrete, gen));
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


} // end namespace core
} // end namespace insieme

