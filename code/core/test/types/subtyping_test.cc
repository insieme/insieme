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

#include "insieme/core/types/subtyping.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace core {
namespace types {

	using namespace utils::set;

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

	TEST(TypeUtils, IsSubTypeOfRefType) {

		NodeManager manager;
		IRBuilder builder(manager);
		const auto& basic = manager.getLangBasic();

		TypePtr any = basic.getAny();

		TypePtr int4 = basic.getInt4();
		TypePtr int8 = basic.getInt8();

		TypePtr real4 = basic.getReal4();
		TypePtr real8 = basic.getReal8();

		TypePtr refAny = builder.refType(any);
		TypePtr refInt4 = builder.refType(int4);
		TypePtr refInt8 = builder.refType(int8);
		TypePtr refReal4 = builder.refType(real4);
		TypePtr refReal8 = builder.refType(real8);

		EXPECT_PRED2(isSubTypeOf, int4, int8);
		EXPECT_PRED2(isSubTypeOf, real4, real8);

		EXPECT_PRED2(isNotSubTypeOf, refInt4, refInt8);
		EXPECT_PRED2(isNotSubTypeOf, refReal4, refReal8);

		EXPECT_PRED2(isSubTypeOf, refInt4, refAny);
		EXPECT_PRED2(isSubTypeOf, refInt8, refAny);
		EXPECT_PRED2(isSubTypeOf, refReal4, refAny);
		EXPECT_PRED2(isSubTypeOf, refReal8, refAny);

		EXPECT_PRED2(isNotSubTypeOf, refAny, refInt4);
		EXPECT_PRED2(isNotSubTypeOf, refAny, refInt8);
		EXPECT_PRED2(isNotSubTypeOf, refAny, refReal4);
		EXPECT_PRED2(isNotSubTypeOf, refAny, refReal8);

		// check arrays - vector relation
		TypePtr refArray  = builder.parseType("ref<array<int<4>,1>>");
		TypePtr refVector = builder.parseType("ref<vector<int<4>,50>>");

		EXPECT_PRED2(isSubTypeOf, refVector, refArray);
		EXPECT_PRED2(isNotSubTypeOf, refArray, refVector);

		TypePtr refArray2 = builder.parseType("ref<array<int<8>,1>>");
		EXPECT_PRED2(isNotSubTypeOf, refArray, refArray2);
		EXPECT_PRED2(isNotSubTypeOf, refArray, refArray2);

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

		funA = builder.functionType(toVector(int4), int2, FK_PLAIN);
		funB = builder.functionType(toVector(int4), int2, FK_CLOSURE);

		EXPECT_EQ("((int<4>)->int<2>)", toString(*funA));
		EXPECT_EQ("((int<4>)=>int<2>)", toString(*funB));

		EXPECT_PRED2(isSubTypeOf, funA, funB);
		EXPECT_PRED2(isNotSubTypeOf, funB, funA);
		EXPECT_PRED2(isSubTypeOf, funA, funA);
		EXPECT_PRED2(isSubTypeOf, funB, funB);


		funA = builder.functionType(toVector(int4), int2, FK_PLAIN);
		funB = builder.functionType(toVector(int2), int2, FK_CLOSURE);

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

	TEST(TypeUtils, RecursiveTypes) {

		// To be tested: a recursive type should be equivalent to its unrolled version
		NodeManager mgr;
		IRBuilder builder(mgr);

		// create a recursive type
		auto type = builder.parseType("let t = struct { A a; ref<t> next; } in t").as<RecTypePtr>();
		EXPECT_TRUE(type);

		EXPECT_PRED2(isSubTypeOf, type, type);
		EXPECT_PRED2(isSubTypeOf, type, type->unroll());
		EXPECT_PRED2(isSubTypeOf, type->unroll(), type);


		// also a mutual recursive type
		type = builder.parseType("let t,s = struct { A a; ref<s> next; }, struct { B b; ref<t> next; } in t").as<RecTypePtr>();
		EXPECT_TRUE(type);

		EXPECT_PRED2(isSubTypeOf, type, type);
		EXPECT_PRED2(isSubTypeOf, type, type->unroll());
		EXPECT_PRED2(isSubTypeOf, type->unroll(), type);
	}

	TEST(TypeUtils, RefAny) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		// the simple ref any
		auto refAny = builder.parseType("ref<any>");

		EXPECT_PRED2(isSubTypeOf, builder.parseType("ref<A>"), refAny);
		EXPECT_PRED2(isSubTypeOf, builder.parseType("ref<int<4>>"), refAny);
		EXPECT_PRED2(isSubTypeOf, builder.parseType("ref<ref<int<4>>>"), refAny);

		EXPECT_PRED2(isNotSubTypeOf, builder.parseType("A"), refAny);
		EXPECT_PRED2(isNotSubTypeOf, builder.parseType("int<4>"), refAny);

		// a more nested version
		refAny = builder.parseType("ref<ref<any>>");

		EXPECT_PRED2(isSubTypeOf, builder.parseType("ref<ref<A>>"), refAny);
		EXPECT_PRED2(isSubTypeOf, builder.parseType("ref<ref<int<4>>>"), refAny);
		EXPECT_PRED2(isSubTypeOf, builder.parseType("ref<ref<ref<int<4>>>>"), refAny);

	}

} // end namespace types
} // end namespace core
} // end namespace insieme
