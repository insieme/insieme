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
#include <algorithm>
#include <stdexcept>
#include <vector>

#include "types.h"
#include "container_utils.h"
#include "functional_utils.h"

using std::vector;

namespace insieme {
namespace core {

template<typename PT>
void basicTypeTests(PT type, bool concrete, bool functional, vector<TypePtr> children = vector<TypePtr>());

TEST(TypeTest, TypeManager ) {

	// create type manager
	TypeManager manager;

	// get a type
	GenericTypePtr typeA1 = GenericType::get(manager, "A");
	GenericTypePtr typeA2 = GenericType::get(manager, "A");
	GenericTypePtr typeB = GenericType::get(manager, "B");

	EXPECT_TRUE (!!typeA1);
	EXPECT_TRUE (!!typeA2);
	EXPECT_TRUE (!!typeB);

	EXPECT_TRUE ( typeA1 == typeA2 );
	EXPECT_FALSE ( typeA1 == typeB );

}

TEST(TypeTest, TypeManagerGetAllBug ) {

	TypeManager manager;

	TypePtr typeA = GenericType::get(manager, "A");
	TypePtr typeB = GenericType::get(manager, "B");
	TypePtr typeR = GenericType::get(manager, "R");

	Identifier ident = "funA";
	TupleType::ElementTypeList list;
	list.push_back(typeA);
	list.push_back(typeB);
	TypePtr tuple = TupleType::get(manager, list);

	FunctionTypePtr funType = FunctionType::get(manager, tuple, typeR);
}

TEST(TypeTest, MultipleTypeManager ) {

	// create type manager
	TypeManager managerA;
	TypeManager managerB;

	// create a complex type in manager A
	GenericTypePtr baseA = GenericType::get(managerA, "A");
	GenericTypePtr baseB = GenericType::get(managerA, "B");

	vector<TypePtr> typesA;
	typesA.push_back(baseA);
	typesA.push_back(baseB);
	GenericTypePtr complexA = GenericType::get(managerA, "C", typesA);

	vector<TypePtr> typesB;
	typesB.push_back(complexA);
	typesB.push_back(baseB);
	GenericTypePtr rootA = GenericType::get(managerA, "R", typesB);

	EXPECT_EQ ( 4, managerA.size() );
	EXPECT_EQ ( 0, managerB.size() );

	// try to obtain the same type from the other manager
	GenericTypePtr rootB = GenericType::get(managerB, "R", typesB);
	EXPECT_EQ ( 4, managerA.size() );
	EXPECT_EQ ( 4, managerB.size() );

	EXPECT_NE ( rootA, rootB );
	EXPECT_EQ ( *rootA, *rootB );
}



TEST(TypeTest, Type_AllConcrete) {

	// create a type manager for this test
	TypeManager manager;

	// create some variable types
	TypeVariablePtr varA = TypeVariable::get(manager, "'alpha");
	EXPECT_FALSE ( varA->isConcrete() );
	EXPECT_FALSE ( Type::allConcrete(toVector<TypePtr>(varA)));
	TypeVariablePtr varB = TypeVariable::get(manager, "'beta");
	EXPECT_FALSE ( varB->isConcrete() );
	EXPECT_FALSE ( Type::allConcrete(toVector<TypePtr>(varB)));

	// create some concrete types
	GenericTypePtr typeA = GenericType::get(manager, "A");
	EXPECT_TRUE ( typeA->isConcrete() );
	EXPECT_TRUE ( Type::allConcrete(toVector<TypePtr>(typeA)));
	GenericTypePtr typeB = GenericType::get(manager, "B");
	EXPECT_TRUE ( typeB->isConcrete() );
	EXPECT_TRUE ( Type::allConcrete(toVector<TypePtr>(typeB)));

	// create a type list
	vector<TypePtr> types;

	// first var, then concrete
	types.clear();
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(varA);
	EXPECT_FALSE (Type::allConcrete(types));
	types.push_back(typeA);
	EXPECT_FALSE (Type::allConcrete(types));

	// first concrete, then var
	types.clear();
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(typeB);
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(varA);
	EXPECT_FALSE (Type::allConcrete(types));

	// multiple concrete
	types.clear();
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(typeA);
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(typeB);
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(varA);
	EXPECT_FALSE (Type::allConcrete(types));

	// multiple variable
	types.clear();
	EXPECT_TRUE (Type::allConcrete(types));
	types.push_back(varA);
	EXPECT_FALSE (Type::allConcrete(types));
	types.push_back(varB);
	EXPECT_FALSE (Type::allConcrete(types));
	types.push_back(typeA);
	EXPECT_FALSE (Type::allConcrete(types));

}


TEST(TypeTest, GenericType_AllConcrete) {

	// create some integer type parameter
	IntTypeParam var = IntTypeParam::getVariableIntParam('p');
	IntTypeParam con = IntTypeParam::getConcreteIntParam(12);
	IntTypeParam inf = IntTypeParam::getInfiniteIntParam();

	// check basic
	EXPECT_FALSE ( var.isConcrete() );
	EXPECT_TRUE ( con.isConcrete() );
	EXPECT_TRUE ( inf.isConcrete() );

	// check vectors if entries
	vector<IntTypeParam> params;
	EXPECT_TRUE ( IntTypeParam::allConcrete(params) );
	params.push_back(var);
	EXPECT_FALSE ( IntTypeParam::allConcrete(params) );

	params.clear();
	EXPECT_TRUE ( IntTypeParam::allConcrete(params) );
	params.push_back(con);
	EXPECT_TRUE ( IntTypeParam::allConcrete(params) );
	params.push_back(inf);
	EXPECT_TRUE ( IntTypeParam::allConcrete(params) );
	params.push_back(var);
	EXPECT_FALSE ( IntTypeParam::allConcrete(params) );
	params.push_back(con);
	EXPECT_FALSE ( IntTypeParam::allConcrete(params) );
}

TEST(TypeTest, GenericType) {

	// create a type manager for this test
	TypeManager manager;

	// some empty lists (required as arguments)
	vector<TypePtr> emptyPtr;
	vector<IntTypeParam> emptyPar;

	// create some variable types
	TypeVariablePtr varA = TypeVariable::get(manager, "alpha");
	EXPECT_FALSE ( varA->isConcrete() );
	TypeVariablePtr varB = TypeVariable::get(manager, "beta");
	EXPECT_FALSE ( varB->isConcrete() );

	// create some concrete types
	GenericTypePtr typeA = GenericType::get(manager, "A");
	EXPECT_TRUE ( typeA->isConcrete() );
	EXPECT_EQ ( "A" , typeA->getName() );
	GenericTypePtr typeB = GenericType::get(manager, "B");
	EXPECT_TRUE ( typeB->isConcrete() );
	EXPECT_EQ ( "B" , typeB->getName() );

	// create complex types
	GenericTypePtr typeC = GenericType::get(manager, "C", toVector<TypePtr>(varA));
	EXPECT_FALSE ( typeC->isConcrete() );
	EXPECT_EQ ( "C<'alpha>" , typeC->getName() );
	GenericTypePtr typeD = GenericType::get(manager, "D", emptyPtr, toVector(IntTypeParam::getVariableIntParam('a')));
	EXPECT_FALSE ( typeD->isConcrete() );
	EXPECT_EQ ( "D<a>" , typeD->getName() );

	// create complex type with multiple parameter
	vector<TypePtr> typeListA;
	typeListA.push_back(typeA);
	typeListA.push_back(typeB);
	GenericTypePtr typeE = GenericType::get(manager, "E", typeListA);
	EXPECT_TRUE ( typeE->isConcrete() );
	EXPECT_EQ ( "E<A,B>" , typeE->getName() );

	// create type with base type
	GenericTypePtr typeF = GenericType::get(manager, "F", emptyPtr, emptyPar, typeA );
	EXPECT_TRUE ( typeF->isConcrete() );
	EXPECT_EQ ( "F" , typeF->getName() );

	GenericTypePtr typeG = GenericType::get(manager, "G", typeListA, toVector(IntTypeParam::getConcreteIntParam(12)), typeA );
	EXPECT_TRUE ( typeG->isConcrete() );
	EXPECT_EQ ( "G<A,B,12>" , typeG->getName() );

	// perform general test cases
	{
		SCOPED_TRACE ( "typeA" );
		basicTypeTests(typeA, true, false);
	}{
		SCOPED_TRACE ( "typeB" );
		basicTypeTests(typeB, true, false);
	}{
		SCOPED_TRACE ( "typeC" );
		basicTypeTests(typeC, false, false, toVector<TypePtr>(varA));
	}{
		SCOPED_TRACE ( "typeD" );
		basicTypeTests(typeD, false, false);
	}{
		SCOPED_TRACE ( "typeE" );
		basicTypeTests(typeE, true, false, typeListA);
	}{
		SCOPED_TRACE ( "typeF" );
		basicTypeTests(typeF, true, false, toVector<TypePtr>(typeA));
	}{
		SCOPED_TRACE ( "typeG" );
		vector<TypePtr> list(typeListA);
		list.push_back(typeA);
		basicTypeTests(typeG, true, false, list);
	}

	// selected equality checks (not after copy)
	EXPECT_FALSE ( varA == varB );
	EXPECT_FALSE ( typeA == typeB );
	EXPECT_FALSE ( varA == typeB );
	EXPECT_TRUE ( varA == varA );
	EXPECT_TRUE ( typeA == typeA );

	// create list of all types
	vector<TypePtr> types;
	types.push_back(typeA);
	types.push_back(typeB);
	types.push_back(typeC);
	types.push_back(typeD);
	types.push_back(typeE);
	types.push_back(typeF);

	// check whether equality is working properly
	for (unsigned i=0; i<types.size(); i++) {
		for (unsigned j=0; j<types.size(); j++) {
			EXPECT_EQ ( i == j , types[i] == types[j]);
		}
	}
}


TEST(TypeTest, TypeVariable) {

	TypeManager manager;
	TypeVariablePtr varTypeA = TypeVariable::get(manager, "alpha");
	TypeVariablePtr varTypeB = TypeVariable::get(manager, "beta");

	// check name
	EXPECT_EQ ( "'alpha", varTypeA->getName() );
	EXPECT_EQ ( "'beta", varTypeB->getName() );
	EXPECT_EQ ( "'alpha", varTypeA->toString() );
	EXPECT_EQ ( "'beta", varTypeB->toString() );

	// perform basic type tests
	basicTypeTests(varTypeA, false, false);
}

TEST(TypeTest, TupleType) {

	TypeManager manager;

	vector<TypePtr> subTypesA;
	vector<TypePtr> subTypesB;
	subTypesB.push_back(GenericType::get(manager, "dummy1"));
	subTypesB.push_back(GenericType::get(manager, "dummy2"));

	TupleTypePtr typeA = TupleType::get(manager, subTypesA);
	TupleTypePtr typeB = TupleType::get(manager, subTypesB);


	// perform basic type tests
	basicTypeTests(typeA, true, false, subTypesA);
	basicTypeTests(typeB, true, false, subTypesB);

}

TEST(TypeTest, FunctionType) {

	TypeManager manager;

	TypePtr argumentA = GenericType::get(manager, "dummyA");
	TypePtr argumentB = TypeVariable::get(manager, "alpha");
	TypePtr resultA = GenericType::get(manager, "returnA");
	TypePtr resultB = GenericType::get(manager, "returnB");

	FunctionTypePtr funTypeA = FunctionType::get(manager, argumentA, resultA);
	FunctionTypePtr funTypeB = FunctionType::get(manager, argumentB, resultB);

	EXPECT_EQ ( "(dummyA->returnA)" , funTypeA->getName() );
	EXPECT_EQ ( "('alpha->returnB)" , funTypeB->getName() );

	vector<TypePtr> subTypesA;
	subTypesA.push_back(argumentA);
	subTypesA.push_back(resultA);

	vector<TypePtr> subTypesB;
	subTypesB.push_back(argumentB);
	subTypesB.push_back(resultB);

	// perform basic type tests
	basicTypeTests(funTypeA, true, true, subTypesA);
	basicTypeTests(funTypeB, true, true, subTypesB);
}

TEST(TypeTest, StructType) {

	TypeManager manager;

	Identifier identA("a");
	Identifier identB("b");

	StructType::Entries entriesA;
	entriesA.push_back(StructType::Entry(identA, GenericType::get(manager, "A")));
	entriesA.push_back(StructType::Entry(identB, GenericType::get(manager, "B")));

	StructTypePtr structA = StructType::get(manager, entriesA);
	EXPECT_EQ ( "struct<a:A,b:B>", structA->getName() );

	StructType::Entries entriesB;
	StructTypePtr structB = StructType::get(manager, entriesB);
	EXPECT_EQ ( "struct<>", structB->getName() );

	StructType::Entries entriesC;
	entriesC.push_back(StructType::Entry(identA, TypeVariable::get(manager,"alpha")));
	entriesC.push_back(StructType::Entry(identB, GenericType::get(manager, "B")));
	StructTypePtr structC = StructType::get(manager, entriesC);
	EXPECT_EQ ( "struct<a:'alpha,b:B>", structC->getName() );

	// test for elements with same name
	StructType::Entries entriesD;
	entriesD.push_back(StructType::Entry(identA, GenericType::get(manager,"A")));
	entriesD.push_back(StructType::Entry(identB, GenericType::get(manager,"A")));
	entriesD.push_back(StructType::Entry(identA, GenericType::get(manager,"A")));
	EXPECT_THROW ( StructType::get(manager, entriesD), std::invalid_argument );

	// .. same type should not be a problem
	entriesD.pop_back();
	EXPECT_NO_THROW ( StructType::get(manager, entriesD) );

	// perform basic type tests
	vector<TypePtr> typeListA;
	std::transform(entriesA.cbegin(), entriesA.cend(), back_inserter(typeListA),
		[](const StructType::Entry& cur) {
			return cur.second;
	});

	vector<TypePtr> typeListB;
	std::transform(entriesB.cbegin(), entriesB.cend(), back_inserter(typeListB),
		[](const StructType::Entry& cur) {
			return cur.second;
	});

	vector<TypePtr> typeListC;
	std::transform(entriesC.cbegin(), entriesC.cend(), back_inserter(typeListC),
		[](const StructType::Entry& cur) {
			return cur.second;
	});

	basicTypeTests(structA, true, false, typeListA);
	basicTypeTests(structB, true, false, typeListB);
	basicTypeTests(structC, false, false, typeListC);
}

TEST(TypeTest, UnionType) {

	TypeManager manager;

	Identifier identA("a");
	Identifier identB("b");

	UnionType::Entries entriesA;
	entriesA.push_back(UnionType::Entry(identA, GenericType::get(manager, "A")));
	entriesA.push_back(UnionType::Entry(identB, GenericType::get(manager, "B")));

	UnionTypePtr unionA = UnionType::get(manager, entriesA);
	EXPECT_EQ ( "union<a:A,b:B>", unionA->getName() );

	UnionType::Entries entriesB;
	UnionTypePtr unionB = UnionType::get(manager, entriesB);
	EXPECT_EQ ( "union<>", unionB->getName() );

	UnionType::Entries entriesC;
	entriesC.push_back(UnionType::Entry(identA, TypeVariable::get(manager,"alpha")));
	entriesC.push_back(UnionType::Entry(identB, GenericType::get(manager, "B")));
	UnionTypePtr unionC = UnionType::get(manager, entriesC);
	EXPECT_EQ ( "union<a:'alpha,b:B>", unionC->getName() );


	// perform basic type tests
	vector<TypePtr> typeListA;
	std::transform(entriesA.cbegin(), entriesA.cend(), back_inserter(typeListA),
		[](const UnionType::Entry& cur) {
			return cur.second;
	});

	vector<TypePtr> typeListB;
	std::transform(entriesB.cbegin(), entriesB.cend(), back_inserter(typeListB),
		[](const UnionType::Entry& cur) {
			return cur.second;
	});

	vector<TypePtr> typeListC;
	std::transform(entriesC.cbegin(), entriesC.cend(), back_inserter(typeListC),
		[](const UnionType::Entry& cur) {
			return cur.second;
	});

	basicTypeTests(unionA, true, false, typeListA);
	basicTypeTests(unionB, true, false, typeListB);
	basicTypeTests(unionC, false, false, typeListC);
}

TEST(TypeTest, ArrayType) {

	// create type manager and element types
	TypeManager manager;
	TypePtr elementTypeA = GenericType::get(manager,"A");
	TypePtr elementTypeB = TypeVariable::get(manager,"a");

	// obtain array types
	ArrayTypePtr arrayTypeA = ArrayType::get(manager, elementTypeA);
	ArrayTypePtr arrayTypeB = ArrayType::get(manager, elementTypeB, 3);

	// check names
	EXPECT_EQ ( "array<A,1>", arrayTypeA->getName() );
	EXPECT_EQ ( "array<'a,3>", arrayTypeB->getName() );

	// check element types
	EXPECT_EQ ( elementTypeA, arrayTypeA->getElementType() );
	EXPECT_EQ ( elementTypeB, arrayTypeB->getElementType() );

	// check dimensions
	EXPECT_EQ ( 1, arrayTypeA->getDimension());
	EXPECT_EQ ( 3, arrayTypeB->getDimension());

	// check remaining type properties
	basicTypeTests(arrayTypeA, true, false, toVector(elementTypeA));
	basicTypeTests(arrayTypeB, false, false, toVector(elementTypeB));
}

TEST(TypeTest, VectorType) {

	// create type manager and element types
	TypeManager manager;
	TypePtr elementTypeA = GenericType::get(manager,"A");
	TypePtr elementTypeB = TypeVariable::get(manager,"a");

	// obtain array types
	VectorTypePtr vectorTypeA = VectorType::get(manager, elementTypeA, 1);
	VectorTypePtr vectorTypeB = VectorType::get(manager, elementTypeB, 3);

	// check names
	EXPECT_EQ ( "vector<A,1>", vectorTypeA->getName() );
	EXPECT_EQ ( "vector<'a,3>", vectorTypeB->getName() );

	// check element types
	EXPECT_EQ ( elementTypeA, vectorTypeA->getElementType() );
	EXPECT_EQ ( elementTypeB, vectorTypeB->getElementType() );

	// check dimensions
	EXPECT_EQ ( 1, vectorTypeA->getSize());
	EXPECT_EQ ( 3, vectorTypeB->getSize());

	// check remaining type properties
	basicTypeTests(vectorTypeA, true, false, toVector(elementTypeA));
	basicTypeTests(vectorTypeB, false, false, toVector(elementTypeB));
}

TEST(TypeTest, ChannelType) {

	// create type manager and element types
	TypeManager manager;
	TypePtr elementTypeA = GenericType::get(manager,"A");
	TypePtr elementTypeB = TypeVariable::get(manager,"a");

	// obtain array types
	ChannelTypePtr channelTypeA = ChannelType::get(manager, elementTypeA, 1);
	ChannelTypePtr channelTypeB = ChannelType::get(manager, elementTypeB, 3);

	// check names
	EXPECT_EQ ( "channel<A,1>", channelTypeA->getName() );
	EXPECT_EQ ( "channel<'a,3>", channelTypeB->getName() );

	// check element types
	EXPECT_EQ ( elementTypeA, channelTypeA->getElementType() );
	EXPECT_EQ ( elementTypeB, channelTypeB->getElementType() );

	// check dimensions
	EXPECT_EQ ( 1, channelTypeA->getSize());
	EXPECT_EQ ( 3, channelTypeB->getSize());

	// check remaining type properties
	basicTypeTests(channelTypeA, true, false, toVector(elementTypeA));
	basicTypeTests(channelTypeB, false, false, toVector(elementTypeB));
}

TEST(TypeTest, RefType) {

	// create type manager and element types
	TypeManager manager;
	TypePtr elementTypeA = GenericType::get(manager,"A");
	TypePtr elementTypeB = TypeVariable::get(manager,"a");

	// obtain array types
	RefTypePtr refTypeA = RefType::get(manager, elementTypeA);
	RefTypePtr refTypeB = RefType::get(manager, elementTypeB);

	// check names
	EXPECT_EQ ( "ref<A>", refTypeA->getName() );
	EXPECT_EQ ( "ref<'a>", refTypeB->getName() );

	// check element types
	EXPECT_EQ ( elementTypeA, refTypeA->getElementType() );
	EXPECT_EQ ( elementTypeB, refTypeB->getElementType() );

	// check remaining type properties
	basicTypeTests(refTypeA, true, false, toVector(elementTypeA));
	basicTypeTests(refTypeB, false, false, toVector(elementTypeB));
}


TEST(TypeTest, IntTypeParam) {
#ifndef WIN32
	// test size limitation
	EXPECT_LE (sizeof(IntTypeParam), (std::size_t) 4);
#endif

	// test toString format
	IntTypeParam p12 = IntTypeParam::getConcreteIntParam(12);
	EXPECT_EQ (p12.toString(), "12");

	IntTypeParam inf = IntTypeParam::getInfiniteIntParam();
	EXPECT_EQ (inf.toString(), "Inf");

	IntTypeParam pvp = IntTypeParam::getVariableIntParam('p');
	EXPECT_EQ (pvp.toString(), "p");

	// test isConcrete()
	EXPECT_EQ (p12.isConcrete(), true);
	EXPECT_EQ (inf.isConcrete(), true);
	EXPECT_EQ (pvp.isConcrete(), false);

	// test == operator
	IntTypeParam params[] = {p12, inf, pvp};
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {
			EXPECT_EQ(params[i]==params[j], i==j);
		}
	}

	IntTypeParam p12b = IntTypeParam::getConcreteIntParam(12);
	EXPECT_TRUE (p12 == p12b);

	IntTypeParam pvpb = IntTypeParam::getVariableIntParam('p');
	EXPECT_TRUE (pvp == pvpb);

	IntTypeParam infb = IntTypeParam::getInfiniteIntParam();
	EXPECT_TRUE (inf == infb);
}

template<typename PT>
void basicTypeTests(PT type, bool concrete, bool functional, vector<TypePtr> children) {

	typedef typename PT::element_type T;

	// ------------ Type Ptr based tests -------------

	// check concrete flag
	EXPECT_EQ( concrete, type->isConcrete() );

	// check function type
	EXPECT_EQ( functional, type->isFunctionType() );

	// check children
	EXPECT_TRUE ( children == *(type->getChildren()) );


	// ------------ Type Token based tests -------------

	// copy and clone the type
	TypeManager manager;
	T copy = T(*type);
	T* clone = dynamic_cast<T*>(dynamic_cast<const Type*>(&*type)->clone(manager));

	// check whether all are equal
	T* all[] = { &*type, &copy, clone };
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {

			T* a = all[i];
			T* b = all[j];

			EXPECT_EQ ( *a , *b );
			EXPECT_EQ ( a->hash(), b->hash() );
			EXPECT_EQ ( a->getName(), b->getName() );
			EXPECT_EQ ( a->toString(), b->toString() );

		}
	}

	// check type properties
	for (int i=0; i<3; i++) {

		T* cur = all[i];

		// check concrete flag
		EXPECT_EQ( concrete, cur->isConcrete() );

		// check function type
		EXPECT_EQ( functional, cur->isFunctionType() );

		// check children
		EXPECT_TRUE ( equals(children, *(cur->getChildren()), equal_target<TypePtr>()));
	}

	delete clone;
}

} // end namespace core
} // end namespace insieme

