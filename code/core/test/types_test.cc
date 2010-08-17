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
#include <vector>

#include "types.h"
#include "container_utils.h"

using std::vector;

template<typename T, typename PT>
void basicTypeTests(PT type, bool concrete, bool functional, vector<TypePtr> children = vector<TypePtr>());

TEST(TypeTest, TypeManager ) {

	// create type manager
	TypeManager manager;

	// get a type
	GenericTypePtr typeA1 = GenericType::get(manager, "A");
	GenericTypePtr typeA2 = GenericType::get(manager, "A");
	GenericTypePtr typeB = GenericType::get(manager, "B");

	EXPECT_TRUE ( typeA1 == typeA2 );
	EXPECT_FALSE ( typeA1 == typeB );

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

	EXPECT_EQ ( rootA, rootB );
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
		basicTypeTests<GenericType>(typeA, true, false);
	}{
		SCOPED_TRACE ( "typeB" );
		basicTypeTests<GenericType>(typeB, true, false);
	}{
		SCOPED_TRACE ( "typeC" );
		basicTypeTests<GenericType>(typeC, false, false, toVector<TypePtr>(varA));
	}{
		SCOPED_TRACE ( "typeD" );
		basicTypeTests<GenericType>(typeD, false, false);
	}{
		SCOPED_TRACE ( "typeE" );
		basicTypeTests<GenericType>(typeE, true, false, typeListA);
	}{
		SCOPED_TRACE ( "typeF" );
		basicTypeTests<GenericType>(typeF, true, false, toVector<TypePtr>(typeA));
	}{
		SCOPED_TRACE ( "typeG" );
		vector<TypePtr> list(typeListA);
		list.push_back(typeA);
		basicTypeTests<GenericType>(typeG, true, false, list);
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
	basicTypeTests<TypeVariable>(varTypeA, false, false);
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
	basicTypeTests<TupleType>(typeA, true, false, subTypesA);
	basicTypeTests<TupleType>(typeB, true, false, subTypesB);

}

TEST(TypeTest, FunctionType) {

	TypeManager manager;

	TypePtr argumentA = GenericType::get(manager, "dummyA");
	TypePtr argumentB = TypeVariable::get(manager, "alpha");
	TypePtr resultA = GenericType::get(manager, "returnA");
	TypePtr resultB = GenericType::get(manager, "returnB");

	FunctionTypePtr funTypeA = FunctionType::get(manager, argumentA, resultA);
	FunctionTypePtr funTypeB = FunctionType::get(manager, argumentB, resultB);

	EXPECT_EQ ( "(dummyA -> returnA)" , funTypeA->getName() );
	EXPECT_EQ ( "('alpha -> returnB)" , funTypeB->getName() );

	vector<TypePtr> subTypesA;
	subTypesA.push_back(argumentA);
	subTypesA.push_back(resultA);

	vector<TypePtr> subTypesB;
	subTypesB.push_back(argumentB);
	subTypesB.push_back(resultB);

	// perform basic type tests
	basicTypeTests<FunctionType>(funTypeA, true, true, subTypesA);
	basicTypeTests<FunctionType>(funTypeB, true, true, subTypesB);
}

//TEST(TypeTest, StructType) {
//
//	IdentifierManager manager;
//	TypeManager manager;
//
//	IdentifierPtr identA = Identifier::get(manager, "a");
//	IdentifierPtr identB = Identifier::get(manager, "a");
//
//	StructType::Entries entriesA;
//	entriesA.push_back(std::make_pair(identA, GenericType::get(manager, "A")));
//	entriesA.push_back(std::make_pair(identB, GenericType::get(manager, "B")));
//
//	StructTypePtr structA = StructType::get(manager, entriesA);
//
//	EXPECT_EQ ( "", structA.getName() );
//
//	// perform basic type tests
//	basicTypeTests<FunctionType>(structA, true, true, entriesA);
//	basicTypeTests<FunctionType>(funTypeB, true, true, subTypesB);
//}



TEST(TypesTest, IntTypeParam) {
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

template<typename T, typename PT>
void basicTypeTests(PT type, bool concrete, bool functional, vector<TypePtr> children) {

	// ------------ Type Ptr based tests -------------

	// check concrete flag
	EXPECT_EQ( concrete, type->isConcrete() );

	// check function type
	EXPECT_EQ( functional, type->isFunctionType() );

	// check children
	EXPECT_TRUE ( children == *(type->getChildren()) );


	// ------------ Type Token based tests -------------

	// create a copy of the type
	T copy = T(*type);
	T clone = *(std::unique_ptr<T>(type->clone()));

	// check whether all are equal
	T all[] = { *type, copy, clone };
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {

			T a = all[i];
			T b = all[j];

			EXPECT_EQ ( a , b );
			EXPECT_EQ ( a.hash(), b.hash() );
			EXPECT_EQ ( a.getName(), b.getName() );
			EXPECT_EQ ( a.toString(), b.toString() );

		}
	}

	// check type properties
	for (int i=0; i<3; i++) {

		T cur = all[i];

		// check concrete flag
		EXPECT_EQ( concrete, cur.isConcrete() );

		// check function type
		EXPECT_EQ( functional, cur.isFunctionType() );

		// check children
		EXPECT_TRUE ( children == *(type->getChildren()) );

	}

}


