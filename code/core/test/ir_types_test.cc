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
#include <algorithm>
#include <stdexcept>
#include <vector>

#include "insieme/core/ir_types.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/channel.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/utils/name_mangling.h"

#include "ir_node_test.inc"

using std::vector;

namespace insieme {
namespace core {

	template <typename PT>
	void basicTypeTests(PT type, bool concrete, const NodeList& children = NodeList());

	TEST(TypeTest, NodeManager) {
		// create type manager
		NodeManager manager;

		// get a type
		GenericTypePtr typeA1 = GenericType::get(manager, "A");
		GenericTypePtr typeA2 = GenericType::get(manager, "A");
		GenericTypePtr typeB = GenericType::get(manager, "B");

		EXPECT_TRUE(!!typeA1);
		EXPECT_TRUE(!!typeA2);
		EXPECT_TRUE(!!typeB);

		EXPECT_TRUE(typeA1 == typeA2);
		EXPECT_FALSE(typeA1 == typeB);
	}

	TEST(TypeTest, NodeManagerGetAllBug) {
		NodeManager manager;

		TypePtr typeA = GenericType::get(manager, "A");
		TypePtr typeB = GenericType::get(manager, "B");
		TypePtr typeR = GenericType::get(manager, "R");

		TypeList list;
		list.push_back(typeA);
		list.push_back(typeB);

		FunctionTypePtr funType = FunctionType::get(manager, list, typeR);
	}

	TEST(TypeTest, MultipleNodeManager) {
		// create type manager
		NodeManager managerA;
		NodeManager managerB;

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

		EXPECT_EQ(12u, managerA.size());
		EXPECT_EQ(0u, managerB.size());

		// try to obtain the same type from the other manager
		GenericTypePtr rootB = GenericType::get(managerB, "R", typesB);
		EXPECT_EQ(12u, managerA.size());
		EXPECT_EQ(12u, managerB.size());

		EXPECT_NE(rootA, rootB);
		EXPECT_EQ(*rootA, *rootB);
	}


	TEST(TypeTest, GenericType) {
		// create a type manager for this test
		NodeManager manager;

		// some empty lists (required as arguments)
		vector<TypePtr> emptyPtr;

		// create some variable types
		TypeVariablePtr varA = TypeVariable::get(manager, "alpha");
		TypeVariablePtr varB = TypeVariable::get(manager, "beta");

		// create some concrete types
		GenericTypePtr typeA = GenericType::get(manager, "A");
		EXPECT_EQ("A", toString(*typeA));
		GenericTypePtr typeB = GenericType::get(manager, "B");
		EXPECT_EQ("B", toString(*typeB));

		// create complex types
		GenericTypePtr typeC = GenericType::get(manager, "C", toVector<TypePtr>(varA));
		EXPECT_EQ("C<'alpha>", toString(*typeC));

		// create complex type with multiple parameter
		vector<TypePtr> typeListA;
		typeListA.push_back(typeA);
		typeListA.push_back(typeB);
		GenericTypePtr typeE = GenericType::get(manager, "E", typeListA);
		EXPECT_EQ("E<A,B>", toString(*typeE));

		// create a type with parents
		auto typeListF = toVector<TypePtr>(typeA, typeB);
		auto parents = toVector(Parent::get(manager, typeA), Parent::get(manager, true, typeB));
		GenericTypePtr typeF = GenericType::get(manager, "F", parents, typeListF);
		EXPECT_EQ("F:[A,virtual B]<A,B>", toString(*typeF));

		// perform general test cases
		{
			SCOPED_TRACE("typeA");
			basicTypeTests(typeA, true, toVector<NodePtr>(typeA->getName(), typeA->getParents(), typeA->getTypeParameter()));
		}
		{
			SCOPED_TRACE("typeB");
			basicTypeTests(typeB, true, toVector<NodePtr>(typeB->getName(), typeB->getParents(), typeB->getTypeParameter()));
		}
		{
			SCOPED_TRACE("typeC");
			basicTypeTests(typeC, false, toVector<NodePtr>(typeC->getName(), typeC->getParents(), typeC->getTypeParameter()));
		}
		{
			SCOPED_TRACE("typeE");
			basicTypeTests(typeE, true, toVector<NodePtr>(typeE->getName(), typeE->getParents(), typeE->getTypeParameter()));
		}
		{
			SCOPED_TRACE("typeF");
			basicTypeTests(typeF, true, toVector<NodePtr>(typeF->getName(), typeF->getParents(), typeF->getTypeParameter()));
		}

		// selected equality checks (not after copy)
		EXPECT_FALSE(varA == varB);
		EXPECT_FALSE(typeA == typeB);
		EXPECT_FALSE(varA == typeB);
		EXPECT_TRUE(varA == varA);
		EXPECT_TRUE(typeA == typeA);

		// create list of all types
		vector<TypePtr> types;
		types.push_back(typeA);
		types.push_back(typeB);
		types.push_back(typeC);
		types.push_back(typeE);
		types.push_back(typeF);

		// check whether equality is working properly
		for(unsigned i = 0; i < types.size(); i++) {
			for(unsigned j = 0; j < types.size(); j++) {
				EXPECT_EQ(i == j, types[i] == types[j]);
			}
		}
	}


	TEST(TypeTest, TypeVariable) {
		NodeManager manager;
		TypeVariablePtr varTypeA = TypeVariable::get(manager, "alpha");
		TypeVariablePtr varTypeB = TypeVariable::get(manager, "beta");

		// check name
		EXPECT_EQ("'alpha", toString(*varTypeA));
		EXPECT_EQ("alpha", varTypeA->getVarName()->getValue());

		EXPECT_EQ("'beta", toString(*varTypeB));
		EXPECT_EQ("beta", varTypeB->getVarName()->getValue());

		// perform basic type tests
		basicTypeTests(varTypeA, false, toList(varTypeA->getVarName()));
	}

	TEST(TypeTest, TupleType) {
		NodeManager manager;

		vector<TypePtr> subTypesA;
		vector<TypePtr> subTypesB;
		subTypesB.push_back(GenericType::get(manager, "dummy1"));
		subTypesB.push_back(GenericType::get(manager, "dummy2"));

		TupleTypePtr typeA = TupleType::get(manager, subTypesA);
		TupleTypePtr typeB = TupleType::get(manager, subTypesB);


		// perform basic type tests
		basicTypeTests(typeA, true, toList(typeA->getElementTypes()));
		basicTypeTests(typeB, true, toList(typeB->getElementTypes()));
	}

	TEST(TypeTest, FunctionType) {
		NodeManager manager;

		TypePtr dummyA = GenericType::get(manager, "dummyA");
		TypePtr dummyB = GenericType::get(manager, "dummyB");
		TypePtr alpha = TypeVariable::get(manager, "alpha");
		TypePtr obj = lang::ReferenceType::create(GenericType::get(manager, "C"));

		TypePtr resultA = GenericType::get(manager, "returnA");
		TypePtr resultB = GenericType::get(manager, "returnB");
		TypePtr unit = manager.getLangBasic().getUnit();

		FunctionTypePtr funTypeA = FunctionType::get(manager, toVector(dummyA), resultA);
		FunctionTypePtr funTypeB = FunctionType::get(manager, toVector(alpha), resultB);
		FunctionTypePtr funTypeC = FunctionType::get(manager, toVector(alpha, dummyA), resultB);
		FunctionTypePtr funTypeD = FunctionType::get(manager, toVector(dummyA), resultA, FK_CLOSURE);

		FunctionTypePtr funTypeE = FunctionType::get(manager, toVector(obj, dummyA), FK_CONSTRUCTOR);
		FunctionTypePtr funTypeF = FunctionType::get(manager, toVector(obj), FK_DESTRUCTOR);
		FunctionTypePtr funTypeG = FunctionType::get(manager, toVector(obj, dummyA), resultA, FK_MEMBER_FUNCTION);
		FunctionTypePtr funTypeH = FunctionType::get(manager, toVector(obj, dummyA), resultA, FK_VIRTUAL_MEMBER_FUNCTION);

		FunctionTypePtr funTypeI = FunctionType::get(manager, Types::get(manager, toVector(obj, alpha, dummyB)), alpha, FK_PLAIN, Types::get(manager, toVector(alpha)));
		FunctionTypePtr funTypeJ = FunctionType::get(manager, Types::get(manager, toVector(obj, dummyA, dummyB)), resultA, FK_PLAIN, Types::get(manager, toVector(resultA)));

		EXPECT_EQ("((dummyA)->returnA)", toString(*funTypeA));
		EXPECT_EQ("(('alpha)->returnB)", toString(*funTypeB));
		EXPECT_EQ("(('alpha,dummyA)->returnB)", toString(*funTypeC));
		EXPECT_EQ("((dummyA)=>returnA)", toString(*funTypeD));

		EXPECT_EQ("(C::(dummyA))", toString(*funTypeE));
		EXPECT_EQ("(~C::())", toString(*funTypeF));
		EXPECT_EQ("(C::(dummyA)->returnA)", toString(*funTypeG));
		EXPECT_EQ("(C::(dummyA)~>returnA)", toString(*funTypeH));

		EXPECT_EQ("<'alpha>((ref<C,f,f,plain>,'alpha,dummyB)->'alpha)", toString(*funTypeI));
		EXPECT_EQ("<returnA>((ref<C,f,f,plain>,dummyA,dummyB)->returnA)", toString(*funTypeJ));

		EXPECT_TRUE(funTypeA->isPlain());
		EXPECT_TRUE(funTypeB->isPlain());
		EXPECT_TRUE(funTypeC->isPlain());
		EXPECT_FALSE(funTypeD->isPlain());
		EXPECT_FALSE(funTypeE->isPlain());
		EXPECT_FALSE(funTypeF->isPlain());
		EXPECT_FALSE(funTypeG->isPlain());

		EXPECT_FALSE(funTypeA->isClosure());
		EXPECT_FALSE(funTypeB->isClosure());
		EXPECT_FALSE(funTypeC->isClosure());
		EXPECT_TRUE(funTypeD->isClosure());
		EXPECT_FALSE(funTypeE->isClosure());
		EXPECT_FALSE(funTypeF->isClosure());
		EXPECT_FALSE(funTypeG->isClosure());

		EXPECT_TRUE(funTypeE->isConstructor());
		EXPECT_TRUE(funTypeF->isDestructor());
		EXPECT_TRUE(funTypeG->isMemberFunction());
		EXPECT_TRUE(funTypeH->isVirtualMemberFunction());

		EXPECT_FALSE(funTypeA->hasInstantiationTypes());
		EXPECT_TRUE(funTypeI->hasInstantiationTypes());
		EXPECT_TRUE(funTypeJ->getInstantiationTypeList().size() > 0);

		EXPECT_NE(funTypeA, funTypeD);

		vector<NodePtr> subNodesA;
		subNodesA.push_back(Types::get(manager, toVector(dummyA)));
		subNodesA.push_back(resultA);
		subNodesA.push_back(UIntValue::get(manager, FK_PLAIN));
		subNodesA.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesB;
		subNodesB.push_back(Types::get(manager, toVector(alpha)));
		subNodesB.push_back(resultB);
		subNodesB.push_back(UIntValue::get(manager, FK_PLAIN));
		subNodesB.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesC;
		subNodesC.push_back(Types::get(manager, toVector(alpha, dummyA)));
		subNodesC.push_back(resultB);
		subNodesC.push_back(UIntValue::get(manager, FK_PLAIN));
		subNodesC.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesD;
		subNodesD.push_back(Types::get(manager, toVector(dummyA)));
		subNodesD.push_back(resultA);
		subNodesD.push_back(UIntValue::get(manager, FK_CLOSURE));
		subNodesD.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesE;
		subNodesE.push_back(Types::get(manager, toVector(obj, dummyA)));
		subNodesE.push_back(unit);
		subNodesE.push_back(UIntValue::get(manager, FK_CONSTRUCTOR));
		subNodesE.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesF;
		subNodesF.push_back(Types::get(manager, toVector(obj)));
		subNodesF.push_back(unit);
		subNodesF.push_back(UIntValue::get(manager, FK_DESTRUCTOR));
		subNodesF.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesG;
		subNodesG.push_back(Types::get(manager, toVector(obj, dummyA)));
		subNodesG.push_back(resultA);
		subNodesG.push_back(UIntValue::get(manager, FK_MEMBER_FUNCTION));
		subNodesG.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesH;
		subNodesH.push_back(Types::get(manager, toVector(obj, dummyA)));
		subNodesH.push_back(resultA);
		subNodesH.push_back(UIntValue::get(manager, FK_VIRTUAL_MEMBER_FUNCTION));
		subNodesH.push_back(Types::get(manager, TypeList()));

		vector<NodePtr> subNodesI;
		subNodesI.push_back(Types::get(manager, toVector(obj, alpha, dummyB)));
		subNodesI.push_back(alpha);
		subNodesI.push_back(UIntValue::get(manager, FK_PLAIN));
		subNodesI.push_back(Types::get(manager, toVector(alpha)));

		vector<NodePtr> subNodesJ;
		subNodesJ.push_back(Types::get(manager, toVector(obj, dummyA, dummyB)));
		subNodesJ.push_back(resultA);
		subNodesJ.push_back(UIntValue::get(manager, FK_PLAIN));
		subNodesJ.push_back(Types::get(manager, toVector(resultA)));

		// perform basic type tests
		basicTypeTests(funTypeA, true, toList(subNodesA));
		basicTypeTests(funTypeB, true, toList(subNodesB));
		basicTypeTests(funTypeC, true, toList(subNodesC));
		basicTypeTests(funTypeD, true, toList(subNodesD));
		basicTypeTests(funTypeE, true, toList(subNodesE));
		basicTypeTests(funTypeF, true, toList(subNodesF));
		basicTypeTests(funTypeG, true, toList(subNodesG));
		basicTypeTests(funTypeH, true, toList(subNodesH));
		basicTypeTests(funTypeI, true, toList(subNodesI));
		basicTypeTests(funTypeJ, true, toList(subNodesJ));
	}

	TEST(TypeTest, TagTypeRecord) {
		// create a manager for this test
		NodeManager manager;
		IRBuilder builder(manager);

		auto isRecursive = [&](const TypePtr& type) { return type.isa<TagTypePtr>() && type.as<TagTypePtr>()->isRecursive(); };

		auto isNotRecursive = [&](const TypePtr& type) { return !isRecursive(type); };

		EXPECT_PRED1(isNotRecursive, builder.parseType("int<4>"));
		EXPECT_PRED1(isNotRecursive, builder.parseType("ref<int<4>>"));
		EXPECT_PRED1(isNotRecursive, builder.parseType("struct { x : int<4>; }"));
		EXPECT_PRED1(isNotRecursive, builder.parseType("decl struct B; def struct B { x : int<4>; }; B"));

		EXPECT_PRED1(isRecursive, builder.parseType("decl struct B; def struct B { x : ref<B>; }; B"));
		EXPECT_PRED1(isRecursive, builder.parseType("decl struct data; def struct data { x : ptr<data>; }; data"));
		EXPECT_PRED1(isRecursive, builder.parseType("decl struct A; decl struct B; def struct A { x : ref<B>; }; def struct B { x : ref<A>; }; B"));
		EXPECT_PRED1(isRecursive, builder.parseType("decl struct A; decl struct B; def struct A { x : ptr<B>; }; def struct B { x : ptr<A>; }; A"));

		EXPECT_PRED1(isNotRecursive, builder.parseType("decl struct A; decl struct B; def struct A { x : ref<A>; }; def struct B { y : ref<A>; }; B"));

		// -- discovered bug case --

		TagTypePtr tagType = builder.parseType(
				"decl struct A; "
				"decl struct B; "
				"def struct A { x : int<4>; }; "
				"def struct B { x : ref<A>; ctor( b : ref<B>) {} }; "
				"A"
		).as<TagTypePtr>();

		EXPECT_PRED1(isNotRecursive, tagType);
	}

	TEST(TypeTest, TagTypePeeling) {
		// create a manager for this test
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_EQ("struct {x:bla<int<4>>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}",
		          toString(*builder.parseType("let A = struct { x : bla<int<4>>; } in A")));
		EXPECT_EQ("struct {x:bla<int<4>>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}",
		          toString(*builder.parseType("let A = struct { x : bla<int<4>>; } in A").as<TagTypePtr>()->peel()));

		EXPECT_EQ("rec ^A.{^A=struct A "
		          "{x:bla<^A>,ctor(),ctor(ref<^A,t,f,cpp_ref>),ctor(ref<^A,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^A,t,f,cpp_ref>)->ref<^A,f,f,cpp_ref>,"
		          + utils::getMangledOperatorAssignName() + "(ref<^A,f,f,cpp_rref>)->ref<^A,f,f,cpp_ref>}}",
		          toString(*builder.parseType("decl struct A; def struct A { x : bla<A>; }; A")));
		EXPECT_EQ("struct A {x:bla<rec ^A.{^A=struct A "
		          "{x:bla<^A>,ctor(),ctor(ref<^A,t,f,cpp_ref>),ctor(ref<^A,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^A,t,f,cpp_ref>)->ref<^A,f,f,cpp_ref>,"
		          + utils::getMangledOperatorAssignName() + "(ref<^A,f,f,cpp_rref>)->ref<^A,f,f,cpp_ref>}}>,"
		          "ctor(),ctor(ref<^A,t,f,cpp_ref>),ctor(ref<^A,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^A,t,f,cpp_ref>)->ref<^A,f,f,cpp_ref>,"
		          + utils::getMangledOperatorAssignName() + "(ref<^A,f,f,cpp_rref>)->ref<^A,f,f,cpp_ref>}",
		          toString(*builder.parseType("decl struct A; def struct A { x : bla<A>; }; A").as<TagTypePtr>()->peel()));

		TagTypePtr tagType = builder.parseType("decl struct A; def struct A { x : bla<A>; }; A").as<TagTypePtr>();
		EXPECT_TRUE(tagType->isRecursive());

		EXPECT_TRUE(checks::check(tagType).empty()) << checks::check(tagType);
		EXPECT_TRUE(checks::check(tagType->peel(0)).empty()) << checks::check(tagType->peel(0));
		EXPECT_TRUE(checks::check(tagType->peel(1)).empty()) << checks::check(tagType->peel(1));
		EXPECT_TRUE(checks::check(tagType->peel(2)).empty()) << checks::check(tagType->peel(2));
	}


	TEST(TypeTest, TagTypeMemberPeeling) {
		// create a manager for this test
		NodeManager manager;
		IRBuilder builder(manager);

		auto tagType = builder.parseType("let A = struct { x : bla<int<4>>; } in A").as<TagTypePtr>();
		EXPECT_TRUE(checks::check(tagType).empty()) << checks::check(tagType);

		// get destructor (the wrong way)
		auto dtor = tagType->getRecord()->getDestructor();
		EXPECT_FALSE(checks::check(dtor).empty());

		auto fixedDtor = tagType->peel(dtor);
		EXPECT_TRUE(checks::check(fixedDtor).empty()) << checks::check(fixedDtor);
	}

	TEST(TypeTest, TagTypeUnpeeling) {
		// create a manager for this test
		NodeManager manager;
		IRBuilder builder(manager);

		auto aType = builder.parseType(R"(
			def struct A {
				lambda retA = () -> A { return *this; }
			};
			A
		)").as<TagTypePtr>();

		// peel once, then unpeel -> should be the same
		auto member = aType->getStruct()->getMemberFunctions().front();
		auto originalT = member.getType();
		auto peeledT = aType.peel(member).getType();
		EXPECT_NE(peeledT, originalT);
		auto unpeeledT = aType.unpeel(peeledT);
		EXPECT_EQ(unpeeledT, originalT);
	}


	TEST(TypeTest, TagTypeUnpeelingNotPeeled) {
		// create a manager for this test
		NodeManager manager;
		IRBuilder builder(manager);

		auto aType = builder.parseType(R"(
			def struct A {
				lambda retA = () -> A { return *this; }
			};
			A
		)").as<TagTypePtr>();

		// peel once, then unpeel -> should be the same
		auto member = aType->getStruct()->getMemberFunctions().front();
		auto originalT = member.getType();
		// peel manually
		auto peeledT = transform::replaceAllGen(manager, originalT, aType->getTag(), aType, transform::globalReplacement);
		EXPECT_NE(peeledT, originalT);
		auto unpeeledT = aType.unpeel(peeledT);
		EXPECT_EQ(unpeeledT, originalT);
	}

	TEST(TypeTest, RecTypeTest) {
		// test accessing the definition of a recursive type using addresses

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto tagType = builder.parseType("let type = struct class { next: ref<class,f,f,plain> ; } in type").isa<TagTypePtr>();
		EXPECT_TRUE(tagType);
		EXPECT_TRUE(tagType->isRecursive());

		auto recType = tagType->getRecord();
		auto recAddr = core::Address<const core::Node>::find(recType, tagType);
		EXPECT_EQ("0-1-0-1", toString(recAddr));
		EXPECT_TRUE(recAddr);
	}

	TEST(TypeTest, StructType) {
		NodeManager manager;
		IRBuilder builder(manager);

		vector<FieldPtr> entriesA;
		entriesA.push_back(builder.field("a", GenericType::get(manager, "A")));
		entriesA.push_back(builder.field("b", GenericType::get(manager, "B")));

		auto genStructA = builder.structType(entriesA);
		EXPECT_EQ("struct {a:A,b:B,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"\
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructA));

		vector<FieldPtr> entriesB;
		auto genStructB = builder.structType(entriesB);
		EXPECT_EQ("struct {ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructB));

		vector<FieldPtr> entriesC;
		entriesC.push_back(builder.field("a", TypeVariable::get(manager, "alpha")));
		entriesC.push_back(builder.field("b", GenericType::get(manager, "B")));
		auto genStructC = builder.structType(entriesC);
		EXPECT_EQ("struct {a:'alpha,b:B,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructC));

		// test for elements with same name
		vector<FieldPtr> entriesD;
		entriesD.push_back(builder.field("a", GenericType::get(manager, "A")));
		entriesD.push_back(builder.field("b", GenericType::get(manager, "A")));
		entriesD.push_back(builder.field("a", GenericType::get(manager, "A")));
		// this check produces the following warning issued by gtest
		// 'Death tests use fork(), which is unsafe particularly in a threaded context.
		//  For this test, Google Test couldn't detect the number of threads.'
		//
		// in agreement with 'the others' this warning can be ignored 'safely'
		#ifndef NDEBUG
			EXPECT_DEATH_IF_SUPPORTED(builder.structType(entriesD), "field names must be unique");
		#endif

		// .. same type should not be a problem
		entriesD.pop_back();
		builder.structType(entriesD);

		// this is an anonymous tagType reference
		auto tagType = builder.tagTypeReference("");
		// build the tagTypeDefinition for each struct
		auto tagDefinitionA = builder.tagTypeDefinition({{tagType, genStructA->getRecord()}});
		basicTypeTests(genStructA, true, toList(toVector<NodePtr>(tagType, tagDefinitionA)));

		auto tagDefinitionB = builder.tagTypeDefinition({{tagType, genStructB->getRecord()}});
		basicTypeTests(genStructB, true, toList(toVector<NodePtr>(tagType, tagDefinitionB)));

		auto tagDefinitionC = builder.tagTypeDefinition({{tagType, genStructC->getRecord()}});
		basicTypeTests(genStructC, false, toList(toVector<NodePtr>(tagType, tagDefinitionC)));
	}

	TEST(TypeTest, StructTypeParents) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create base type A
		vector<FieldPtr> entriesA;
		entriesA.push_back(builder.field("a", GenericType::get(manager, "A")));
		auto genStructA = builder.structType(entriesA);
		EXPECT_EQ("struct {a:A,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructA));

		// create derived type B : A
		vector<FieldPtr> entriesB;
		entriesB.push_back(builder.field("b", GenericType::get(manager, "B")));
		ParentsPtr parentsB = Parents::get(manager, toVector(Parent::get(manager, genStructA)));
		auto genStructB = builder.structType(parentsB, entriesB);
		EXPECT_EQ("struct : [struct {a:A,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}] " \
				  "{b:B,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructB));

		// create derived type C : A, B
		vector<FieldPtr> entriesC;
		entriesC.push_back(builder.field("c", GenericType::get(manager, "C")));
		ParentsPtr parentsC = Parents::get(manager, toVector(Parent::get(manager, genStructA), Parent::get(manager, true, genStructB)));
		auto genStructC = builder.structType(parentsC, entriesC);
		EXPECT_EQ("struct : [struct {a:A,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}, " \
				  "virtual struct : [struct {a:A,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"\
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}] " \
				  "{b:B,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}] " \
				  "{c:C,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructC));

		// this is an anonymous tagType reference
		auto tagType = builder.tagTypeReference("");
		// build the tagTypeDefinition for each struct
		auto tagDefinitionA = builder.tagTypeDefinition({{tagType, genStructA->getRecord()}});
		basicTypeTests(genStructA, true, toList(toVector<NodePtr>(tagType, tagDefinitionA)));

		auto tagDefinitionB = builder.tagTypeDefinition({{tagType, genStructB->getRecord()}});
		basicTypeTests(genStructB, true, toList(toVector<NodePtr>(tagType, tagDefinitionB)));

		auto tagDefinitionC = builder.tagTypeDefinition({{tagType, genStructC->getRecord()}});
		basicTypeTests(genStructC, true, toList(toVector<NodePtr>(tagType, tagDefinitionC)));
	}

	TEST(TypeTest, StructTypeNames) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create base type A
		vector<FieldPtr> entriesA;
		entriesA.push_back(builder.field("a", GenericType::get(manager, "A")));
		auto genStructA = builder.structType("xy", entriesA);
		EXPECT_EQ("struct xy {a:A,ctor(),ctor(ref<^xy,t,f,cpp_ref>),ctor(ref<^xy,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}", toString(*genStructA));

		// create derived type B : A
		vector<FieldPtr> entriesB;
		entriesB.push_back(builder.field("b", GenericType::get(manager, "B")));
		ParentsPtr parentsB = Parents::get(manager, toVector(Parent::get(manager, genStructA)));
		auto genStructB = builder.structType(builder.stringValue("xy"), parentsB, entriesB);
		EXPECT_EQ("struct xy : [struct xy {a:A,ctor(),ctor(ref<^xy,t,f,cpp_ref>),ctor(ref<^xy,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}] " \
				  "{b:B,ctor(),ctor(ref<^xy,t,f,cpp_ref>),ctor(ref<^xy,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}", toString(*genStructB));

		// create derived type C : A, B
		vector<FieldPtr> entriesC;
		entriesC.push_back(builder.field("c", GenericType::get(manager, "C")));
		ParentsPtr parentsC = Parents::get(manager, toVector(Parent::get(manager, genStructA), Parent::get(manager, true, genStructB)));
		auto genStructC = builder.structType(builder.stringValue("xy"), parentsC, entriesC);
		EXPECT_EQ("struct xy : [struct xy {a:A,ctor(),ctor(ref<^xy,t,f,cpp_ref>),ctor(ref<^xy,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}, " \
				  "virtual struct xy : [struct xy {a:A,ctor(),ctor(ref<^xy,t,f,cpp_ref>)," \
				  "ctor(ref<^xy,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>,"\
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}] " \
				  "{b:B,ctor(),ctor(ref<^xy,t,f,cpp_ref>),ctor(ref<^xy,f,f,cpp_rref>),dtor()," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}] " \
				  "{c:C,ctor(),ctor(ref<^xy,t,f,cpp_ref>),ctor(ref<^xy,f,f,cpp_rref>),dtor()," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,t,f,cpp_ref>)->ref<^xy,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^xy,f,f,cpp_rref>)->ref<^xy,f,f,cpp_ref>}", toString(*genStructC));

		// this is a named tagType reference
		auto tagType = builder.tagTypeReference("xy");
		// build the tagTypeDefinition for each struct
		auto tagDefinitionA = builder.tagTypeDefinition({{tagType, genStructA->getRecord()}});
		basicTypeTests(genStructA, true, toList(toVector<NodePtr>(tagType, tagDefinitionA)));

		auto tagDefinitionB = builder.tagTypeDefinition({{tagType, genStructB->getRecord()}});
		basicTypeTests(genStructB, true, toList(toVector<NodePtr>(tagType, tagDefinitionB)));

		auto tagDefinitionC = builder.tagTypeDefinition({{tagType, genStructC->getRecord()}});
		basicTypeTests(genStructC, true, toList(toVector<NodePtr>(tagType, tagDefinitionC)));
	}

	TEST(TypeTest, RecStructType) {
		// create a manager for this test
		NodeManager manager;
		IRBuilder builder(manager);

		// TODO: test whether order of definitions is important ... (it should not)

		// create a simple recursive type uX.X (no actual type)
		auto varX = builder.tagTypeReference("X");

		vector<FieldPtr> entriesA;
		entriesA.push_back(builder.field("a", GenericType::get(manager, "A")));
		entriesA.push_back(builder.field("b", builder.refType(varX)));

		auto genStructA = builder.structType(entriesA);
		EXPECT_EQ("struct {a:A,b:ref<^X,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genStructA));

		auto definition = builder.tagTypeDefinition({{varX, genStructA.as<TagTypePtr>()->getRecord()}});
		EXPECT_EQ("{^X=struct {a:A,b:ref<^X,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>)," \
				  "ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}}", toString(*definition));

		auto tagType = builder.tagType(varX, definition);
		EXPECT_EQ("rec ^X.{^X=struct {a:A,b:ref<^X,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>)," \
				  "ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}}", toString(*tagType));
		EXPECT_EQ("struct {a:A,b:ref<^X,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>)," \
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*tagType->getDefinition()->getDefinitionOf(varX)));

		// perform basic type tests
		basicTypeTests(tagType, true, toList(toVector<NodePtr>(varX, definition)));
	}

	TEST(TypeTest, UnionType) {
		NodeManager manager;
		IRBuilder builder(manager);

		vector<FieldPtr> entriesA;
		entriesA.push_back(builder.field("a", GenericType::get(manager, "A")));
		entriesA.push_back(builder.field("b", GenericType::get(manager, "B")));

		auto genUnionA = builder.unionType(entriesA);
		EXPECT_EQ("union {a:A,b:B,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genUnionA));

		vector<FieldPtr> entriesB;
		auto genUnionB = builder.unionType(entriesB);
		EXPECT_EQ("union {ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genUnionB));

		vector<FieldPtr> entriesC;
		entriesC.push_back(builder.field("a", TypeVariable::get(manager, "alpha")));
		entriesC.push_back(builder.field("b", GenericType::get(manager, "B")));
		auto genUnionC = builder.unionType(entriesC);
		EXPECT_EQ("union {a:'alpha,b:B,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," \
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}", toString(*genUnionC));

		// this is an anonymous tagType reference
		auto tagType = builder.tagTypeReference("");
		// build the tagTypeDefinition for each struct
		auto tagDefinitionA = builder.tagTypeDefinition({{tagType, genUnionA->getRecord()}});
		basicTypeTests(genUnionA, true, toList(toVector<NodePtr>(tagType, tagDefinitionA)));

		auto tagDefinitionB = builder.tagTypeDefinition({{tagType, genUnionB->getRecord()}});
		basicTypeTests(genUnionB, true, toList(toVector<NodePtr>(tagType, tagDefinitionB)));

		auto tagDefinitionC = builder.tagTypeDefinition({{tagType, genUnionC->getRecord()}});
		basicTypeTests(genUnionC, true, toList(toVector<NodePtr>(tagType, tagDefinitionC)));
	}

	TEST(TypeTest, ArrayType) {
		// create type manager and element types
		NodeManager manager;
		IRBuilder builder(manager);

		auto elementTypeA = GenericType::get(manager, "A");
		auto elementTypeB = TypeVariable::get(manager, "a");

		// obtain array types
		auto param1 = builder.intLit(1);
		auto param3 = builder.intLit(3);
		GenericTypePtr genArrayTypeA = builder.arrayType(elementTypeA, param1);
		GenericTypePtr genArrayTypeB = builder.arrayType(elementTypeB, param3);

		// check names
		EXPECT_EQ("array<A,1>", toString(*genArrayTypeA));
		EXPECT_EQ("array<'a,3>", toString(*genArrayTypeB));

		auto arrayTypeA = core::lang::ArrayType(genArrayTypeA);
		auto arrayTypeB = core::lang::ArrayType(genArrayTypeB);

		// check element types
		EXPECT_EQ(elementTypeA, arrayTypeA.getElementType());
		EXPECT_EQ(elementTypeB, arrayTypeB.getElementType());

		// check dimensions
		auto sizeA = arrayTypeA.getSize();
		auto sizeB = arrayTypeB.getSize();

		ASSERT_TRUE(sizeA->getNodeType() == core::NT_Literal);
		ASSERT_TRUE(sizeB->getNodeType() == core::NT_Literal);

		EXPECT_TRUE(manager.getLangBasic().isInt(sizeA.as<LiteralPtr>()->getType()));
		EXPECT_TRUE(manager.getLangBasic().isInt(sizeB.as<LiteralPtr>()->getType()));

		EXPECT_EQ("1", sizeA.as<LiteralPtr>()->getStringValue());
		EXPECT_EQ("3", sizeB.as<LiteralPtr>()->getStringValue());

		// check remaining type properties
		basicTypeTests(genArrayTypeA, true, toList(genArrayTypeA.getName(), builder.parents(), genArrayTypeA.getTypeParameter()));
		basicTypeTests(genArrayTypeB, false, toList(genArrayTypeB.getName(), builder.parents(), genArrayTypeB.getTypeParameter()));
	}

	TEST(TypeTest, ChannelType) {
		// create type manager and element types
		NodeManager manager;
		IRBuilder builder(manager);

		auto elementTypeA = GenericType::get(manager, "A");
		auto elementTypeB = TypeVariable::get(manager, "a");

		// obtain array types
		auto param1 = builder.uintLit(1);
		auto param3 = builder.uintLit(3);
		GenericTypePtr genChannelTypeA = builder.channelType(elementTypeA, param1);
		GenericTypePtr genChannelTypeB = builder.channelType(elementTypeB, param3);

		// check names
		EXPECT_EQ("channel<A,1>", toString(*genChannelTypeA));
		EXPECT_EQ("channel<'a,3>", toString(*genChannelTypeB));

		auto channelTypeA = core::lang::ChannelType(genChannelTypeA);
		auto channelTypeB = core::lang::ChannelType(genChannelTypeB);

		// check element types
		EXPECT_EQ(elementTypeA, channelTypeA.getElementType());
		EXPECT_EQ(elementTypeB, channelTypeB.getElementType());

		// check dimensions
		auto sizeA = channelTypeA.getSize();
		auto sizeB = channelTypeB.getSize();

		ASSERT_TRUE(sizeA->getNodeType() == core::NT_Literal);
		ASSERT_TRUE(sizeB->getNodeType() == core::NT_Literal);

		EXPECT_TRUE(manager.getLangBasic().isInt(sizeA.as<LiteralPtr>()->getType()));
		EXPECT_TRUE(manager.getLangBasic().isInt(sizeB.as<LiteralPtr>()->getType()));

		EXPECT_EQ("1", sizeA.as<LiteralPtr>()->getStringValue());
		EXPECT_EQ("3", sizeB.as<LiteralPtr>()->getStringValue());

		// check remaining type properties
		basicTypeTests(genChannelTypeA, true, toList(genChannelTypeA.getName(), builder.parents(), genChannelTypeA.getTypeParameter()));
		basicTypeTests(genChannelTypeB, false, toList(genChannelTypeB.getName(), builder.parents(), genChannelTypeB.getTypeParameter()));
	}

	TEST(TypeTest, RefType) {
		// create type manager and element types
		NodeManager manager;
		IRBuilder builder(manager);

		auto elementTypeA = GenericType::get(manager, "A");
		auto elementTypeB = TypeVariable::get(manager, "a");

		// obtain array types
		auto apTypeA = builder.refType(elementTypeA, false, false);
		auto apTypeB = builder.refType(elementTypeB, false, false);
		// obtain plain reference types
		auto refTypeA = core::lang::ReferenceType(apTypeA);
		auto refTypeB = core::lang::ReferenceType(apTypeB);

		// check names
		EXPECT_EQ("AP(ref<A,f,f,plain>)", toString(apTypeA));
		EXPECT_EQ("ref<A,f,f,plain>", toString(*apTypeA));
		EXPECT_EQ("AP(ref<'a,f,f,plain>)", toString(apTypeB));
		EXPECT_EQ("ref<'a,f,f,plain>", toString(*apTypeB));

		// check element types
		EXPECT_EQ(elementTypeA, refTypeA.getElementType());
		EXPECT_EQ(elementTypeB, refTypeB.getElementType());

		// check properties of the reference
		EXPECT_TRUE(refTypeA.isPlain());
		EXPECT_FALSE(refTypeA.isConst());
		EXPECT_FALSE(refTypeA.isVolatile());
		EXPECT_EQ(core::lang::ReferenceType::Kind::Plain, refTypeA.getKind());
		EXPECT_FALSE(refTypeA.isCppReference());
		EXPECT_FALSE(refTypeA.isCppRValueReference());

		EXPECT_TRUE(refTypeB.isPlain());
		EXPECT_FALSE(refTypeB.isConst());
		EXPECT_FALSE(refTypeB.isVolatile());
		EXPECT_EQ(core::lang::ReferenceType::Kind::Plain, refTypeB.getKind());
		EXPECT_FALSE(refTypeB.isCppReference());
		EXPECT_FALSE(refTypeB.isCppRValueReference());
	}


	template <typename PT>
	void basicTypeTests(PT type, bool concrete, const NodeList& children) {
		typedef typename PT::element_type T;

		// ------------- Basic Node Tests ----------------

		basicNodeTests(type, children);

		// ------------ Type Ptr based tests -------------

		// check concrete flag
		// TODO: implement alternative is-concrete check
		// EXPECT_EQ( concrete, type->isConcrete() );

		// ------------ Type Token based tests -------------

		// copy and clone the type
		NodeManager manager;
		T copy = T(*type);
		T* clone = &*manager.get(type);

		// check whether all are equal
		T* all[] = {&*type, &copy, clone};
		for(int i = 0; i < 3; i++) {
			for(int j = 0; j < 3; j++) {
				T* a = all[i];
				T* b = all[j];

				EXPECT_EQ(*a, *b);
				EXPECT_EQ(a->hash(), b->hash());
				EXPECT_EQ(toString(*a), toString(*b));
			}
		}

		// check type properties
		for(int i = 0; i < 3; i++) {
			T* cur = all[i];

			// check concrete flag
			// TODO: implement alternative is-concrete check
			// EXPECT_EQ( concrete, cur->isConcrete() );

			// check children
			EXPECT_TRUE(equals(children, cur->getChildList(), equal_target<NodePtr>())) << "Should: " << children << "\n"
			                                                                            << "Actual: " << cur->getChildList() << "\n";
		}
	}

} // end namespace core
} // end namespace insieme
