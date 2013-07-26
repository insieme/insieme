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

#include <sstream>

#include "insieme/core/ir_class_info.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/dump/binary_dump.h"

namespace insieme {
namespace core {

	using namespace std;

	TEST(MemberFunction, Basic) {

		NodeManager manager;
		IRBuilder builder(manager);

		auto impl = builder.parse("C::(int<4> a)->int<4> { return a; }").as<LambdaExprPtr>();
		MemberFunction fun("f", impl);

		EXPECT_EQ("f = mfun C v1 :: (int<4> v2) -> int<4> {\n    return v2;\n}", toString(fun));

		MemberFunction fun2("f", impl, true, true);
		EXPECT_EQ("virtual const f = mfun C v1 :: (int<4> v2) -> int<4> {\n    return v2;\n}", toString(fun2));

		EXPECT_NE(fun, fun2);
		EXPECT_EQ(fun, MemberFunction("f", impl));

	}

	TEST(ClassInfo, Basic) {

		NodeManager manager;
		IRBuilder builder(manager);

		auto ctor = builder.parse("C::() { }").as<LambdaExprPtr>();
		auto ctor2 = builder.parse("C::(int<4> a) { }").as<LambdaExprPtr>();
		auto dtor = builder.parse("~C::() { }").as<LambdaExprPtr>();

		auto fun1Impl = builder.parse("C::(int<4> a)->int<4> { return a; }").as<LambdaExprPtr>();
		auto fun2Impl = builder.parse("C::(int<4> a, int<4> b)->int<4> { return a+b; }").as<LambdaExprPtr>();

		EXPECT_TRUE(ctor);
		EXPECT_TRUE(dtor);
		EXPECT_TRUE(fun1Impl);
		EXPECT_TRUE(fun2Impl);

		// --------------------------------

		ClassMetaInfo info1;
		info1.addConstructor(ctor);
		info1.addConstructor(ctor2);
		info1.setDestructor(dtor);
		info1.addMemberFunction("f", fun1Impl);
		info1.addMemberFunction("f", fun2Impl, true);

//		std::cout << info1 << "\n";

	}

	TEST(ClassInfo, MemberFunctionLookup) {
		NodeManager manager;
		IRBuilder builder(manager);

		ClassMetaInfo info;

		auto fun = builder.parse("A::()->int<4> { return 3; }").as<LambdaExprPtr>();
		auto funType = fun->getFunctionType();

		MemberFunctionPtr not_found = NULL;

		EXPECT_FALSE(info.hasMemberFunction("test", funType));
		EXPECT_FALSE(info.hasMemberFunction("test", funType, true));
		EXPECT_FALSE(info.hasMemberFunction("test", funType, false));

		EXPECT_EQ(not_found, info.getMemberFunction("test", funType));
		EXPECT_EQ(not_found, info.getMemberFunction("test", funType, true));
		EXPECT_EQ(not_found, info.getMemberFunction("test", funType, false));

		info.addMemberFunction("test", fun, true, true);

		EXPECT_TRUE(info.hasMemberFunction("test", funType));
		EXPECT_TRUE(info.hasMemberFunction("test", funType, true));
		EXPECT_FALSE(info.hasMemberFunction("test", funType, false));

		EXPECT_NE(not_found, info.getMemberFunction("test", funType));
		EXPECT_NE(not_found, info.getMemberFunction("test", funType, true));
		EXPECT_EQ(not_found, info.getMemberFunction("test", funType, false));
	}

	TEST(ClassInfo, ManagerClone) {

		NodeManager mgrA;
		NodeManager mgrB;

		IRBuilder builderA(mgrA);

		StructTypePtr typeA = builderA.parse("struct { int<4> a; real<8> b; }").as<StructTypePtr>();
		EXPECT_TRUE(typeA);

		// create a class info
		std::map<string, NodePtr> symbols;
		symbols["T"] = typeA;

		ClassMetaInfo info;
		info.addConstructor(builderA.parse("T::() {}", symbols).as<LambdaExprPtr>());
		info.addConstructor(builderA.parse("T::(int<4> x) {}", symbols).as<LambdaExprPtr>());
		info.setDestructor(builderA.parse("~T::() {}", symbols).as<LambdaExprPtr>());
		info.addMemberFunction("f", builderA.parse("T::(int<4> a)->int<4> { return a; }", symbols).as<LambdaExprPtr>());

		// attach to type
		setMetaInfo(typeA, info);
		EXPECT_TRUE(typeA->hasAttachedValue<ClassMetaInfo>());

		// migrate to other manager
		TypePtr typeB = mgrB.get(typeA);

		EXPECT_EQ(*typeA, *typeB);
		EXPECT_NE(typeA, typeB);

		EXPECT_TRUE(mgrA.addressesLocal(typeA));
		EXPECT_TRUE(mgrB.addressesLocal(typeB));
		EXPECT_FALSE(mgrA.addressesLocal(typeB));
		EXPECT_FALSE(mgrB.addressesLocal(typeA));

		EXPECT_TRUE(typeB->hasAttachedValue<ClassMetaInfo>());
		const auto& infoB = getMetaInfo(typeB);

		// info should have been correctly migrated
		EXPECT_EQ(info, infoB);

		auto isManagedByB = [&](const NodePtr& cur)->bool {
			return mgrB.addressesLocal(cur);
		};

		// child-nodes should all reference copies within manager B
		EXPECT_FALSE(visitDepthFirstInterruptible(typeB, isManagedByB, true));

		// also the components of the meta-information class
		EXPECT_TRUE(all(infoB.getConstructors(), isManagedByB));
		EXPECT_TRUE(isManagedByB(infoB.getDestructor()));
		EXPECT_TRUE(all(info.getMemberFunctions(), [&](const MemberFunction& cur) { return mgrB.contains(cur.getImplementation()); }));

	}

	TEST(ClassInfo, ManagerCloneIndirect) {

		/**
		 * To be tested: whether the migration of annotations exhibiting a cyclic
		 * dependency with a size >1 to the node they are annotating is working.
		 */

		NodeManager mgrA;
		NodeManager mgrB;

		IRBuilder builderA(mgrA);

		TypePtr typeA = builderA.parse("struct { int<4> a; real<8> b; }").as<StructTypePtr>();
		EXPECT_TRUE(typeA);

		// create a class info
		std::map<string, NodePtr> symbols;
		symbols["T"] = typeA;

		ClassMetaInfo info;
		info.addConstructor(builderA.parse("T::() {}", symbols).as<LambdaExprPtr>());
		info.addConstructor(builderA.parse("T::(int<4> x) {}", symbols).as<LambdaExprPtr>());
		info.setDestructor(builderA.parse("~T::() {}", symbols).as<LambdaExprPtr>());
		info.addMemberFunction("f", builderA.parse("T::(int<4> a)->int<4> { return a; }", symbols).as<LambdaExprPtr>());

		// attach to type
		setMetaInfo(typeA, info);
		EXPECT_TRUE(typeA->hasAttachedValue<ClassMetaInfo>());

		// get one step higher
		typeA = builderA.parseType("T::()", symbols).as<FunctionTypePtr>();

		EXPECT_EQ("(struct<a:int<4>,b:real<8>>::())", toString(*typeA));
		EXPECT_TRUE(typeA.as<FunctionTypePtr>()->getObjectType()->hasAttachedValue<ClassMetaInfo>());
		EXPECT_EQ(info, getMetaInfo(typeA.as<FunctionTypePtr>()->getObjectType()));

		// migrate to other manager
		TypePtr typeB = mgrB.get(typeA);

		EXPECT_EQ(*typeA, *typeB);
		EXPECT_NE(typeA, typeB);

		EXPECT_TRUE(mgrA.addressesLocal(typeA));
		EXPECT_TRUE(mgrB.addressesLocal(typeB));
		EXPECT_FALSE(mgrA.addressesLocal(typeB));
		EXPECT_FALSE(mgrB.addressesLocal(typeA));

		auto oTypeA = typeA.as<FunctionTypePtr>()->getObjectType();
		auto oTypeB = typeB.as<FunctionTypePtr>()->getObjectType();

		EXPECT_EQ(*oTypeA, *oTypeB);
		EXPECT_NE(oTypeA, oTypeB);

		EXPECT_TRUE(mgrA.addressesLocal(oTypeA));
		EXPECT_TRUE(mgrB.addressesLocal(oTypeB));
		EXPECT_FALSE(mgrA.addressesLocal(oTypeB));
		EXPECT_FALSE(mgrB.addressesLocal(oTypeA));

		EXPECT_TRUE(typeB.as<FunctionTypePtr>()->getObjectType()->hasAttachedValue<ClassMetaInfo>());
		const auto& infoB = getMetaInfo(typeB.as<FunctionTypePtr>()->getObjectType());

		// info should have been correctly migrated
		EXPECT_EQ(info, infoB);
		EXPECT_NE(&getMetaInfo(typeA.as<FunctionTypePtr>()->getObjectType()), &getMetaInfo(typeB.as<FunctionTypePtr>()->getObjectType()));

		auto isManagedByB = [&](const NodePtr& cur)->bool {
			return mgrB.addressesLocal(cur);
		};

		// child-nodes should all reference copies within manager B
		EXPECT_FALSE(visitDepthFirstInterruptible(typeB, isManagedByB, true));

		// also the components of the meta-information class
		EXPECT_TRUE(all(infoB.getConstructors(), isManagedByB));
		EXPECT_TRUE(isManagedByB(infoB.getDestructor()));
		EXPECT_TRUE(all(info.getMemberFunctions(), [&](const MemberFunction& cur) { return mgrB.contains(cur.getImplementation()); }));

	}

	TEST(ClassInfo, Transformation) {

		/**
		 * The class info instance should be automatically adjusted in case the
		 * node it is annotating is modified.
		 */

		NodeManager mgr;
		IRBuilder builder(mgr);

		// create a type
		TypePtr type = builder.parseType("struct { int<4> x; int<4> y; }");

		std::map<string, NodePtr> symbols;
		symbols["T"] = type;

		// add information regarding a default constructor
		ClassMetaInfo info;
		info.addConstructor(builder.parseExpr("T::() { this->x = 0; this->y = 0; }", symbols).as<LambdaExprPtr>());
		info.addConstructor(builder.parseExpr("T::(int<4> x, int<4> y) { this->x = x; this->y = y; }", symbols).as<LambdaExprPtr>());

		info.setDestructor(builder.parseExpr("~T::() {}", symbols).as<LambdaExprPtr>());
		info.setDestructorVirtual(true);

		info.addMemberFunction("abs", builder.parseExpr(
				"let sqrt = lit(\"sqrt\" : (int<4>)->int<4>) in "
				"T::()->int<4> { return sqrt(this->x*this->x + this->y * this->y); }"
				,symbols).as<LambdaExprPtr>());

		info.addMemberFunction("magic", builder.getPureVirtual(builder.parseType("T::()->bool", symbols).as<FunctionTypePtr>()), true);

		setMetaInfo(type, info);


		// run semantic checks - they should be fine
		EXPECT_TRUE(checks::check(type).empty()) << checks::check(type);

//		std::cout << "Class Info: \n" << info << "\n";

		// transform type - by adding a new field and using std-replacement utils
		TypePtr newType = builder.parseType("struct { int<4> x; int<4> y; int<4> z; }");

		auto transformed = core::transform::replaceAll(mgr, type, type, newType).as<TypePtr>();;

		EXPECT_EQ(transformed, newType);
		ASSERT_TRUE(hasMetaInfo(transformed));

		// inspect meta-information
//		const ClassMetaInfo& newInfo = getMetaInfo(transformed);
//		std::cout << "New Class Info: \n" << newInfo << "\n";

		// apply semantic checks
		EXPECT_TRUE(checks::check(transformed).empty()) << checks::check(transformed);

	}


	TEST(ClassInfo, BinaryDump) {

		// create a code fragment using manager A
		NodeManager mgrA;
		IRBuilder builderA(mgrA);

		StructTypePtr typeA = builderA.parse("struct { int<4> a; real<8> b; }").as<StructTypePtr>();
		EXPECT_TRUE(typeA);

		// create a class info
		std::map<string, NodePtr> symbols;
		symbols["T"] = typeA;

		ClassMetaInfo info;
		info.addConstructor(builderA.parse("T::() {}", symbols).as<LambdaExprPtr>());
		info.addConstructor(builderA.parse("T::(int<4> x) {}", symbols).as<LambdaExprPtr>());
		info.setDestructor(builderA.parse("~T::() {}", symbols).as<LambdaExprPtr>());
		info.addMemberFunction("f", builderA.parse("T::(int<4> a)->int<4> { return a; }", symbols).as<LambdaExprPtr>());

		// attach to type
		setMetaInfo(typeA, info);
		EXPECT_TRUE(typeA->hasAttachedValue<ClassMetaInfo>());

		// ---------------------------

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		dump::binary::dumpIR(buffer, typeA);

		// reload IR using a different node manager
		NodeManager mgrB;
		TypePtr restored = dump::binary::loadIR(buffer, mgrB).as<TypePtr>();

		EXPECT_NE(typeA, restored);
		EXPECT_EQ(*typeA, *restored);

		// annotation should still be available
		EXPECT_TRUE(restored->hasAttachedValue<ClassMetaInfo>());

		// annotation should be restored correctly
		EXPECT_EQ(info, getMetaInfo(restored)) << "Original:\n" << info << "\nRestored:\n" << getMetaInfo(restored);

	}

	TEST(ClassInfo, BinaryDumpWithoutDestructor) {

		// create a code fragment using manager A
		NodeManager mgrA;
		IRBuilder builderA(mgrA);

		StructTypePtr typeA = builderA.parse("struct { int<4> a; real<8> b; }").as<StructTypePtr>();
		EXPECT_TRUE(typeA);

		// create a class info
		std::map<string, NodePtr> symbols;
		symbols["T"] = typeA;

		ClassMetaInfo info;
		info.addConstructor(builderA.parse("T::() {}", symbols).as<LambdaExprPtr>());
		info.addConstructor(builderA.parse("T::(int<4> x) {}", symbols).as<LambdaExprPtr>());
		info.addMemberFunction("f", builderA.parse("T::(int<4> a)->int<4> { return a; }", symbols).as<LambdaExprPtr>());

		EXPECT_FALSE(info.hasDestructor());

		// attach to type
		setMetaInfo(typeA, info);
		EXPECT_TRUE(typeA->hasAttachedValue<ClassMetaInfo>());

		// ---------------------------

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		dump::binary::dumpIR(buffer, typeA);

		// reload IR using a different node manager
		NodeManager mgrB;
		TypePtr restored = dump::binary::loadIR(buffer, mgrB).as<TypePtr>();

		EXPECT_NE(typeA, restored);
		EXPECT_EQ(*typeA, *restored);

		// annotation should still be available
		EXPECT_TRUE(restored->hasAttachedValue<ClassMetaInfo>());

		// annotation should be restored correctly
		EXPECT_EQ(info, getMetaInfo(restored)) << "Original:\n" << info << "\nRestored:\n" << getMetaInfo(restored);

	}

} // end namespace core
} // end namespace insieme
