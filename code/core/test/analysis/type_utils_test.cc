/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace core {
namespace analysis {

	bool hasNoFreeTypeVariables(const core::TypePtr& type) {
		return !hasFreeTypeVariables(type);
	}

	TEST(FreeTypeVariables, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test some cases with free variables
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("'a"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("set<'a>"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("array<'a,1>"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("struct { data : 'a; }"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("struct { x : struct { data : 'a; }; }"));

		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("int<4>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("set<int<4>>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("array<int<4>,1>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("struct { data : int<4>; }"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("struct { x : struct { data : int<4>; }; }"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("('a)->'a"));
	}

	TEST(TrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("int<4>")));
	}

	TEST(ArrayOfTrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("array<int<4>,1>")));
	}

	TEST(ArrayOfNonTrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType("array<struct { x: ref<'a,f,f>; },1>")));
	}

	TEST(TrivialStruct, DISABLED_Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("struct class {}")));
	}

	TEST(TrivialUnion, DISABLED_Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("union { data: int<4>; }")));
	}

	TEST(TrivialMember, DISABLED_Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a struct with a trivial member must also be trivial, iff no user-defined ctor is in place
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: int<4>;"
			"}")));
	}

	TEST(NonTrivialMember, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a member of type reference without initialization is non-trivial
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  data: ref<'a,f,f>;"
			"}")));
	}

	TEST(NoneTrivialMemberNested, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  data: struct {"
			"    data: ref<'a,f,f>;"
			"  };"
			"}")));
	}

	TEST(NoneVirtualLambda, DISABLED_Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda fn : () -> unit { }"
			"}")));
	}

	TEST(VirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  virtual lambda fn : () -> unit { }"
			"}")));
	}

	TEST(PureVirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  pure virtual fn : () -> unit"
			"}")));
	}

	TEST(VirtualDestructor, DISABLED_UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);

		// this is the 'base'-struct which is used to generate the actual struct with a virtual dtor
		auto recordType = builder.parseType(
			"struct class {"
			"  ctor() {}"
			"  ctor(other: ref<class,t,f,cpp_ref>) {}"
			"  ctor(other: ref<class,t,f,cpp_rref>) {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_ref>) -> ref<class,f,f,cpp_ref> {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> {}"
			"}").isa<TagTypePtr>()->getRecord();

		core::ExpressionList ctors;
		for (auto ctor : recordType->getConstructors())
			ctors.push_back(ctor.as<LambdaExprPtr>());

		core::MemberFunctionList members;
		for(auto memFun : recordType->getMemberFunctions())
			members.push_back(memFun);

		ParentList parents;
		FieldList fields;
		PureVirtualMemberFunctionList pvmfuns;
		EXPECT_FALSE(isTrivial(builder.structType("class", parents, fields, ctors, builder.getDefaultDestructor("class"), true, members, pvmfuns)));
	}

	TEST(Constructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"}")));
	}

	TEST(Constructor, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() {}"
			"}")));
	}

	TEST(CopyConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,t,f,cpp_ref>) { return; }"
			"}")));
	}

	TEST(CopyConstructor, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,t,f,cpp_ref>) {}"
			"}")));
	}

	TEST(MoveConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,t,f,cpp_rref>) { return; }"
			"}")));
	}

	TEST(MoveConstructor, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,t,f,cpp_rref>) {}"
			"}")));
	}

	TEST(Constructors, DISABLED_UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"  ctor(other: ref<class,t,f,cpp_ref>) { return; }"
			"  ctor(other: ref<class,t,f,cpp_rref>) { return; }"
			"}")));
	}

	TEST(Constructors, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() {}"
			"  ctor(other: ref<class,t,f,cpp_ref>) {}"
			"  ctor(other: ref<class,t,f,cpp_rref>) {}"
			"}")));
	}

	TEST(NonTrivialConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(data: int<4>) { return; }"
			"}")));
	}

	TEST(TrivialStructWithTrivialBase, DISABLED_Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"let base = struct base_class {} in struct class : [public base] {}")));
	}

	TEST(TrivialStructWithNonTrivialBase, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor() {}"
			"} in struct class : [public base] {}")));
	}

	TEST(CopyAssignment, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda operator_assign : (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> { return; }"
			"}")));
	}

	TEST(CopyAssignment, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda operator_assign : (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> { }"
			"}")));
	}

	TEST(MoveAssignment, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> {}"
			"}")));
	}

	TEST(MoveAssignment, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> { return; }"
			"}")));
	}

	TEST(ConstructorsAndAssignments, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() {}"
			"  ctor(other: ref<class,t,f,cpp_ref>) {}"
			"  ctor(other: ref<class,t,f,cpp_rref>) {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_ref>) -> ref<class,f,f,cpp_ref> {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> {}"
			"}")));
	}

	TEST(ConstructorsAndAssignmentsWithNonTrivialBaseCopyConstructor, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor(other: ref<base_class,t,f,cpp_ref>) { return; }"
			"} in struct class : [public base] {"
			"  ctor() {}"
			"  ctor(other: ref<class,t,f,cpp_ref>) {}"
			"  ctor(other: ref<class,t,f,cpp_rref>) {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_ref>) -> ref<class,f,f,cpp_ref> {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> {}"
			"}")));
	}

	TEST(ConstructorsAndAssignmentsWithNonTrivialBaseCopyAssignment, DISABLED_UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  lambda operator_assign : (rhs: ref<base_class,f,f,cpp_ref>) -> ref<nase_class,f,f,cpp_ref> { return; }"
			"} in struct class : [public base] {"
			"  ctor() {}"
			"  ctor(other: ref<class,t,f,cpp_ref>) {}"
			"  ctor(other: ref<class,t,f,cpp_rref>) {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_ref>) -> ref<class,f,f,cpp_ref> {}"
			"  lambda operator_assign : (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> {}"
			"}")));
	}

	TEST(TrivialStructWithNonTrivialBaseCopyConstructor, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor(other: ref<base_class,t,f,cpp_ref>) { return; }"
			"} in struct class : [public base] { }")));
	}

	TEST(TrivialStructWithNonTrivialBaseCopyAssignment, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  lambda operator_assign : (rhs: ref<base_class,f,f,cpp_ref>) -> ref<base_class,f,f,cpp_ref> { return; }"
			"} in struct class : [public base] { }")));
	}

	/*
	TEST(GlobalRec, InitBug) {
	    NodeManager manager;
	    IRBuilder builder(manager);
	    std::map<string, NodePtr> symbols;
	    auto recType = builder.parseType("let type0 = struct { ref<array<type0,1>> s; } in type0");
	    symbols["recTy"] = recType;

	    auto init = builder.parseExpr("ref_reinterpret(ref_null, lit(type<array<recTy,1>>))",symbols);
	    //std::cout << init << std::endl;

	    auto structExpr = builder.structExpr(toVector(builder.namedValue("s",init)));
	    //std::cout << structExpr << std::endl;

	    ExpressionList elements;
	    elements.push_back(structExpr);

	    auto vecPartialInit = builder.callExpr(
	        builder.getLangBasic().getVectorInitPartial(),
	        core::encoder::toIR(manager, elements),
	        builder.getIntParamLiteral(3));
	    //std::cout << vecPartialInit<< std::endl;

	    auto global = builder.literal("global", builder.parseType("ref<vector<recTy,3>>", symbols));
	    //std::cout << global << std::endl;

	    auto assign= builder.assign(global,vecPartialInit);
	    //std::cout << assign << std::endl;

	    auto semanticErrors = insieme::core::checks::check(assign);
	    std::cout << semanticErrors << std::endl;
	    EXPECT_TRUE(semanticErrors.empty());
	}
	*/


} // end namespace analysis
} // end namespace core
} // end namespace insieme
