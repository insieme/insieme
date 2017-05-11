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
#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/name_mangling.h"

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

	TEST(TrivialType, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("ref<int<4>>")));
	}

	TEST(ArrayOfTrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("array<int<4>,1>")));
	}

	TEST(ArrayOfNonTrivialType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType("array<struct s { ctor() { return; } },1u>")));
	}

	TEST(TrivialStruct, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("struct class {}")));
	}

	TEST(TrivialUnion, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType("union { data: int<4>; }")));
	}

	TEST(TrivialMember, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a struct with a trivial member must also be trivial, iff no user-defined ctor is in place
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: int<4>;"
			"}")));
	}

	TEST(TrivialMember, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a member of type reference is non-trivial
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: ref<'a,f,f>;"
			"}")));
	}

	TEST(NonTrivialMember, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		// a member of type reference without initialization is non-trivial
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  data: ref<'a,f,f,cpp_ref>;"
			"}")));
	}

	TEST(TrivialMemberNested, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  data: struct {"
			"    data: ref<'a,f,f>;"
			"  };"
			"}")));
	}

	TEST(NonTrivialMemberNested, Reference) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  data: struct {"
			"    data: ref<'a,f,f,cpp_ref>;"
			"  };"
			"}")));
	}

	TEST(NonVirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda fn = () -> unit { }"
			"}")));
	}

	TEST(VirtualLambda, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  virtual lambda fn = () -> unit { }"
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

	TEST(VirtualDestructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  dtor virtual() {}"
			"}")));
	}

	TEST(Constructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"}")));
	}

	TEST(Constructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
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

	TEST(CopyConstructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"}")));
	}

	TEST(MoveConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,f,f,cpp_rref>) { return; }"
			"}")));
	}

	TEST(MoveConstructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"}")));
	}

	TEST(Constructors, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"  ctor(other: ref<class,t,f,cpp_ref>) { return; }"
			"  ctor(other: ref<class,f,f,cpp_rref>) { return; }"
			"}")));
	}

	TEST(Constructors, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"}")));
	}

	TEST(NonTrivialConstructor, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() { return; }"
			"}")));
	}

	TEST(TrivialStructWithTrivialBase, Basic) {
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
			"  ctor() { return; }"
			"} in struct class : [public base] {}")));
	}

	TEST(CopyAssignment, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> { return; }"
			"}")));
	}

	TEST(CopyAssignment, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(MoveAssignment, UserDefined) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> { return; }"
			"}")));
	}

	TEST(MoveAssignment, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(ConstructorsAndAssignments, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_TRUE(isTrivial(builder.parseType(
			"struct class {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(ConstructorsAndAssignmentsWithNonTrivialBaseCopyConstructor, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  ctor(other: ref<base_class,t,f,cpp_ref>) { return; }"
			"} in struct class : [public base] {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,f,f,cpp_rref>) = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
			"}")));
	}

	TEST(ConstructorsAndAssignmentsWithNonTrivialBaseCopyAssignment, UserDefaulted) {
		NodeManager manager;
		IRBuilder builder(manager);
		EXPECT_FALSE(isTrivial(builder.parseType(
			"let base = struct base_class {"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<base_class,t,f,cpp_ref>) -> ref<base_class,f,f,cpp_ref> { return; }"
			"} in struct class : [public base] {"
			"  ctor() = default;"
			"  ctor(other: ref<class,t,f,cpp_ref>) = default;"
			"  ctor(other: ref<class,t,f,cpp_rref>) = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,t,f,cpp_ref>) -> ref<class,f,f,cpp_ref> = default;"
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<class,f,f,cpp_rref>) -> ref<class,f,f,cpp_ref> = default;"
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
			"  lambda " + utils::getMangledOperatorAssignName() + " = (rhs: ref<base_class,t,f,cpp_ref>) -> ref<base_class,f,f,cpp_ref> { 5; return ref_cast(this, type_lit(f), type_lit(f), type_lit(cpp_ref)); }"
			"} in struct class : [public base] { }")));
	}

	TEST(IsaDefaultConstructor, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto thisType = builder.refType(builder.tagTypeReference("A"));
		auto parents = builder.parents();
		auto fields = builder.fields();
		auto defaultCtor = builder.getDefaultConstructor(thisType, parents, fields).as<ExpressionPtr>();
		auto defaultDtor = builder.getDefaultDestructor(thisType);
		auto record = builder.structType("A", parents, fields, builder.expressions(toVector(defaultCtor)), defaultDtor, false, builder.memberFunctions(), builder.pureVirtualMemberFunctions());

		{
			auto ctor = builder.getDefaultConstructor(thisType, parents, fields).as<ExpressionPtr>();
			EXPECT_TRUE(isaDefaultConstructor(ctor));
		}

		{
			auto ctor = builder.getDefaultCopyConstructor(thisType, parents, fields).as<ExpressionPtr>();
			EXPECT_TRUE(isaDefaultConstructor(ctor));
		}

		{
			auto ctor = builder.getDefaultMoveConstructor(thisType, parents, fields).as<ExpressionPtr>();
			EXPECT_TRUE(isaDefaultConstructor(ctor));
		}

		{
			auto funType = builder.functionType(toVector(thisType.as<TypePtr>()), FK_CONSTRUCTOR);
			auto ctor = builder.lambdaExpr(funType, builder.parameters(toVector(builder.variable(thisType))), builder.getNoOp());
			EXPECT_FALSE(isaDefaultConstructor(ctor));
		}
	}

	TEST(HasDefaultDestructor, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto thisType = builder.refType(builder.tagTypeReference("A"));
		auto parents = builder.parents();
		auto fields = builder.fields();
		auto defaultCtor = builder.getDefaultConstructor(thisType, parents, fields).as<ExpressionPtr>();
		auto defaultDtor = builder.getDefaultDestructor(thisType);

		{
			auto record = builder.structType("A", parents, fields, builder.expressions(toVector(defaultCtor)), defaultDtor, false, builder.memberFunctions(), builder.pureVirtualMemberFunctions());
			EXPECT_TRUE(hasDefaultDestructor(record));
		}

		{
			auto funType = builder.functionType(toVector(thisType.as<TypePtr>()), FK_DESTRUCTOR);
			auto dtor = builder.lambdaExpr(funType, builder.parameters(toVector(builder.variable(thisType))), builder.getNoOp());
			auto record = builder.structType("A", parents, fields, builder.expressions(toVector(defaultCtor)), dtor, false, builder.memberFunctions(), builder.pureVirtualMemberFunctions());
			EXPECT_FALSE(hasDefaultDestructor(record));
		}
	}

	TEST(IsaDefaultMember, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto thisType = builder.refType(builder.tagTypeReference("A"));
		auto parents = builder.parents();
		auto fields = builder.fields();
		auto defaultCtor = builder.getDefaultConstructor(thisType, parents, fields).as<ExpressionPtr>();
		auto defaultDtor = builder.getDefaultDestructor(thisType);
		auto record = builder.structType("A", parents, fields, builder.expressions(toVector(defaultCtor)), defaultDtor, false, builder.memberFunctions(), builder.pureVirtualMemberFunctions());

		{
			auto member = builder.getDefaultCopyAssignOperator(thisType, parents, fields);
			EXPECT_TRUE(core::analysis::isaDefaultMember(member));
		}

		{
			auto member = builder.getDefaultMoveAssignOperator(thisType, parents, fields);
			EXPECT_TRUE(core::analysis::isaDefaultMember(member));
		}

		{
			auto funType = builder.functionType(toVector(thisType.as<TypePtr>()), FK_MEMBER_FUNCTION);
			auto member = builder.memberFunction(false, "foo", builder.lambdaExpr(funType, builder.parameters(toVector(builder.variable(thisType))), builder.getNoOp()));
			EXPECT_FALSE(core::analysis::isaDefaultMember(member));
		}

		{
			auto funType = builder.functionType(toVector(thisType.as<TypePtr>()), FK_MEMBER_FUNCTION);
			auto member = builder.memberFunction(false, utils::getMangledOperatorAssignName(), builder.lambdaExpr(funType, builder.parameters(toVector(builder.variable(thisType))), builder.getNoOp()));
			EXPECT_FALSE(core::analysis::isaDefaultMember(member));
		}
	}

	TEST(EssentialsChecks, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto A = builder.parseType("struct A { }").isa<TagTypePtr>();
		EXPECT_TRUE(A);
		bool(*hD1)(const TagTypePtr&) = hasDefaultDestructor;
		bool(*hD2)(const RecordPtr&) = hasDefaultDestructor;
		EXPECT_PRED1(hasDefaultConstructor, A);
		EXPECT_PRED1(hasCopyConstructor, A);
		EXPECT_PRED1(hasMoveConstructor, A);
		EXPECT_PRED1(hD1, A);
		EXPECT_PRED1(hD2, A->getRecord());

		EXPECT_PRED1(hasCopyAssignment, A);
		EXPECT_PRED1(hasMoveAssignment, A);
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

	TEST(GenericTypeVariable, Normalize) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};
		auto var = [&](const std::string& code) {
			return type(code).as<GenericTypeVariablePtr>();
		};

		EXPECT_EQ(type("'a<>"), normalize(var("'a<>")));
		EXPECT_EQ(type("'a<'_>"), normalize(var("'a<'b>")));
		EXPECT_EQ(type("'a<'_,'_>"), normalize(var("'a<'b,'c>")));
		EXPECT_EQ(type("'a<'_,'_,'_>"), normalize(var("'a<'b,'c,'d>")));

		EXPECT_EQ(type("'a<'_,'_>"), normalize(var("'a<A,B>")));
		EXPECT_EQ(type("'a<'_,'_>"), normalize(var("'a<A,B<C,D>>")));

		EXPECT_EQ(type("'a<'_,'_...>"), normalize(var("'a<A,'b...>")));

		EXPECT_EQ(type("'a<'_,'_<'_,'_>>"), normalize(var("'a<A,'b<'c,'d>>")));
		EXPECT_EQ(type("'a<'_,'_...<'_,'_>>"), normalize(var("'a<A,'b...<'c,'d>>")));

		EXPECT_EQ(type("'a<'_,'_...<'_,'_<'_>>>"), normalize(var("'a<A,'b...<'c,'d<'e>>>")));

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
