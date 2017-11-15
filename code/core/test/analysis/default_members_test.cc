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

#include "insieme/core/analysis/default_members.h"

#include "insieme/core/ir_builder.h"

#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace core {
namespace analysis {

	TEST(IsaDefaultConstructor, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto thisType = builder.refType(builder.tagTypeReference("A"));
		auto parents = builder.parents();
		auto fields = builder.fields();

		{
			auto ctor = analysis::buildDefaultDefaultConstructor(thisType, parents, fields).as<ExpressionPtr>();
			EXPECT_TRUE(isaDefaultConstructor(ctor));
		}

		{
			auto ctor = analysis::buildDefaultCopyConstructor(thisType, parents, fields).as<ExpressionPtr>();
			EXPECT_TRUE(isaDefaultConstructor(ctor));
		}

		{
			auto ctor = analysis::buildDefaultMoveConstructor(thisType, parents, fields).as<ExpressionPtr>();
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
		auto defaultCtor = analysis::buildDefaultDefaultConstructor(thisType, parents, fields).as<ExpressionPtr>();
		auto defaultDtor = analysis::buildDefaultDestructor(thisType, parents, fields);

		{
			auto record = builder.structType("A", parents, fields, builder.expressions(toVector(defaultCtor)), defaultDtor, false, builder.memberFunctions(), builder.pureVirtualMemberFunctions());
			EXPECT_TRUE(isaDefaultDestructor(getDestructor(record)));
		}

		{
			auto funType = builder.functionType(toVector(thisType.as<TypePtr>()), FK_DESTRUCTOR);
			auto dtor = builder.lambdaExpr(funType, builder.parameters(toVector(builder.variable(thisType))), builder.getNoOp());
			auto record = builder.structType("A", parents, fields, builder.expressions(toVector(defaultCtor)), dtor, false, builder.memberFunctions(), builder.pureVirtualMemberFunctions());
			EXPECT_FALSE(isaDefaultDestructor(getDestructor(record)));
		}
	}

	TEST(IsaDefaultMember, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto thisType = builder.refType(builder.tagTypeReference("A"));
		auto parents = builder.parents();
		auto fields = builder.fields();

		{
			auto member = analysis::buildDefaultCopyAssignOperator(thisType, parents, fields);
			EXPECT_TRUE(core::analysis::isaDefaultMember(member));
		}

		{
			auto member = analysis::buildDefaultMoveAssignOperator(thisType, parents, fields);
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
		EXPECT_PRED1(hasDefaultConstructor, A);
		EXPECT_PRED1(hasCopyConstructor, A);
		EXPECT_PRED1(hasMoveConstructor, A);
		EXPECT_PRED1(hasDestructor, A);
		EXPECT_PRED1(hasCopyAssignment, A);
		EXPECT_PRED1(hasMoveAssignment, A);
	}

	TEST(DefaultedMembers, Basic) {
		NodeManager man;
		IRBuilder builder(man);

		auto addresses = builder.parseAddressesStatement(R"(
			def struct S { };
			def struct O {
				ctor () { "Nonstandard"; }
				lambda f = () -> unit {}
			};
			{
				var ref<S> s = $S::(s)$;
				$S::(s, ref_cast(s, type_lit(t), type_lit(f), type_lit(cpp_ref)))$;
				$s.)" + utils::getMangledOperatorAssignName() + R"((ref_cast(s, type_lit(t), type_lit(f), type_lit(cpp_ref)))$;
				$s.)" + utils::getMangledOperatorAssignName() + R"((ref_kind_cast(s, type_lit(cpp_rref)))$;

				var ref<O> o = $O::(o)$;
				$O::(o, ref_cast(o, type_lit(t), type_lit(f), type_lit(cpp_ref)))$;
				$o.)" + utils::getMangledOperatorAssignName() + R"((ref_cast(o, type_lit(t), type_lit(f), type_lit(cpp_ref)))$;
				$o.f()$;
			}
		)");

		auto extractLambda = [](const NodeAddress& addr) {
			return addr.getAddressedNode().as<CallExprPtr>()->getFunctionExpr().as<LambdaExprPtr>();
		};

		ASSERT_EQ(8, addresses.size());

		EXPECT_TRUE (isaDefaultMember(extractLambda(addresses[0]))); // default ctor
		EXPECT_TRUE (isaDefaultMember(extractLambda(addresses[1]))); // copy ctor
		EXPECT_TRUE (isaDefaultMember(extractLambda(addresses[2]))); // copy assignment
		EXPECT_TRUE (isaDefaultMember(extractLambda(addresses[3]))); // move assignment

		EXPECT_FALSE(isaDefaultMember(extractLambda(addresses[4]))); // non-default ctor
		EXPECT_TRUE (isaDefaultMember(extractLambda(addresses[5]))); // copy ctor
		EXPECT_TRUE (isaDefaultMember(extractLambda(addresses[6]))); // copy assignment
		EXPECT_FALSE(isaDefaultMember(extractLambda(addresses[7]))); // member function
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
