/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(IRppUtils, IsCTorTest) {
		NodeManager man;
		IRBuilder builder(man);

		auto call = builder.parseExpr(R"(
			def struct Whatever { };
			Whatever::(ref_temp(type_lit(Whatever)))
		)");

		EXPECT_TRUE(isConstructorCall(call));
	}

	TEST(IRppUtils, DefaultedMembers) {
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

	TEST(IRppUtils, isIRpp) {
		// checking, if, starting from a nodePtr, there are some object orientated constructs
		NodeManager nm;
		IRBuilder b(nm);

		auto ir1 = b.parseType("def struct A {}; def struct B : [ public A ] {}; B");
		EXPECT_TRUE(isIRpp(ir1));

		auto ir2 = b.parseType("def struct A { ctor (v1: ref<int<4>>) {}}; A");
		EXPECT_TRUE(isIRpp(ir2));

		auto ir3 = b.parseStmt("{ throw 1; }");
		EXPECT_TRUE(isIRpp(ir3));

		auto ir4 = b.parseStmt("{ try {1;} catch (v1 : ref<int<4>>) {2;} }");
		EXPECT_TRUE(isIRpp(ir4));

		auto ir5 = b.parseType("int<4>");
		EXPECT_FALSE(isIRpp(ir5));

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
