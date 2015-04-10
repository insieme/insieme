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

#include <gtest/gtest.h>

#include "insieme/core/transform/sequentialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/address_mapper.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace core {
namespace transform {

TEST(AddressMapper, Simplest) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(
			"5 + $4$;"
	);

	EXPECT_EQ(addresses.size(), 1);

	auto mapper = makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddress) -> NodePtr {
		if(addresses[0] == prevAddress) {
			return builder.intLit(31337).as<NodePtr>();
		}
		return builtPtr;
	});

	auto result = mapper.mapFromRoot(addresses[0].getRootNode());
	EXPECT_EQ(result, builder.parseExpr("5+31337"));
}

TEST(AddressMapper, Simple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
		{		
			let int = int<4>;
			ref<int> x = var(2);
			ref<int> a = var($2$+$2$);
			ref<int> b = var(a + 6);
		}
	)raw");

	EXPECT_EQ(addresses.size(), 2);

	auto mapper = makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddress) -> NodePtr {
		if(addresses[0] == prevAddress) {
			return builder.intLit(31337).as<NodePtr>();
		} 
		if(addresses[1] == prevAddress) {
			return builder.intLit(42).as<NodePtr>();
		}
		return builtPtr;
	});

	auto result = mapper.mapFromRoot(addresses[0].getRootNode());
	EXPECT_TRUE(core::analysis::contains(result, builder.parseExpr("var(31337+42)")));
	EXPECT_TRUE(core::analysis::contains(result, builder.parseExpr("var(2)")));
}

TEST(AddressMapper, Nested) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	auto& basic = builder.getLangBasic();

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let int = int<4>;
		$$(int a, int b) -> int {
			ref<int> ret = $a * b + a$;
			return *ret;
		}$(4,2)$;
	}
	)raw");

	EXPECT_EQ(addresses.size(), 3);
	const auto callAddr = addresses[0];
	const auto funAddr = addresses[1];
	const auto exprAddr = addresses[2];

	// what we do here is add a new variable to the lambda, its call, and use it in the body
	auto varToAdd = builder.variable(basic.getInt4(), 10042);
	auto mapper = makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddr) -> NodePtr {
		if(prevAddr == exprAddr) {
			return builder.sub(builtPtr.as<ExpressionPtr>(), varToAdd);
		} 
		if(prevAddr == funAddr) {
			auto curFun = builtPtr.as<LambdaExprPtr>();
			auto newFunType = builder.functionType(toVector(basic.getInt4(), basic.getInt4(), basic.getInt4()), curFun->getFunctionType()->getReturnType());
			auto parList = curFun->getLambda()->getParameterList();
			parList.push_back(varToAdd);
			return builder.lambdaExpr(newFunType, parList, curFun->getBody());
		}
		if(prevAddr == callAddr) {
			auto curCall = builtPtr.as<CallExprPtr>();
			return builder.callExpr(curCall->getFunctionExpr(), curCall->getArgument(0), curCall->getArgument(1), builder.intLit(31337));
		}
		return builtPtr;
	});

	auto result = builder.normalize(mapper.mapFromRoot(addresses[0].getRootNode()));
	EXPECT_EQ(result->toString(),
		R"raw({rec v0.{v0=fun(int<4> v1, int<4> v2, int<4> v3) {ref<int<4>> v4 = int.sub(int.add(int.mul(v1, v2), v1), v3); return ref.deref(v4);}}(4, 2, 31337);})raw");
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
