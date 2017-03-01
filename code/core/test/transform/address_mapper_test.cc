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

#include "insieme/core/transform/sequentialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/address_mapper.h"



namespace insieme {
namespace core {
namespace transform {

	TEST(AddressMapper, Simplest) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement("5 + $4$;");

		EXPECT_EQ(addresses.size(), 1);

		auto mapper = makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddress) -> NodePtr {
			if(addresses[0] == prevAddress) { return builder.intLit(31337).as<NodePtr>(); }
			return builtPtr;
		});

		auto result = mapper.mapFromRoot(addresses[0].getRootNode());
		EXPECT_EQ(result, builder.parseExpr("5+31337"));
	}

	TEST(AddressMapper, Simple) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(R"raw(
		alias int = int<4>;
		{		
			var ref<int,f,f,plain> x = 2;
			var ref<int,f,f,plain> a = $2$+$2$;
			var ref<int,f,f,plain> b = a + 6;
		}
	)raw");

		EXPECT_EQ(addresses.size(), 2);

		auto mapper = makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddress) -> NodePtr {
			if(addresses[0] == prevAddress) { return builder.intLit(31337).as<NodePtr>(); }
			if(addresses[1] == prevAddress) { return builder.intLit(42).as<NodePtr>(); }
			return builtPtr;
		});

		auto result = mapper.mapFromRoot(addresses[0].getRootNode());
		EXPECT_TRUE(core::analysis::contains(result, builder.parseExpr("31337+42")));
		EXPECT_TRUE(core::analysis::contains(result, builder.parseExpr("2")));
	}

	TEST(AddressMapper, Nested) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = builder.getLangBasic();

		auto addresses = builder.parseAddressesStatement(R"raw(
	alias int = int<4>;
	{		
		$$(a : int, b : int) -> int {
			var ref<int,f,f,plain> ret = $a * b + a$;
			return *ret;
		}$(4,2)$;
	}
	)raw");

		EXPECT_EQ(addresses.size(), 3);
		const auto callAddr = addresses[0];
		const auto funAddr = addresses[1];
		const auto exprAddr = addresses[2];

		// what we do here is add a new variable to the lambda, its call, and use it in the body
		auto varToAdd = builder.variable(builder.refType(basic.getInt4()), 10042);
		auto refDeref = mgr.getLangExtension<lang::ReferenceExtension>().getRefDeref();
		auto mapper = makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddr) -> NodePtr {
			if(prevAddr == exprAddr) { return builder.sub(builtPtr.as<ExpressionPtr>(), builder.callExpr(refDeref, varToAdd)); }
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
		EXPECT_EQ(
		    R"raw({rec _.{_=fun(ref<int<4>,f,f,plain> v0, ref<int<4>,f,f,plain> v1, ref<int<4>,f,f,plain> v2) {ref<int<4>,f,f,plain> v3 = int_sub(int_add(int_mul(ref_deref(v0), ref_deref(v1)), ref_deref(v0)), ref_deref(v2)); return ref_deref(v3);}}(4, 2, 31337);})raw",
		    result->toString());
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
