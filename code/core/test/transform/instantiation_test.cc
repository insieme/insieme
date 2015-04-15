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

#include "insieme/core/transform/instantiate.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace core {
namespace transform {

TEST(IntTypeParamInstantiation, Simple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test = (vector<'res,#l> a) -> unit {
			$vector<'res, #l> res;$
		};

		vector<int<4>, 8> a;
		test(a);
	}
	)raw");
	
	EXPECT_EQ(addresses.size(), 1);
	
	auto result = instantiateIntTypeParams(addresses[0].getRootNode());
	
	auto newAddr = addresses[0].switchRoot(result);
	EXPECT_EQ(builder.normalize(builder.parseStmt("vector<'res, 8> res;")), builder.normalize(newAddr.getAddressedNode()));
}

TEST(IntTypeParamInstantiation, Return) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test = $(vector<'res,#l> a) -> vector<'res,#l> {
			return a;
		}$;

		vector<int<4>, 8> a;
		test(a);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 1);

	auto result = instantiateIntTypeParams(addresses[0].getRootNode());

	auto newAddr = addresses[0].switchRoot(result);
	auto retType = newAddr.as<LambdaExprPtr>()->getFunctionType()->getReturnType();
	EXPECT_EQ(builder.normalize(builder.parseType("vector<'res, 8>")), builder.normalize(retType));
}

TEST(IntTypeParamInstantiation, Multiple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test = (vector<'res,#l> a, matrix<'res,#x,#y> b) -> unit {
			$vector<'res, #l> res;$
			$matrix<'res, #x, #y> res2;$
		};

		vector<int<4>, 8> a;
		matrix<int<4>, 16, 32> b;
		test(a, b);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 2);

	auto result = instantiateIntTypeParams(addresses[0].getRootNode());

	auto newAddrVec = addresses[0].switchRoot(result);
	auto newAddrMat = addresses[1].switchRoot(result);
	EXPECT_EQ(builder.normalize(builder.parseStmt("vector<'res, 8> res;")), builder.normalize(newAddrVec.getAddressedNode()));
	EXPECT_EQ(builder.normalize(builder.parseStmt("matrix<'res, 16, 32> res2;")), builder.normalize(newAddrMat.getAddressedNode()));
}

TEST(IntTypeParamInstantiation, Nested) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test_array = (vector<'res,#l> a) -> unit {
			$vector<'res, #l> res;$
		};
		let test_outer = (vector<'res,#l> a, matrix<'res,#x,#y> b) -> unit {
			test_array(a);
			$matrix<'res, #x, #y> res2;$
		};

		vector<int<4>, 8> a;
		matrix<int<4>, 16, 32> b;
		test_outer(a, b);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 2);

	auto result = instantiateIntTypeParams(addresses[0].getRootNode());

	auto newAddrVec = addresses[0].switchRoot(result);
	auto newAddrMat = addresses[1].switchRoot(result);
	EXPECT_EQ(builder.normalize(builder.parseStmt("vector<'res, 8> res;")), builder.normalize(newAddrVec.getAddressedNode()));
	EXPECT_EQ(builder.normalize(builder.parseStmt("matrix<'res, 16, 32> res2;")), builder.normalize(newAddrMat.getAddressedNode()));
}

TEST(TypeVariableInstantiation, Simple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{
		let test = ('a f) -> unit {
			auto y = f;
			let x = ('a a)->unit {
				auto z = a; 
				$z$; 
			};
			x(f);
		};
		int<4> x = 1;
		test(x);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 1);

	auto result = instantiateTypeVariables(addresses[0].getRootNode());
		
	auto newAddr = addresses[0].switchRoot(result);
	EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>()->getType());
}

TEST(TypeVariableInstantiation, ExpressionArgument) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{
		let test = ('a f) -> unit {
			auto y = f;
			let x = ('a a)->unit {
				auto z = a; 
				$z$; 
			};
			x(f);
		};
		int<4> x = 1;
		test(x*5);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 1);

	auto result = instantiateTypeVariables(addresses[0].getRootNode());

	auto newAddr = addresses[0].switchRoot(result);
	EXPECT_EQ(builder.parseType("int<4>"), newAddr.getAddressedNode().as<ExpressionPtr>()->getType());
}

TEST(TypeInstantiation, Simple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test = (vector<'res,#l> a) -> unit {
			$vector<'res, #l> res;$
		};

		vector<int<4>, 8> a;
		test(a);
	}
	)raw");
	
	EXPECT_EQ(addresses.size(), 1);
	
	auto result = instantiateTypes(addresses[0].getRootNode());
	
	auto newAddr = addresses[0].switchRoot(result);
	EXPECT_EQ(builder.normalize(builder.parseStmt("vector<int<4>, 8> res;")), builder.normalize(newAddr.getAddressedNode()));
}


TEST(TypeInstantiation, NameAnnotations) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test = (vector<'res,#l> a) -> unit {
			$vector<'res, #l> res;$
			int<4> x = 0;
			$x$;
			$res$;
		};

		vector<int<4>, 8> a;
		test(a);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 3);

	auto result = instantiateTypes(addresses[0].getRootNode());

	auto newAddr = addresses[0].switchRoot(result);
	auto newAnnAddr = addresses[1].switchRoot(result);
	auto newAnnAddr2 = addresses[2].switchRoot(result);
	EXPECT_EQ(builder.normalize(builder.parseStmt("vector<int<4>, 8> res;")), builder.normalize(newAddr.getAddressedNode()));
	EXPECT_TRUE(annotations::hasAttachedName(addresses[1].getAddressedNode()));
	EXPECT_TRUE(annotations::hasAttachedName(newAnnAddr.getAddressedNode()));
	EXPECT_TRUE(annotations::hasAttachedName(newAnnAddr2.getAddressedNode()));
	EXPECT_EQ(annotations::getAttachedName(addresses[1].getAddressedNode()), annotations::getAttachedName(newAnnAddr.getAddressedNode()));
	EXPECT_EQ(annotations::getAttachedName(addresses[2].getAddressedNode()), annotations::getAttachedName(newAnnAddr2.getAddressedNode()));
}

TEST(TypeInstantiation, TypeAnnotations) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{		
		let test = (vector<'res,#l> a) -> unit {
			$vector<'res, #l> res;$
		};

		vector<int<4>, 8> a;
		test(a);
	}
	)raw");

	EXPECT_EQ(addresses.size(), 1);

	auto declStmtType = addresses[0].getAddressedNode().as<DeclarationStmtPtr>()->getVariable()->getType();
	annotations::attachName(declStmtType, "NewtypeGundam");

	auto result = instantiateIntTypeParams(addresses[0].getRootNode());

	auto newAddr = addresses[0].switchRoot(result);
	auto newDeclStmtType = newAddr.getAddressedNode().as<DeclarationStmtPtr>()->getVariable()->getType();
	EXPECT_TRUE(annotations::hasAttachedName(newDeclStmtType));
	EXPECT_EQ(annotations::getAttachedName(declStmtType), annotations::getAttachedName(newDeclStmtType));
	EXPECT_EQ("NewtypeGundam", annotations::getAttachedName(newDeclStmtType));
}

TEST(TypeInstantiation, AnnotationsOnCallExp) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(R"raw(
	{
		let test = (vector<'res,#l> a) -> unit {
			vector<'res, #l> res;
		};

		vector<int<4>, 8> a;
		$test(a)$;
	}
	)raw");

	EXPECT_EQ(addresses.size(), 1);

	//attach a dummy annotation to the callExpr and check whether it is still there after the types have been instantiated
	auto root = addresses[0].getRootNode();
	annotations::attachLocation(addresses[0].getAddressedNode(), "dummy", 0, 1, 2, 3);
	EXPECT_TRUE(annotations::hasAttachedLocation(addresses[0].getAddressedNode()));

	auto result = instantiateTypes(root);
	auto newAddr = addresses[0].switchRoot(result);
	EXPECT_TRUE(annotations::hasAttachedLocation(newAddr.getAddressedNode()));
	EXPECT_EQ(annotations::getLocation(addresses[0].getAddressedNode()), annotations::getLocation(newAddr.getAddressedNode()));
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
