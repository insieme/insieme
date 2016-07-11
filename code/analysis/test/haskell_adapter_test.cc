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

#include <gtest/gtest.h>

#include "insieme/analysis/haskell/adapter.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump;

namespace insieme {
namespace analysis {
namespace haskell {

	struct Env {
		Environment& env;
		Env() : env(Environment::getInstance()) {}
	};

	struct SimpleDeclaration {

		NodeManager manager;
		IRBuilder builder;
		NodePtr root;

		SimpleDeclaration() : builder(manager) {
			root = builder.parseStmt(
				"{ "
				"   var int<4> x = 12; "
				"   2 + 3; "
				"   x; "
				"} "
			);
		}

	};

	class HaskellAdapter :
		public ::testing::Test,
		public Env,
		public SimpleDeclaration {};

	TEST_F(HaskellAdapter, NodeCount) {
		auto tree = env.passTree(root);

		// calculate overall node count
		size_t nodeCount = 0;
		visitDepthFirst(NodeAddress(root), [&] (const NodeAddress& n) {
				nodeCount++;
		}, true, true);

		EXPECT_EQ(nodeCount, tree.size());
	}

	TEST_F(HaskellAdapter, AddressLength) {
		auto tree = env.passTree(root);
		NodeAddress addr = NodeAddress(root).getAddressOfChild(0, 0, 0, 0);
		Address addr_hs = env.passAddress(addr, tree);
		EXPECT_EQ(addr.getDepth(), addr_hs.size() + 1);
	}

	TEST_F(HaskellAdapter, AddressTransfere) {
		auto tree = env.passTree(root);
		NodeAddress addr = NodeAddress(root).getAddressOfChild(1, 0, 1);
		Address addr_hs = env.passAddress(addr, tree);
		NodeAddress addr_res = addr_hs.toNodeAddress(root);
		EXPECT_EQ(addr,addr_res);
	}

	TEST_F(HaskellAdapter, FindDeclaration) {
		auto tree = env.passTree(root);

		// get the targeted variable
		CompoundStmtAddress addrRoot(root.as<CompoundStmtPtr>());
		StatementAddress addrVar = addrRoot[2];
		EXPECT_TRUE(addrVar.isa<VariableAddress>());

		auto var = env.passAddress(addrVar, tree);

		boost::optional<Address> decl = env.findDeclr(var);
		EXPECT_TRUE(decl);
		EXPECT_EQ(addrRoot[0].as<DeclarationStmtAddress>().getVariable(), decl->toNodeAddress(root));
	}

	TEST_F(HaskellAdapter, DISABLED_PrintTree) {
		// This test is only used to generate tree for further debugging
		StatementPtr stmt = builder.parseStmt(R"1N5P1RE(

				decl lfun : (ref<int<4>>)->int<4>;
				def fun = (arg : int<4>)->int<4> { return arg + 1; };
				def rfun = (arg : ref<int<4>>)->int<4> { return *arg;};
			{
				var ref<int<4>> a;
				var ref<int<4>> b;
				var ref<int<4>> c;
				var ref<int<4>> d;
				var ref<int<4>> e;
				var ref<int<4>> f;
				var ref<int<4>> g;
				{
					a = 7;
					fun(*b);
					rfun(c);
					fun(fun(*d));
					fun(rfun(e));
					lfun(f);
					rfun(ref_temp_init(lfun(g)));
				}
			}
		)1N5P1RE");

		auto tree = env.passTree(stmt);
		tree.print();
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme
