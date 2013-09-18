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

#include "insieme/core/ir_builder.h"

#include "insieme/analysis/cba/framework/call_site_manager.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA_Call_Site_Mgr, DirectCalls) {

		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		auto code = builder.parseStmt(
				"{"
				"	let f = ()->unit {};"
				"	f();"				// a function call
				"	()=> 2 ();"			// a bind call
				"	1 + 2;"				// a literal
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		// extract caller and callee
		Caller callerA(root[0].as<CallExprAddress>());
		Callee calleeA(root[0].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>()->getLambda());

		Caller callerB(root[1].as<CallExprAddress>());
		Callee calleeB(root[1].as<CallExprAddress>()->getFunctionExpr().as<BindExprAddress>());

		Caller callerC(root[2].as<CallExprAddress>());
		Callee calleeC(root[2].as<CallExprAddress>()->getFunctionExpr().as<LiteralAddress>());

		EXPECT_TRUE(calleeA.isLambda());
		EXPECT_TRUE(calleeB.isBind());
		EXPECT_TRUE(calleeC.isLiteral());

		// create call site manager
		CallSiteManager mgr(root);

		// check direct connection
		EXPECT_EQ("[(Lambda@0-0-1-2-0-1)]", toString(mgr.getCallee(callerA)));
		EXPECT_EQ("[(0-0)]", toString(mgr.getCaller(calleeA)));

		EXPECT_EQ("[(BindExpr@0-1-1)]", toString(mgr.getCallee(callerB)));
		EXPECT_EQ("[(0-1)]", toString(mgr.getCaller(calleeB)));

		EXPECT_EQ("[int.add]", toString(mgr.getCallee(callerC)));
		EXPECT_EQ("[(0-2)]", toString(mgr.getCaller(calleeC)));
	}

	TEST(CBA_Call_Site_Mgr, IndirectVariableCalls) {

		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		auto code = builder.parseStmt(
				"{"
				"	auto f = ()->unit {};"
				"	f();"				// a function call

				"	auto g = ()=> 2;"
				"	g();"				// a bind call

				"	auto h = int.add;"
				"	h(2,3);"			// a literal
				"	h(1,4);"			// used multiple times

				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		// extract caller and callee
		Caller callerA(root[1].as<CallExprAddress>());
		Callee calleeA(root[0].as<DeclarationStmtAddress>()->getInitialization().as<LambdaExprAddress>()->getLambda());

		Caller callerB(root[3].as<CallExprAddress>());
		Callee calleeB(root[2].as<DeclarationStmtAddress>()->getInitialization().as<BindExprAddress>());

		Caller callerC(root[5].as<CallExprAddress>());
		Callee calleeC(root[4].as<DeclarationStmtAddress>()->getInitialization().as<LiteralAddress>());

		EXPECT_TRUE(calleeA.isLambda());
		EXPECT_TRUE(calleeB.isBind());
		EXPECT_TRUE(calleeC.isLiteral());

		// create call site manager
		CallSiteManager mgr(root);

		// check direct connection
		EXPECT_EQ("[(Lambda@0-0-1-2-0-1)]", toString(mgr.getCallee(callerA)));
		EXPECT_EQ("[(0-1)]", toString(mgr.getCaller(calleeA)));

		EXPECT_EQ("[(BindExpr@0-2-1)]", toString(mgr.getCallee(callerB)));
		EXPECT_EQ("[(0-3)]", toString(mgr.getCaller(calleeB)));

		EXPECT_EQ("[int.add]", toString(mgr.getCallee(callerC)));
		EXPECT_EQ("[(0-5),(0-6)]", toString(mgr.getCaller(calleeC)));
	}

	// TODO: indirect calls via parameters

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
