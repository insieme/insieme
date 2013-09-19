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
				"	h(1,h(2,3));"		// used multiple times

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
		EXPECT_EQ("[(0-5),(0-6),(0-6-3)]", toString(mgr.getCaller(calleeC)));

		// - the same, yet not string based
		EXPECT_EQ(toString(toVector(calleeA)), toString(mgr.getCallee(callerA)));
		EXPECT_EQ(toString(toVector(callerA)), toString(mgr.getCaller(calleeA)));

		EXPECT_EQ(toString(toVector(calleeB)), toString(mgr.getCallee(callerB)));
		EXPECT_EQ(toString(toVector(callerB)), toString(mgr.getCaller(calleeB)));

		EXPECT_EQ(toString(toVector(calleeC)), toString(mgr.getCallee(callerC)));
	}

	TEST(CBA_Call_Site_Mgr, IndirectParameterCalls) {

		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		auto code = builder.parseStmt(
				"{"
				"	let f = (()=>unit g)->unit { g(); };"

				"	f(()->unit { });"			// a function passing as an argument
				"	f(()=> 2);"					// a bind passed as an argument
				"	f(lit(\"x\":()->unit));"	// a literal passed as an argument

				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		// extract caller and callee
		Caller callerA(root[0].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>().getBody()[0].as<CallExprAddress>());
		Callee calleeA(root[0].as<CallExprAddress>()[0].as<LambdaExprAddress>()->getLambda());

		Caller callerB(root[1].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>().getBody()[0].as<CallExprAddress>());
		Callee calleeB(root[1].as<CallExprAddress>()[0].as<BindExprAddress>());

		Caller callerC(root[2].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>().getBody()[0].as<CallExprAddress>());
		Callee calleeC(root[2].as<CallExprAddress>()[0].as<LiteralAddress>());

		EXPECT_TRUE(calleeA.isLambda());
		EXPECT_TRUE(calleeB.isBind());
		EXPECT_TRUE(calleeC.isLiteral());

		// create call site manager
		CallSiteManager mgr(root);

		// check direct connection
		EXPECT_EQ(toString(toVector(calleeA)), toString(mgr.getCallee(callerA)));
		EXPECT_EQ(toString(toVector(callerA)), toString(mgr.getCaller(calleeA)));

		EXPECT_EQ(toString(toVector(calleeB)), toString(mgr.getCallee(callerB)));
		EXPECT_EQ(toString(toVector(callerB)), toString(mgr.getCaller(calleeB)));

		EXPECT_EQ(toString(toVector(calleeC)), toString(mgr.getCallee(callerC)));
		EXPECT_EQ(toString(toVector(callerC)), toString(mgr.getCaller(calleeC)));
	}

	TEST(CBA_Call_Site_Mgr, RecursiveCalls) {

		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		auto code = builder.parseStmt(
				"{"
				"	let f = (int x)->int { f(x); };"		// a recursive function
				"	f(3);"									// a direct call to a recursive function

				"	auto g = f;"
				"	g(4);"									// an indirect call to a recursive function

				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		// extract caller and callee
		Caller callerA1(root[0].as<CallExprAddress>());
		Caller callerA2(root[0].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>().getBody()[0].as<CallExprAddress>());
		Callee calleeA (root[0].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>()->getLambda());

		EXPECT_TRUE(calleeA.isLambda());

		// create call site manager
		CallSiteManager mgr(root);

		// check direct connection
		EXPECT_EQ(toString(toVector(calleeA)), toString(mgr.getCallee(callerA1)));
		EXPECT_EQ(toString(toVector(calleeA)), toString(mgr.getCallee(callerA2)));
		EXPECT_EQ(toString(toVector(callerA1,callerA2)), toString(mgr.getCaller(calleeA)));

		// ----------------------

		// check indirect recursive call
		Callee calleeB (root[1].as<DeclarationStmtAddress>()->getInitialization().as<LambdaExprAddress>()->getLambda());
		Caller callerB1(root[2].as<CallExprAddress>());
		Caller callerB2(root[1].as<DeclarationStmtAddress>()->getInitialization().as<LambdaExprAddress>()->getLambda()->getBody()[0].as<CallExprAddress>());

		EXPECT_EQ(toString(toVector(calleeB)), toString(mgr.getCallee(callerB1)));
		EXPECT_EQ(toString(toVector(calleeB)), toString(mgr.getCallee(callerB2)));
		EXPECT_EQ(toString(toVector(callerB1,callerB2)), toString(mgr.getCaller(calleeB)));

	}

	TEST(CBA_Call_Site_Mgr, MutualRecursiveCalls) {

		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		auto code = builder.parseStmt(
				"{"
				"	let f,g = "
				"			(int x)->int { g(x); },"
				"			(int x)->int { f(x); };"		// tow mutually recursive functions
				"	f(3);"									// a direct call to a recursive function f
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		LambdaExprAddress l = root[0].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>();
		LambdaAddress f = l->getDefinition()[0]->getLambda();
		LambdaAddress g = l->getDefinition()[1]->getLambda();

		// extract caller and callee
		Caller callerA(root[0].as<CallExprAddress>());
		Caller callerG(f.getBody()[0].as<CallExprAddress>());
		Caller callerF(g.getBody()[0].as<CallExprAddress>());

		Callee calleeF (f);
		Callee calleeG (g);

		EXPECT_TRUE(calleeF.isLambda());
		EXPECT_TRUE(calleeG.isLambda());

		// create call site manager
		CallSiteManager mgr(root);

		// check direct connection
		EXPECT_EQ(toString(toVector(calleeF)), toString(mgr.getCallee(callerA)));
		EXPECT_EQ(toString(toVector(calleeF)), toString(mgr.getCallee(callerF)));
		EXPECT_EQ(toString(toVector(calleeG)), toString(mgr.getCallee(callerG)));

		EXPECT_EQ(toString(toVector(callerA,callerF)), toString(mgr.getCaller(calleeF))) << calleeF;
		EXPECT_EQ(toString(toVector(callerG)), toString(mgr.getCaller(calleeG))) << calleeG;

	}

	TEST(CBA_Call_Site_Mgr, MutualRecursiveCalls_2) {

		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		auto code = builder.parseStmt(
				"{"
				"	let f,g = "
				"			(int x)->int { g(x); f(x); },"
				"			(int x)->int { f(x); g(x); };"		// tow mutually recursive functions with mutliple recursive calls
				"	f(3);"										// a direct call to a recursive function f
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		LambdaExprAddress l = root[0].as<CallExprAddress>()->getFunctionExpr().as<LambdaExprAddress>();
		LambdaAddress f = l->getDefinition()[0]->getLambda();
		LambdaAddress g = l->getDefinition()[1]->getLambda();

		// extract caller and callee
		Caller callerA(root[0].as<CallExprAddress>());
		Caller callerG1(f.getBody()[0].as<CallExprAddress>());
		Caller callerG2(g.getBody()[1].as<CallExprAddress>());
		Caller callerF1(g.getBody()[0].as<CallExprAddress>());
		Caller callerF2(f.getBody()[1].as<CallExprAddress>());

		Callee calleeF (f);
		Callee calleeG (g);

		EXPECT_TRUE(calleeF.isLambda());
		EXPECT_TRUE(calleeG.isLambda());

		// create call site manager
		CallSiteManager mgr(root);

		// check direct connection
		EXPECT_EQ(toString(toVector(calleeF)), toString(mgr.getCallee(callerA)));
		EXPECT_EQ(toString(toVector(calleeF)), toString(mgr.getCallee(callerF1)));
		EXPECT_EQ(toString(toVector(calleeF)), toString(mgr.getCallee(callerF2)));
		EXPECT_EQ(toString(toVector(calleeG)), toString(mgr.getCallee(callerG1)));
		EXPECT_EQ(toString(toVector(calleeG)), toString(mgr.getCallee(callerG2)));

		EXPECT_EQ(toString(toVector(callerA,callerF2,callerF1)), toString(mgr.getCaller(calleeF))) << calleeF;
		EXPECT_EQ(toString(toVector(callerG1,callerG2)), toString(mgr.getCaller(calleeG))) << calleeF;

	}

	TEST(CBA_Call_Site_Mgr, OpenCall) {


		NodeManager nodeMgr;
		IRBuilder builder(nodeMgr);

		map<string, NodePtr> symbols;
		symbols["hide"] = builder.parseExpr("lit(\"hide\" : ('a)->unit)");
		symbols["get"]  = builder.parseExpr("lit(\"get\" : () -> ()->unit)");

		auto code = builder.parseStmt(
				"{"
				"	hide(()->unit {});"					// create a function and forward it somewhere
				"	hide(()->unit {});"					// and another
				"	hide((int x)->unit {});"			// and another with a different type
				"	hide(()=> 2);"						// and a bind
				"	()->unit {};"						// a function not being used ever
				"	()=> 3;"							// a bind not being used ever
				"	get()();"							// retrieve some 'hidden' function and call it
				"}", symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);

		CompoundStmtAddress root(code);

		// get list of all functions
		Callee calleeA = root[0].as<CallExprAddress>()[0].as<LambdaExprAddress>();
		Callee calleeB = root[1].as<CallExprAddress>()[0].as<LambdaExprAddress>();
		Callee calleeC = root[2].as<CallExprAddress>()[0].as<LambdaExprAddress>();
		Callee calleeD = root[3].as<CallExprAddress>()[0].as<BindExprAddress>();
		Callee calleeE = root[4].as<LambdaExprAddress>();
		Callee calleeF = root[5].as<BindExprAddress>();

		// get caller
		Caller caller = root[6].as<CallExprAddress>();

		// create call site manager
		CallSiteManager mgr(root);

		// check list of all potential targets
		EXPECT_EQ(toString(toVector(calleeA, calleeB, calleeD)), toString(mgr.getCallee(caller)));

		vector<Caller> empty;
		vector<Caller> callA = toVector(caller);

		EXPECT_EQ(toString(callA), toString(mgr.getCaller(calleeA)));
		EXPECT_EQ(toString(callA), toString(mgr.getCaller(calleeB)));
		EXPECT_EQ(toString(empty), toString(mgr.getCaller(calleeC)));
		EXPECT_EQ(toString(callA), toString(mgr.getCaller(calleeD)));
		EXPECT_EQ(toString(empty), toString(mgr.getCaller(calleeE)));
		EXPECT_EQ(toString(empty), toString(mgr.getCaller(calleeF)));
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
