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

#include "insieme/analysis/dep_graph.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace dep {

	using namespace core;

	TEST(DependenceAnalysis, Basic) {

		// create a small SCoP using a local variable
		NodeManager manager;
		IRBuilder builder(manager);

		auto node = builder.parseStmt(
			"{"
			"	ref<int<4>> sum = 0;"
			"	for(uint<4> i = 10 .. 50 : 1) {"
			"		sum = sum+1;"
			"	}; "
			"}");

		EXPECT_TRUE(node);

		// obtain addresses of the statements
		NodeAddress root(node);
		StatementAddress init = root.getAddressOfChild(0).as<StatementAddress>();
		StatementAddress add = root.getAddressOfChild(1,3,0).as<StatementAddress>();


		// obtain list of dependencies
		DependenceGraph graph = extractDependenceGraph(node, ALL);

		// check number of dependencies
		EXPECT_EQ(6u, graph.getNumDependencies());

		// use for debugging:
		// std::cout << graph << "\n";

		// both nodes should be within the graph
		EXPECT_TRUE(graph.getStatementID(init));
		EXPECT_TRUE(graph.getStatementID(add));

		// check for some dependencies
		EXPECT_TRUE(graph.containsDependency(init, add, TRUE));		// after the declaration it is read
		EXPECT_TRUE(graph.containsDependency(add,  add, TRUE));		// after reading it, it is assigned
		EXPECT_TRUE(graph.containsDependency(add,  add, ANTI));		// but it is also read after been written

		// check for output dependencies
		EXPECT_TRUE(graph.containsDependency(init, add, OUTPUT));
		EXPECT_TRUE(graph.containsDependency(add,  add, OUTPUT));

		// check input dependency
		EXPECT_TRUE(graph.containsDependency(add,  add, INPUT));

	}

	TEST(DependenceAnalysis, LocalVariables) {

		// create a small SCoP using a local variable
		NodeManager manager;
		IRBuilder builder(manager);

		auto node = builder.parseStmt(
			"for(int<4> i = 10 .. 50 : 1) {"
			"	ref<int<4>> sum = 0;"
			"	for(int<4> k = 2 .. 100 : 1) {"
			"		sum = sum+1;"
			"	}"
			"}");

		EXPECT_TRUE(node);


		// obtain list of dependencies
		DependenceGraph graph = extractDependenceGraph(node, ALL);

		// use for debugging:
		// std::cout << graph << "\n";

		NodeAddress root(node);
		StatementAddress init = root.getAddressOfChild(3,0).as<StatementAddress>();
		StatementAddress add = root.getAddressOfChild(3,1,3,0).as<StatementAddress>();

		// both nodes should be within the graph
		EXPECT_TRUE(graph.getStatementID(init));
		EXPECT_TRUE(graph.getStatementID(add));

		// check for some dependencies
		EXPECT_TRUE(graph.containsDependency(init, add, TRUE));		// after the declaration it is read
		EXPECT_TRUE(graph.containsDependency(add,  add, TRUE));		// after reading it, it is assigned
		EXPECT_TRUE(graph.containsDependency(add,  add, ANTI));		// but it is also read after been written


		// check some dependencies which should not be there
		EXPECT_FALSE(graph.containsDependency(init, add, ANTI));	// since within the init it is not read!
		EXPECT_FALSE(graph.containsDependency(add, init, WAR));		// it is not the same variable written after read in the previous iteration
		EXPECT_FALSE(graph.containsDependency(add, init, WAW));		// same as above

		// there should not be a loop-carried dependency for the local variable
		EXPECT_FALSE(graph.containsDependency(init, init));

	}

	TEST(DependenceAnalysis, LocalVariablesMultiLevel) {

		// create a small SCoP using a local variable
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr node = builder.parseStmt(
			"for(uint<4> i = 10 .. 50 : 1) {"
			"	ref<int<4>> a = 0;"
			"	for(uint<4> k = 2 .. 100 : 1) {"
			"		a = a+1;"
			"		ref<int<4>> b = 0;"
			"		for(uint<4> j = 2 .. 100 : 1) {"
			"			a = a+1;"
			"			b = b+1;"
			"		}"
			"	}"
			"}");

		EXPECT_TRUE(node);


		// obtain list of dependencies
		DependenceGraph graph = extractDependenceGraph(node, ALL);

		// use for debugging:
//		std::cout << graph << "\n";

//		std::cout << "SCoP: " << *polyhedral::scop::ScopRegion::toScop(node) << "\n";

		NodeAddress root(node);
		StatementAddress init_a = root.getAddressOfChild(3,0).as<StatementAddress>();
		StatementAddress init_b = root.getAddressOfChild(3,1,3,1).as<StatementAddress>();

		StatementAddress inc_a1 = root.getAddressOfChild(3,1,3,0).as<StatementAddress>();
		StatementAddress inc_a2 = root.getAddressOfChild(3,1,3,2,3,0).as<StatementAddress>();
		StatementAddress inc_b  = root.getAddressOfChild(3,1,3,2,3,1).as<StatementAddress>();

		// both nodes should be within the graph
		EXPECT_TRUE(graph.getStatementID(init_a));
		EXPECT_TRUE(graph.getStatementID(init_b));

		EXPECT_TRUE(graph.getStatementID(inc_a1));
		EXPECT_TRUE(graph.getStatementID(inc_a2));
		EXPECT_TRUE(graph.getStatementID(inc_b));

		// check for some dependencies
		EXPECT_TRUE(graph.containsDependency(init_a, inc_a1, TRUE));		// after the declaration it is read

		// transitive dependencies should not be there
		EXPECT_FALSE(graph.containsDependency(inc_a1, inc_a1, TRUE));
		EXPECT_FALSE(graph.containsDependency(inc_a1, inc_a1, ANTI));

		// yet, they should be present indirectly
		EXPECT_TRUE(graph.containsDependency(inc_a1, inc_a2, TRUE));
		EXPECT_TRUE(graph.containsDependency(inc_a2, inc_a1, TRUE));


		// check some dependencies which should not be there
		EXPECT_FALSE(graph.containsDependency(init_a, inc_a1, ANTI));		// since within the init it is not read!
		EXPECT_FALSE(graph.containsDependency(init_a, inc_a1, WAR));		// it is not the same variable written after read in the previous iteration
		EXPECT_FALSE(graph.containsDependency(inc_a1, init_a, WAW));		// same as above

		// there should not be a loop-carried dependency for the local variable
		EXPECT_FALSE(graph.containsDependency(init_a, init_a));


		// also check some dependencies for b
		EXPECT_FALSE(graph.containsDependency(init_b, init_b));		// no dependency accross loops
		EXPECT_FALSE(graph.containsDependency(inc_b, init_b));

		// the default list of dependencies
		EXPECT_TRUE(graph.containsDependency(init_b, inc_b, TRUE));
		EXPECT_TRUE(graph.containsDependency(init_b, inc_b, OUTPUT));
		EXPECT_TRUE(graph.containsDependency(inc_b, inc_b, TRUE));
		EXPECT_TRUE(graph.containsDependency(inc_b, inc_b, ANTI));

	}

	TEST(DependenceAnalysis, MatrixMultiplication) {

		// As it is one of our most important examples,
		// we should check whether the dependencies are
		// as we would like to have them.

		// implement matrix multiplication
		NodeManager manager;
		IRBuilder builder(manager);

		std::map<std::string, NodePtr> symbols;
		symbols["A"] = builder.variable(builder.parseType("ref<vector<vector<uint<4>,50>,50>>"));
		symbols["B"] = builder.variable(builder.parseType("ref<vector<vector<uint<4>,50>,50>>"));
		symbols["C"] = builder.variable(builder.parseType("ref<vector<vector<uint<4>,50>,50>>"));

		auto node = builder.parseStmt(
			"for(uint<4> i = 0 .. 50 : 1) {"
			"	for(uint<4> j = 0 .. 50 : 1) {"
			"		ref<int<4>> sum = 0;"
			"		for(uint<4> k = 0 .. 50 : 1) {"
			"			sum = sum + A[i][k] * B[k][j]; "
			"		}"
			"		C[i][j] = sum;"
			"	}"
			"}", symbols);

		EXPECT_TRUE(node);




		// obtain list of dependencies
		DependenceGraph graph = extractDependenceGraph(node, ALL);

		// use for debugging:
//		std::cout << "Code: \n" << core::printer::PrettyPrinter(node) << "\n";
//		std::cout << graph << "\n";
//		std::cout << "SCoP: " << *polyhedral::scop::ScopRegion::toScop(node) << "\n";

		NodeAddress root(node);
		StatementAddress decl = root.getAddressOfChild(3,0,3,0).as<StatementAddress>();
		StatementAddress calc = root.getAddressOfChild(3,0,3,1,3,0).as<StatementAddress>();
		StatementAddress save = root.getAddressOfChild(3,0,3,2).as<StatementAddress>();

		// those should be the only statements in the dependency graph
		EXPECT_EQ(3u, graph.size());
		EXPECT_TRUE(graph.getStatementID(decl));
		EXPECT_TRUE(graph.getStatementID(calc));
		EXPECT_TRUE(graph.getStatementID(save));


		// now, check for the dependencies
		EXPECT_EQ(10u, graph.getNumDependencies());

		// check true control flow
		EXPECT_TRUE(graph.containsDependency(decl, calc, TRUE));
		EXPECT_TRUE(graph.containsDependency(calc, calc, TRUE));
		EXPECT_TRUE(graph.containsDependency(calc, save, TRUE));

		// anti-dependencies (due to reduction)
		EXPECT_TRUE(graph.containsDependency(calc, calc, ANTI));

		// check output-dependencies
		EXPECT_TRUE(graph.containsDependency(decl, calc, OUTPUT));
		EXPECT_TRUE(graph.containsDependency(calc, calc, OUTPUT));

		// check input-dependencies (although not that important)
		EXPECT_TRUE(graph.containsDependency(calc, calc, INPUT, 1)); 	// on A
		EXPECT_TRUE(graph.containsDependency(calc, calc, INPUT, 2)); 	// on B



		EXPECT_FALSE(graph.containsDependency(decl, decl));		// there should be no dependency between the sum-declaration

	}

} // end namespace dep
} // end namespace analysis
} // end namespace insieme
