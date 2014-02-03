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


#include "insieme/analysis/cba/cba.h"
#include "insieme/analysis/cba/parallel_analysis.h"
#include "insieme/analysis/cba/framework/entities/execution_net.h"
//#include "insieme/analysis/cba/framework/cba.h"
//#include "insieme/analysis/cba/analysis/thread_regions.h"
//#include "insieme/core/ir_builder.h"

#include "insieme/utils/petri_net/petri_net_io.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, SimpleSequential) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				" 	a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get execution net
		auto net = getExecutionNet(code);

		EXPECT_EQ(1, net.getNumPlaces());
		EXPECT_EQ(0, net.getNumTransitions());

//		std::cout << "Plotting network ...\n";
//		utils::petri_net::plot(net);

//		createDotDump(analysis);
	}

	TEST(CBA, SimpleSpawn) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				" 	spawn a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get execution net
		auto net = getExecutionNet(code);

		EXPECT_EQ(3, net.getNumPlaces());
		EXPECT_EQ(1, net.getNumTransitions());

//		std::cout << "Plotting network ...\n";
//		utils::petri_net::plot(net);

//		createDotDump(analysis);
	}

	TEST(CBA, MultiSpawn) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				" 	spawn a = 1;"
				"	a = 2;"
				" 	spawn a = 3;"
				"	a = 4;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get execution net
		auto net = getExecutionNet(code);

		EXPECT_EQ(5, net.getNumPlaces());
		EXPECT_EQ(2, net.getNumTransitions());

//		std::cout << "Plotting network ...\n";
//		utils::petri_net::plot(net);

//		createDotDump(analysis);
	}

	TEST(CBA, Channels) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				"	auto c = channel.create(lit(int<4>),param(1));"
				" 	auto t1 = spawn {"
				"		a = 1;"
				"		channel.send(c,1);"
				"	};"
				" 	auto t2 = spawn {"
				"		channel.recv(c);"
				"		a = 2;"
				"	};"
				"	a = 3;"
				"	sync t1;"
				"	sync t2;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get execution net
		auto net = getExecutionNet(code);

		EXPECT_EQ(10, net.getNumPlaces());
		EXPECT_EQ(6, net.getNumTransitions());

//		std::cout << "Plotting network ...\n";
//		utils::petri_net::plot(net);

//		createDotDump(code);
	}

	TEST(CBA, Uncertain) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);
		std::map<string, NodePtr> symbols;
		symbols["c"] = builder.variable(mgr.getLangBasic().getBool(), 100);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	ref<int> x = var(12);"
				"	"
				"	auto j1 = job { x = 1; };"
				"	auto j2 = job { x = 2; };"
				"	"
				"	auto t = parallel((c)?j1:j2);"
				"	sync t;"
				"}", symbols
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get execution net
		auto net = getExecutionNet(code);

		EXPECT_EQ(7, net.getNumPlaces());
		EXPECT_EQ(6, net.getNumTransitions());

//		std::cout << "Plotting network ...\n";
//		utils::petri_net::plot(net);

//		createDotDump(code);
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
