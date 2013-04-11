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

#include <cstdlib>
#include <future>

#include <boost/filesystem.hpp>

#include "insieme/driver/measure/measure.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace driver {
namespace measure {

	using namespace std;
	using namespace core;

	TEST(Measuring, Metrics) {
		Logger::setLevel(WARNING);

		// play a little using units
		auto time = Metric::TOTAL_EXEC_TIME;

		EXPECT_EQ(time, Metric::TOTAL_EXEC_TIME);
		EXPECT_NE(time, Metric::TIMESTAMP_END);

		EXPECT_EQ("total_exec_time", toString(time));

		EXPECT_EQ(Metric::TOTAL_EXEC_TIME, Metric::getForName(Metric::TOTAL_EXEC_TIME->getName()));
	}

	TEST(Measuring, MetricsDependencies) {
		Logger::setLevel(WARNING);

		// test some parameter without dependency
		EXPECT_TRUE(Metric::PAPI_L1_DCM->getDependencies().empty());
		EXPECT_TRUE(Metric::TIMESTAMP_START->getDependencies().empty());

		// read the dependencies of a metric
		std::set<MetricPtr> dep;
		dep.insert(Metric::TIMESTAMP_START);
		dep.insert(Metric::TIMESTAMP_END);
		EXPECT_EQ(dep, Metric::TOTAL_EXEC_TIME->getDependencies());

		// check something with a single dependency
		dep.clear();
		dep.insert(Metric::PAPI_L3_TCM);
		EXPECT_EQ(dep, Metric::TOTAL_L3_CACHE_MISS->getDependencies());

	}


	TEST(Measuring, MetricsDependencyClosure) {
		Logger::setLevel(WARNING);

		// the set of dependencies to compare with
		std::set<MetricPtr> dep;

		// check empty list
		EXPECT_EQ(dep, getDependencyClosureLeafs(toVector<MetricPtr>()));

		// check a leaf metric itself
		dep.insert(Metric::PAPI_L3_TCM);
		EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::PAPI_L3_TCM)));

		// check a derived metric
		EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::TOTAL_L3_CACHE_MISS)));

		// check something with multiple dependencies
		dep.clear();
		dep.insert(Metric::TIMESTAMP_START);
		dep.insert(Metric::TIMESTAMP_END);
		EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::TOTAL_EXEC_TIME)));


		// check multiple metrics
		dep.insert(Metric::PAPI_L3_TCM);
		EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::TOTAL_EXEC_TIME, Metric::TOTAL_L3_CACHE_MISS)));

	}

	TEST(Measuring, MeasureExecutionTime) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 10 .. 50 : 1) {"
				"		sum = sum + 1;"
				"	}"
				"}"
		);

		EXPECT_TRUE(stmt);

		StatementAddress addr(stmt);

		// measure execution time of this fragment
		auto time = measure(addr, Metric::TOTAL_EXEC_TIME);

		EXPECT_TRUE(time.isValid());
		EXPECT_TRUE(time > 0 * s) << "Actual time: " << time;

	}


	TEST(Measuring, MeasureExecutionTimePFor) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		auto& basic = manager.getLangBasic();
		IRBuilder builder(manager);

		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 10 .. 50 : 1) {"
				"		sum = sum + 1;"
				"	}"
				"}"
		);

		EXPECT_TRUE(stmt);

		// wrap into pfor statement + make parallel
		ExpressionPtr start = builder.intLit(0);
		ExpressionPtr end = builder.intLit(1000*1000*2);
		ExpressionPtr step = builder.intLit(1);

		VariablePtr iter = builder.variable(start->getType());
		ForStmtPtr loop = builder.forStmt(iter, start, end, step, stmt);

		// convert into parallel loop
		stmt = builder.callExpr(basic.getUnit(), basic.getMerge(), builder.parallel(builder.pfor(loop)));

		std::map<StatementAddress, driver::measure::region_id> regions;

		StatementAddress addr(stmt);
		regions[addr] = 0;
		regions[core::analysis::findLeftMostOutermostCallOf(addr, basic.getPFor())] = 1;

		// ---- run code ----

		// measure execution time of this fragment
		auto res = measure(regions, toVector(
			Metric::TOTAL_EXEC_TIME,
			Metric::TOTAL_WALL_TIME, Metric::TOTAL_CPU_TIME,
			Metric::PARALLELISM     // Metric::AVG_NUM_WORKERS,
			// Metric::AVG_EFFICIENCY,  Metric::WEIGHTED_EFFICIENCY
		));

//		std::cout << res << "\n";

		EXPECT_GT(res[0][Metric::TOTAL_WALL_TIME].getValue(),res[1][Metric::TOTAL_WALL_TIME].getValue());
		EXPECT_GT(res[0][Metric::TOTAL_CPU_TIME].getValue(),res[1][Metric::TOTAL_CPU_TIME].getValue());

		for(int i =0; i<2; i++) {

			auto totalTime = res[i][Metric::TOTAL_EXEC_TIME];
			auto wallTime = res[i][Metric::TOTAL_WALL_TIME];
			auto cpuTime = res[i][Metric::TOTAL_CPU_TIME];

			auto parallelism = res[i][Metric::PARALLELISM];
//			auto num_worker = res[i][Metric::AVG_NUM_WORKERS];

//			auto avg_efficiency = res[i][Metric::AVG_EFFICIENCY];
//			auto weighted_efficiency = res[i][Metric::WEIGHTED_EFFICIENCY];

			ASSERT_TRUE(totalTime.isValid());
			ASSERT_TRUE(wallTime.isValid());
			ASSERT_TRUE(cpuTime.isValid());
			ASSERT_TRUE(parallelism.isValid());

			EXPECT_GT(cpuTime.getValue(), 0);
			EXPECT_GT(wallTime.getValue(), 0);
			EXPECT_GT(totalTime.getValue(), 0);

			EXPECT_GT(parallelism.getValue(), 0);
//			EXPECT_GE(num_worker.getValue(), 1);

//			EXPECT_GT(avg_efficiency.getValue(), 0);
//			EXPECT_GT(weighted_efficiency.getValue(), 0);
//			EXPECT_EQ(avg_efficiency, weighted_efficiency);

			// cpu time should be roughly equal to the wall time
//			EXPECT_EQ((int)(totalTime.getValue() / (1000*1000)), (int)(cpuTime.getValue() / (1000*1000)));

			//EXPECT_GT(cpuTime, wallTime);
			EXPECT_EQ(parallelism, cpuTime / wallTime);

		}
	}

	TEST(Measuring, NestedRegions) {
		Logger::setLevel(WARNING);

		NodeManager manager;
		IRBuilder builder(manager);

		vector<NodeAddress> stmts= builder.parseAddresses(
				"{"
				"	let load = (int<4> n)->int<4> {"
				"		ref<int<4>> sum = var(0);"
				"		for(int<4> i = 0 .. n) {"
				"			sum = sum / i;"
				"			for(int<4> j = 0 .. 100000) {"
				"				sum = sum + j;"
				"			}"
				"		}"
				"		return *sum;"
				"	};"
				"	"
				"	ref<int<4>> res = var(0);"
				"	$for(int<4> i = 1 .. 5000) {"
				"		$for(int<4> j = 1 .. 5000) {"
				"			res = res + load(100000);"
				"		}$"
				"		$for(int<4> k = 1 .. 50) {"
				"			$for(int<4> l = 1 .. 100) {"
				"				res = res + load(100000);"
				"			}$"
				"		}$"
				"	}$"
				"}"
		);

		ASSERT_EQ(4u, stmts.size());

		ForStmtAddress forI = stmts[0].as<ForStmtAddress>();
		ForStmtAddress forJ = stmts[1].as<ForStmtAddress>();
		ForStmtAddress forK = stmts[2].as<ForStmtAddress>();
		ForStmtAddress forL = stmts[3].as<ForStmtAddress>();
		StatementAddress root(forI.getRootNode().as<StatementPtr>());

//		std::cout << "\n------------------------ Loop I: \n"; dump(forI);
//		std::cout << "\n------------------------ Loop J: \n"; dump(forJ);
//		std::cout << "\n------------------------ Loop K: \n"; dump(forK);
//		std::cout << "\n------------------------ Loop L: \n"; dump(forL);
//		std::cout << "\n------------------------ Root: \n"; dump(root);

		// measure execution times
		auto res = measure(toVector<StatementAddress>(root, forI, forJ, forK, forL), toVector(Metric::TOTAL_WALL_TIME, Metric::TOTAL_CPU_TIME));

		// check whether data is valid
		EXPECT_TRUE(res[root][Metric::TOTAL_WALL_TIME].isValid());
		EXPECT_TRUE(res[forI][Metric::TOTAL_WALL_TIME].isValid());
		EXPECT_TRUE(res[forJ][Metric::TOTAL_WALL_TIME].isValid());
		EXPECT_TRUE(res[forK][Metric::TOTAL_WALL_TIME].isValid());
		EXPECT_TRUE(res[forL][Metric::TOTAL_WALL_TIME].isValid());

		EXPECT_TRUE(res[root][Metric::TOTAL_CPU_TIME].isValid());
		EXPECT_TRUE(res[forI][Metric::TOTAL_CPU_TIME].isValid());
		EXPECT_TRUE(res[forJ][Metric::TOTAL_CPU_TIME].isValid());
		EXPECT_TRUE(res[forK][Metric::TOTAL_CPU_TIME].isValid());
		EXPECT_TRUE(res[forL][Metric::TOTAL_CPU_TIME].isValid());

		// check whether data is not 0
		EXPECT_LT(0.0, res[root][Metric::TOTAL_WALL_TIME].getValue());
		EXPECT_LT(0.0, res[forI][Metric::TOTAL_WALL_TIME].getValue());
		EXPECT_LT(0.0, res[forJ][Metric::TOTAL_WALL_TIME].getValue());
		EXPECT_LT(0.0, res[forK][Metric::TOTAL_WALL_TIME].getValue());
		EXPECT_LT(0.0, res[forL][Metric::TOTAL_WALL_TIME].getValue());

		// check whether data is valid
		EXPECT_LT(0.0, res[root][Metric::TOTAL_CPU_TIME].getValue());
		EXPECT_LT(0.0, res[forI][Metric::TOTAL_CPU_TIME].getValue());
		EXPECT_LT(0.0, res[forJ][Metric::TOTAL_CPU_TIME].getValue());
		EXPECT_LT(0.0, res[forK][Metric::TOTAL_CPU_TIME].getValue());
		EXPECT_LT(0.0, res[forL][Metric::TOTAL_CPU_TIME].getValue());

		// root has to be the sum of the loops
		EXPECT_GT(res[root][Metric::TOTAL_WALL_TIME], res[forI][Metric::TOTAL_WALL_TIME]);
		EXPECT_GT(res[root][Metric::TOTAL_CPU_TIME], res[forI][Metric::TOTAL_CPU_TIME]);

		// loop I is bigger than sum of J and K
		EXPECT_GT(res[forI][Metric::TOTAL_WALL_TIME], res[forJ][Metric::TOTAL_WALL_TIME] + res[forK][Metric::TOTAL_WALL_TIME]);
		EXPECT_GT(res[forI][Metric::TOTAL_CPU_TIME], res[forJ][Metric::TOTAL_CPU_TIME] + res[forK][Metric::TOTAL_CPU_TIME]);

		// loop K is bigger than L
		EXPECT_GT(res[forK][Metric::TOTAL_WALL_TIME], res[forL][Metric::TOTAL_WALL_TIME]);
		EXPECT_GT(res[forK][Metric::TOTAL_CPU_TIME], res[forL][Metric::TOTAL_CPU_TIME]);

	}

	TEST(Measuring, MultipleExitPoints) {
		Logger::setLevel(WARNING);

		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_TRUE(measure(builder.parseStmt("{ return; }"), Metric::TOTAL_WALL_TIME).isValid());

		EXPECT_TRUE(measure(builder.parseAddresses("{ for(int<4> i= 0 .. 10) { ${ break; }$ } }")[0].as<core::StatementAddress>(), Metric::TOTAL_WALL_TIME).isValid());

		EXPECT_TRUE(measure(builder.parseAddresses("{ for(int<4> i= 0 .. 10) { ${ continue; }$ } }")[0].as<core::StatementAddress>(), Metric::TOTAL_WALL_TIME).isValid());

		EXPECT_TRUE(measure(builder.parseStmt("{ if(true) { return; } else { return; } }"), Metric::TOTAL_WALL_TIME).isValid());


		// a return with a n expression
		EXPECT_TRUE(measure(builder.parseAddresses("{ ()->int<4> { for(int<4> i= 0 .. 10) { ${ return 1 + 2; }$ } } (); }")[0].as<core::StatementAddress>(), Metric::TOTAL_WALL_TIME).isValid());


		// two nested regions ending at the same point
		vector<NodeAddress> addr = builder.parseAddresses("{ ()->int<4> { for(int<4> i= 0 .. 10) { ${ 2 + 3; ${ return 1 + 2; }$ }$ } } (); }");
		auto res = measure(toVector(addr[0].as<core::StatementAddress>(), addr[1].as<core::StatementAddress>()), toVector(Metric::TOTAL_WALL_TIME));

		EXPECT_TRUE(res[addr[0].as<core::StatementAddress>()][Metric::TOTAL_WALL_TIME].isValid());
		EXPECT_TRUE(res[addr[1].as<core::StatementAddress>()][Metric::TOTAL_WALL_TIME].isValid());

	}

	TEST(Measuring, Measure) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 10 .. 50 : 1) {"
				"		sum = sum + 1;"
				"	}"
				"}"
		);

		EXPECT_TRUE(stmt);

		StatementAddress addr(stmt);

		// measure execution time of this fragment
		auto time = measure(addr, Metric::TOTAL_EXEC_TIME);

		EXPECT_TRUE(time.isValid());
		EXPECT_TRUE(time > 0 * s) << "Actual time: " << time;


		// measure cache misses of this fragment
		auto misses = measure(addr, toVector(Metric::TOTAL_L1_DATA_CACHE_MISS, Metric::TOTAL_L2_CACHE_MISS));

		EXPECT_TRUE(misses[Metric::TOTAL_L1_DATA_CACHE_MISS].isValid());
		EXPECT_TRUE(misses[Metric::TOTAL_L1_DATA_CACHE_MISS].getValue() > 0);

		EXPECT_TRUE(misses[Metric::TOTAL_L2_CACHE_MISS].isValid());
		EXPECT_TRUE(misses[Metric::TOTAL_L2_CACHE_MISS].getValue() > 0);

	}

	TEST(Measuring, MeasureRemote) {
		Logger::setLevel(WARNING);

		// test whether a remote session to the local host can be created
		if (system("ssh localhost pwd > /dev/null")) {
			std::cout << "Skipped remote test!\n";
			return;		// skip this test
		}

		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 10 .. 50 : 1) {"
				"		sum = sum + 1;"
				"	}"
				"}"
		);

		EXPECT_TRUE(stmt);

		StatementAddress addr(stmt);
		auto executor = makeRemoteExecutor("localhost");

		// measure execution time of this fragment
		auto time = measure(addr, Metric::TOTAL_EXEC_TIME, executor);

		EXPECT_TRUE(time.isValid());
		EXPECT_TRUE(time > 0 * s) << "Actual time: " << time;


		// measure cache misses of this fragment
		auto misses = measure(addr, toVector(Metric::TOTAL_L1_DATA_CACHE_MISS, Metric::TOTAL_L2_CACHE_MISS), executor);

		EXPECT_TRUE(misses[Metric::TOTAL_L1_DATA_CACHE_MISS].isValid());
		EXPECT_TRUE(misses[Metric::TOTAL_L1_DATA_CACHE_MISS].getValue() > 0);

		EXPECT_TRUE(misses[Metric::TOTAL_L2_CACHE_MISS].isValid());
		EXPECT_TRUE(misses[Metric::TOTAL_L2_CACHE_MISS].getValue() > 0);

	}

	TEST(Measuring, MeasureParallel) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 10 .. 50 : 1) {"
				"		sum = sum + 1;"
				"	}"
				"}"
		);

		EXPECT_TRUE(stmt);

		StatementAddress addr(stmt);

		vector<ExecutorPtr> executors;
		executors.push_back(makeLocalExecutor());
		executors.push_back(makeLocalExecutor());

		// test whether a remote session to the local host can be created
		if (!system("ssh localhost pwd > /dev/null")) {
			executors.push_back(makeRemoteExecutor("localhost"));
		}

		// build binary
		auto binary = buildBinary(addr);

		vector<std::future<void>> futures;
		for_each(executors, [&](const ExecutorPtr& executor) {
			// run executors in parallel
			for(int i=0; i<5; i++) {
				futures.push_back(std::async(std::launch::async, [&](){

					// measure cache misses of this fragment
					auto data = measure(binary, toVector(Metric::TOTAL_L1_DATA_CACHE_MISS, Metric::TOTAL_L2_CACHE_MISS), 1, executor);

					auto misses = data[0][0];
					EXPECT_TRUE(misses[Metric::TOTAL_L1_DATA_CACHE_MISS].isValid());
					EXPECT_TRUE(misses[Metric::TOTAL_L1_DATA_CACHE_MISS].getValue() > 0);

					EXPECT_TRUE(misses[Metric::TOTAL_L2_CACHE_MISS].isValid());
					EXPECT_TRUE(misses[Metric::TOTAL_L2_CACHE_MISS].getValue() > 0);
				}));
			}
		});

		// join futures
		for_each(futures, [](const std::future<void>& cur) {
			cur.wait();
		});

		boost::filesystem::remove(binary);
	}

	TEST(Measuring, MeasureMultipleRegionsMultipleMetrics) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 10 .. 50 : 1) {"
				"		for(uint<4> j = 0 .. 80 : 1) {"
				"			sum = sum + 1;"
				"		}"
				"		for(uint<4> k = 0 .. 160 : 1) {"
				"			sum = sum + 1;"
				"		}"
				"		for(uint<4> m = 0 .. 240 : 1) {"
				"			sum = sum + 1;"
				"		}"
				"		for(uint<4> n = 0 .. 320 : 1) {"
				"			sum = sum + 1;"
				"		}"
				"	}"
				"}"
		);

		EXPECT_TRUE(stmt);

		StatementAddress root(stmt);
		ForStmtAddress for1 = root.getAddressOfChild(1,3,0).as<ForStmtAddress>();
		ForStmtAddress for2 = root.getAddressOfChild(1,3,1).as<ForStmtAddress>();
		ForStmtAddress for3 = root.getAddressOfChild(1,3,2).as<ForStmtAddress>();
		ForStmtAddress for4 = root.getAddressOfChild(1,3,3).as<ForStmtAddress>();

		// pick metrics
		vector<MetricPtr> metrics = toVector(Metric::TOTAL_EXEC_TIME, Metric::AVG_EXEC_TIME);

		// label regions
		std::map<StatementAddress, region_id> regions;
		regions[for1] = 4;
		regions[for2] = 4;
		regions[for3] = 8;
		regions[for4] = 7;

		// measure execution time of this fragment
		auto res = measure(regions, metrics);

		ASSERT_EQ(9u, res.size());		// intermediate regions should be filled

		ASSERT_EQ(2u, res[4].size());
		ASSERT_EQ(2u, res[7].size());
		ASSERT_EQ(2u, res[8].size());

		EXPECT_TRUE(res[4][Metric::TOTAL_EXEC_TIME].isValid());
		EXPECT_TRUE(res[4][Metric::AVG_EXEC_TIME].isValid());
		EXPECT_TRUE(res[7][Metric::TOTAL_EXEC_TIME].isValid());
		EXPECT_TRUE(res[7][Metric::AVG_EXEC_TIME].isValid());
		EXPECT_TRUE(res[8][Metric::TOTAL_EXEC_TIME].isValid());
		EXPECT_TRUE(res[8][Metric::AVG_EXEC_TIME].isValid());

		EXPECT_GT(res[4][Metric::TOTAL_EXEC_TIME], res[4][Metric::AVG_EXEC_TIME]);
		EXPECT_GT(res[7][Metric::TOTAL_EXEC_TIME], res[7][Metric::AVG_EXEC_TIME]);
		EXPECT_GT(res[8][Metric::TOTAL_EXEC_TIME], res[8][Metric::AVG_EXEC_TIME]);


		// conduct multiple runs
		auto res2 = measure(regions, metrics, 5);

		ASSERT_EQ(5u, res2.size());

		for(unsigned i = 0; i<res2.size(); i++) {
			auto& res = res2[i];

			ASSERT_EQ(9u, res.size());

			ASSERT_EQ(2u, res[4].size());
			ASSERT_EQ(2u, res[7].size());
			ASSERT_EQ(2u, res[8].size());

			EXPECT_TRUE(res[4][Metric::TOTAL_EXEC_TIME].isValid());
			EXPECT_TRUE(res[4][Metric::AVG_EXEC_TIME].isValid());
			EXPECT_TRUE(res[7][Metric::TOTAL_EXEC_TIME].isValid());
			EXPECT_TRUE(res[7][Metric::AVG_EXEC_TIME].isValid());
			EXPECT_TRUE(res[8][Metric::TOTAL_EXEC_TIME].isValid());
			EXPECT_TRUE(res[8][Metric::AVG_EXEC_TIME].isValid());

			EXPECT_GT(res[4][Metric::TOTAL_EXEC_TIME], res[4][Metric::AVG_EXEC_TIME]);
			EXPECT_GT(res[7][Metric::TOTAL_EXEC_TIME], res[7][Metric::AVG_EXEC_TIME]);
			EXPECT_GT(res[8][Metric::TOTAL_EXEC_TIME], res[8][Metric::AVG_EXEC_TIME]);

		}
	}


	TEST(Measuring, PapiCounterPartitioning) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(
				"{"
				"	ref<int<4>> sum = var(0);"
				"	for(uint<4> i = 1 .. 5000 : 1) {"
				"		for(uint<4> j = 1 .. 5000 : 1) {"
				"			sum = sum + 1;"
				"		}"
				"	}"
//				"	print(\"sum = %d\\n\", *sum);"
				"}"
		);

		EXPECT_TRUE(stmt);

		StatementAddress addr(stmt);

		// measure execution time of this fragment
		auto metrics = toVector(
				Metric::TOTAL_EXEC_TIME,
				Metric::TOTAL_L1_DCM,
				Metric::TOTAL_L1_ICM,
				Metric::TOTAL_L2_DCM,
				Metric::TOTAL_L2_ICM,
				Metric::TOTAL_L1_TCM,
				Metric::TOTAL_L2_TCM,
				Metric::TOTAL_TLB_DM,
				Metric::TOTAL_TLB_IM,
				Metric::TOTAL_L1_LDM,
				Metric::TOTAL_L1_STM,
				Metric::TOTAL_L2_STM
		);

		auto res = measure(addr, metrics);

		EXPECT_EQ(metrics.size(), res.size());
		for_each(metrics, [&](const MetricPtr& cur) {
			EXPECT_FALSE(res.find(cur) == res.end());
		});

		// test whether time is only counted once
		auto time = res[Metric::TOTAL_EXEC_TIME];

		// run without additional parameters
		auto time2 = measure(addr, Metric::TOTAL_EXEC_TIME);

		// the two times should roughly be the same
		auto factor = Quantity(3);
		// TODO: fix the aggregation of measurements and re-enable this test case
//		EXPECT_LT(time2, factor*time);
//		EXPECT_LT(time, factor*time2);
	}

//
//	DISABLED DUE TO REQUIRED USER PRIVILEGES
//
//	TEST(Measuring, MeasureRemoteSGE) {
//		Logger::setLevel(WARNING);
//
//		// create a small example code fragment
//		NodeManager manager;
//		IRBuilder builder(manager);
//		StatementPtr stmt = builder.parseStmt(
//				"{"
//				"	ref<int<4>> sum = var(0);"
//				"	for(uint<4> i = 10 .. 50 : 1) {"
//				"		sum = sum + 1;"
//				"	}"
//				"}"
//		);
//
//		EXPECT_TRUE(stmt);
//
//		StatementAddress addr(stmt);
//		auto executor = makeRemoteSGEExecutor("leo3.uibk.ac.at", "c7031057", "/scratch/c7031057");
//
//		// measure execution time of this fragment
//		auto time = measure(addr, Metric::TOTAL_EXEC_TIME, executor);
//
//		ASSERT_TRUE(time.isValid());
//		EXPECT_TRUE(time > 0 * s) << "Actual time: " << time;
//
//	}
//
//	TEST(Measuring, MeasureRemotePBS) {
//		Logger::setLevel(WARNING);
//
//		// create a small example code fragment
//		NodeManager manager;
//		IRBuilder builder(manager);
//		StatementPtr stmt = builder.parseStmt(
//				"{"
//				"	ref<int<4>> sum = var(0);"
//				"	for(uint<4> i = 10 .. 50 : 1) {"
//				"		sum = sum + 1;"
//				"	}"
//				"}"
//		);
//
//		EXPECT_TRUE(stmt);
//
//		StatementAddress addr(stmt);
//		auto executor = makeRemotePBSExecutor("mach.uibk.ac.at", "c7031057", "/scratch/c703/c7031057");
//
//		// measure execution time of this fragment
//		auto time = measure(addr, Metric::TOTAL_EXEC_TIME, executor);
//
//		ASSERT_TRUE(time.isValid());
//		EXPECT_TRUE(time > 0 * s) << "Actual time: " << time;
//
//	}


} // end namespace measure
} // end namespace driver
} // end namespace insieme
