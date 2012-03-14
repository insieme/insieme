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

#include <boost/algorithm/string.hpp>

#include <vector>
#include <map>

#include "insieme/frontend/frontend.h"
#include "insieme/analysis/features/code_feature_catalog.h"
#include "insieme/analysis/features/cache_features.h"
#include "insieme/analysis/dep_graph.h"

#include "insieme/transform/primitives.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/polyhedral/transformations.h"
#include "insieme/transform/rulebased/transformations.h"
#include "insieme/transform/filter/filter.h"
#include "insieme/transform/filter/standard_filter.h"

#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/sequential/sequential_backend.h"

#include "insieme/driver/measure/measure.h"
#include "insieme/driver/loader/integration_test_loader.h"


using namespace std;

using namespace insieme::analysis::features;
using namespace insieme::utils::test;
using namespace insieme::transform;
using namespace insieme::transform::polyhedral;
using namespace insieme::transform::rulebased;
using namespace insieme::core;
using namespace insieme::core::printer;
using namespace insieme::driver;
using namespace insieme::driver::measure;
using namespace insieme::driver::loader;

vector<map<MetricPtr, Quantity>> runEval(const StatementAddress& stmt, int tileSize, const vector<MetricPtr>& metrics) {

	// transform code
	TransformationPtr trans = makeNoOp();
	if (tileSize > 1) {
//		trans = makeLoopTiling(tileSize, tileSize, tileSize);

		int unroll = tileSize % 1000;
		if (unroll == 0) {
			// tiling + total unrolling
			trans = makePipeline(
					makeLoopTiling(tileSize, tileSize, tileSize),
					makeForAll(filter::innermostLoops(), makeTotalLoopUnrolling())
			);
		} else if (unroll == 1) {
			// tiling + no unrolling (does not pay off)
			trans = makeLoopTiling(tileSize, tileSize, tileSize);
		} else {
			// tiling + unrolling with a factor of 8 (largest power-of-2 dividing 1000)
			trans = makePipeline(
					makeLoopTiling(tileSize, tileSize, tileSize),
					makeForAll(filter::innermostLoops(), makeLoopUnrolling(unroll))
			);
		}
	}

	// execute remotely
	auto executor = makeRemoteExecutor("ifigner", "philipp");
//		executor = makeRemoteExecutor("localhost");
	return insieme::driver::measure::measure(trans->apply(stmt), metrics, 3, executor);
//		return measure(trans->apply(stmt), metrics, 3, executor);
//
//		std::cout << "Transformed: \n";
//		auto transformed = trans->apply(stmt);
//		std::cout << core::printer::PrettyPrinter(transformed) << "\n\n";
//
//		std::cout << "C code:\n";
//		std::cout << *backend::runtime::RuntimeBackend::getDefault()->convert(transformed.getRootNode()) << "\n\n";
//		std::cout << *backend::sequential::SequentialBackend::getDefault()->convert(transformed.getRootNode()) << "\n\n";

	// measure transformed code
//		return vector<std::map<MetricPtr, Quantity>>();
//		return insieme::driver::measure::measure(trans->apply(stmt), metrics, 1);
}


int main(int argc, char** argv) {
	Logger::setLevel(ERROR);

	NodeManager manager;

	// load test case
	ProgramPtr prog = loadIntegrationTest(manager, "matrix_mul_static", false);
	//ProgramPtr prog = loadIntegrationTest(manager, "lu_decomposition", false);
	if (!prog) {
		std::cout << "Unable to load test case - evaluation aborted!\n";
		return EXIT_FAILURE;
	}

	// find matrix-multiplication loop
	auto targetFilter = filter::allMatches(insieme::transform::pattern::irp::innerMostForLoopNest(3));

	vector<NodeAddress> targets = targetFilter(prog);
	if (targets.empty()) {
		std::cout << "Could not locate 3-times nested loop within test case - evaluation aborted!\n";
		return EXIT_FAILURE;
	}

	std::sort(targets.begin(), targets.end());
	StatementAddress target = targets[0].as<StatementAddress>();

	// run the measurement sequence

	// collect execution time and misses
	auto ms = milli * s;

	vector<MetricPtr> metrics;
	metrics.push_back(Metric::TOTAL_EXEC_TIME_NS);

	// pick metrics specifically
	metrics.push_back(Metric::TOTAL_BR_CN);
	metrics.push_back(Metric::TOTAL_L1_DCM);
	metrics.push_back(Metric::TOTAL_L1_ICM);
	metrics.push_back(Metric::TOTAL_L2_DCM);
	metrics.push_back(Metric::TOTAL_L2_ICM);
	metrics.push_back(Metric::TOTAL_L3_TCM);

//	for_each(Metric::getAll(), [&](const MetricPtr& cur) {
//		if (boost::algorithm::starts_with(cur->getName(), "total_PAPI")) metrics.push_back(cur);
//	});

	std::cout << "Nr,TS," << join(",", metrics) << "\n" << std::flush;
	int i = 1;
//		for (int ts : { 2, 4, 8, 16, 30, 60, 120, 180, 360, 720 }) {
	for (int ts = 1; ts <=1000; ts++) {
//	for (int ts = 1; ts <=80; ts++) {
//	for (int ts = 8; ts <= 1024; ts <<= 1) {

//			if (!(1000 % ts == 0 || 1000 % ts == ts/2)) {
//				continue;
//			}

		auto res = runEval(target, ts, metrics);
		for_each(res, [&](std::map<MetricPtr, Quantity>& cur) {
			std::cout << i << "," << ts << ",";
			std::cout << join(",", metrics, [&](std::ostream& out, const MetricPtr& metric) {
				if (metric == Metric::TOTAL_EXEC_TIME_NS) {
					out << Quantity(cur[metric].to(ms).getValue());
				} else {
					out << cur[metric];
				}
			});
			std::cout << "\n" << std::flush;
		});

		i++;
	}
}
