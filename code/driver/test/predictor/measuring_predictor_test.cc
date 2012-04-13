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

#include "insieme/driver/predictor/measuring_predictor.h"
#include "insieme/driver/driver_config.h"

#include "insieme/frontend/frontend.h"

#include "insieme/core/ir_builder.h"

#include "insieme/transform/primitives.h"
#include "insieme/transform/filter/filter.h"
#include "insieme/transform/pattern/pattern.h"
#include "insieme/transform/pattern/ir_pattern.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace predictor {

	namespace {

		core::ProgramPtr loadExample(core::NodeManager& manager, const std::string& fileName) {
			return insieme::frontend::ConversionJob(manager, fileName).execute();
		}

		region::Region getExampleRegion(core::NodeManager& manager) {
			core::ProgramPtr prog = loadExample(manager, SRC_DIR "inputs/simple_loop_nest.c");

			EXPECT_TRUE(prog);

			// determine region => outermost for loop
			auto outermost = transform::filter::pattern(transform::pattern::outermost(transform::pattern::var("x",transform::pattern::irp::forStmt())), "x");

			vector<core::NodeAddress> matches = outermost(prog);

			EXPECT_EQ(1u, matches.size());
			EXPECT_EQ(core::NT_ForStmt, matches[0]->getNodeType());

			return matches[0].as<core::ForStmtAddress>()->getBody();
		}

	}


//	TEST(MeasuringPredictor, MeasuringTest) {
//		Logger::setLevel(ERROR);
//
//		core::NodeManager manager;
//
//		// load example region
//		region::Region region = getExampleRegion(manager);
//
//		// measure time
//		EXPECT_LT(0u, measureExecutionTime(region));
//
//	}
//
//
//	TEST(MeasuringPredictor, IterationLimit) {
//		Logger::setLevel(ERROR);
//
//		core::NodeManager manager;
//
//		// load example region
//		region::Region region = getExampleRegion(manager);
//
//		// measure execution time with limited iterations
//
//		// measure time
//		auto valueA = measureExecutionTime(region, 10);
//		EXPECT_LT(0u, valueA);
//
//	}
//
//	TEST(MeasuringPredictor, IterationLimitNoEnclosingLoop) {
//		Logger::setLevel(ERROR);
//
//		core::NodeManager manager;
//
//		// load example region
//		region::Region region = getExampleRegion(manager);
//
//		// go two levels up => no more surrounding for
//		region = region.getParentAddress(2).as<region::Region>();
//
//		// measure time
//		auto valueA = measureExecutionTime(region);
//		EXPECT_LT(0u, valueA);
//
//		// measure time
//		auto valueB = measureExecutionTime(region, 10);
//		EXPECT_LT(0u, valueB);
//
//	}

	TEST(MeasuringPredictor, PredictionTest) {
		Logger::setLevel(ERROR);

		core::NodeManager manager;

		// load example region
		region::Region region = getExampleRegion(manager);

		transform::TransformationPtr id = std::make_shared<transform::NoOp>();

		MeasuringPredictor predictor;
		EXPECT_EQ(Undecided, predictor.compare(region, id, id));

	}


	TEST(MeasuringPredictor, RemoteExecution) {
		Logger::setLevel(ERROR);

		// test whether an ssh session to localhost is possible
		if (system("ssh localhost pwd > /dev/null")) {
			std::cout << "Skipped remote test!\n";
			return;		// skip this test
		}

		core::NodeManager manager;

		// load example region
		region::Region region = getExampleRegion(manager);

		// create remote executor
		measure::ExecutorPtr executor = measure::makeRemoteExecutor("localhost");

		transform::TransformationPtr id = std::make_shared<transform::NoOp>();

		MeasuringPredictor predictor;
		EXPECT_EQ(Undecided, predictor.compare(region, id, id));
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
