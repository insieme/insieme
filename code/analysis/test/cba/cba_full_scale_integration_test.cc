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

#include <fstream>
#include <sstream>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "insieme/utils/config.h"
#include "insieme/utils/petri_net/petri_net_io.h"

#include "insieme/driver/integration/tests.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/frontend/frontend.h"

#include "insieme/analysis/cba/cba.h"
#include "insieme/analysis/cba/parallel_analysis.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::string;
	using namespace insieme::core;
	namespace fs = boost::filesystem;
	namespace fe = insieme::frontend;

	vector<string> getInputFiles();

	// the type definition (specifying the parameter type)
	class CBAInputTest : public ::testing::TestWithParam<string> { };

	// define the test case pattern
	TEST_P(CBAInputTest, C_Code) {

		string file = GetParam();

		SCOPED_TRACE(file);

		// check whether file is present
		EXPECT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		ASSERT_TRUE(fs::exists(file));

		// load file using the frontend
		NodeManager mgr;
		fe::ConversionJob job(file);
		job.setOption(fe::ConversionJob::Cilk);
		job.setOption(fe::ConversionJob::OpenMP);
		auto prog = job.execute(mgr);

		// running semantic checks
		auto res = core::checks::check(prog);
		EXPECT_TRUE(res.empty()) << res;

		auto body = ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody();
//dumpPretty(prog);

		// dump the list of thread regions
//		std::cout << "SyncPoints:\n\t" << join("\n\t", getSyncPoints(body)) << "\n";

		auto syncPoints = getSyncPoints(body);
		EXPECT_TRUE(!syncPoints.empty()) << "SyncPoints:\n\t" << syncPoints << "\n";

		auto threadRegions = getThreadRegions(body);
		EXPECT_TRUE(!threadRegions.empty()) << "Thread Regions:\n\t" << join("\n\t", threadRegions) << "\n";

		// dump the dot plot of the execution net
		const auto& net = getExecutionNet(body);
		EXPECT_LT(1, net.getNumPlaces());
		utils::petri_net::plot(net, "execution_net.svg");

		// print some equation statistics
		getCBA(body).plotStats();

//		// dump the dot plot
//		createDotDump(body);
//		createDotDumpRoots(body);
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, CBAInputTest, ::testing::ValuesIn(getInputFiles()));


	vector<string> getInputFiles() {
		vector<string> res;

		namespace idi = insieme::driver::integration;

		// test cases to be tested
		res.push_back(idi::getCase("matrix_mul_static")->getFiles()[0].string());
		res.push_back(idi::getCase("pendulum")->getFiles()[0].string());
		res.push_back(idi::getCase("omp/dijkstra")->getFiles()[0].string());

		return res;
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
