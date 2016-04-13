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

#include <string>
#include <iomanip>

#include <boost/algorithm/string.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/version.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/container_utils.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/object_file_utils.h"
#include "insieme/driver/measure/measure.h"

#include "insieme/core/analysis/region/for_selector.h"
#include "insieme/core/analysis/region/fun_call_selector.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace dr = insieme::driver;
namespace cmd = insieme::driver::cmd;
namespace dm = insieme::driver::measure;

std::vector<core::StatementAddress> findLoopsWithMPI(const core::NodeAddress addr) {
	core::ProgramPtr retval;

	core::analysis::region::ForSelector forSelector;
	core::analysis::region::FunctionCallSelector funCallSelector("MPI_");

	// find all for loops
	auto loopList = forSelector.getRegions(addr);
	std::cout << "Total for loops: " << loopList.size() << "\n";

	// find all MPI calls
	vector<core::StatementAddress> mpiCallList;
	for(auto loop : loopList) {
		auto funCalls = funCallSelector.getRegions(loop);
		std::copy(funCalls.begin(), funCalls.end(), std::back_inserter(mpiCallList));
	}

	std::cout << "Total mpi calls: " << mpiCallList.size() << "\n";
	
	return mpiCallList;
}

int main(int argc, char** argv) {
	// Step 1: parse input parameters
	std::vector<std::string> arguments(argv, argv + argc);
	cmd::Options options = cmd::Options::parse(arguments);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }

	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	std::cout << "Insieme mpi model builder - Version: " << utils::getVersion() << "\n";

	// Step 2: filter input files
	vector<fe::path> inputs;
	vector<fe::path> libs;
	vector<fe::path> extLibs;

	set<string> cExtensions = { ".c", ".i", ".h" };
	set<string> cplusplusExtensions = { ".C", ".cc", ".cp", ".cpp", ".CPP", ".cxx", ".c++", ".ii", ".H", ".hh", ".hp", ".hxx", ".hpp", ".HPP", ".h++", ".tcc" };

	for(const fe::path& cur : options.job.getFiles()) {
		auto ext = fs::extension(cur);
		if(ext == ".o" || ext == ".so") {
			if(dr::isInsiemeLib(cur)) {
				libs.push_back(cur);
			}
			else {
				extLibs.push_back(cur);
			}
		}
		else if(cExtensions.count(ext) || cplusplusExtensions.count(ext)) {
			inputs.push_back(cur);
		}
		else {
			LOG(ERROR) << "Unrecognized file format: " << cur << "\n";
			return 1;
		}
	}
	// indicates that a shared object file should be created
	bool createSharedObject = fs::extension(options.settings.outFile) == ".so";

	// std::cout << "Libs:    " << libs << "\n";
	// std::cout << "Inputs:  " << inputs << "\n";
	// std::cout << "ExtLibs: " << extLibs << "\n";
	// std::cout << "OutFile: " << options.settings.outFile << "\n";
	// std::cout << "Compile Only: " << options.settings.compileOnly << "\n";
	// std::cout << "SharedObject: " << createSharedObject << "\n";
	// std::cout << "WorkingDir: " << boost::filesystem::current_path() << "\n";

	// update input files
	options.job.setFiles(inputs);

	// Step 3: load input code
	co::NodeManager mgr;

	// load libraries
	options.job.setLibs(::transform(libs, [&](const fe::path& cur) {
		std::cout << "Loading " << cur << " ...\n";
		return dr::loadLib(mgr, cur);
	}));


	// if it is compile only or if it should become an object file => save it
	if(options.settings.compileOnly || createSharedObject) {
		auto res = options.job.toIRTranslationUnit(mgr);
		std::cout << "Saving object file ...\n";
		dr::saveLib(res, options.settings.outFile);
		return dr::isInsiemeLib(options.settings.outFile) ? 0 : 1;
	}

	std::cout << "Extracting executable ...\n";

	// convert src file to target code
	auto program = options.job.execute(mgr);

	auto firstExpr = core::ExpressionAddress(program->getEntryPoints().front());

	auto mpiCallsInLoops = findLoopsWithMPI(firstExpr);

	//wall time, cpu time, num executions

	std::vector<dm::MetricPtr> metrics = { dm::Metric::getForNameAndUnit("wall_time(ns)") };
	utils::compiler::Compiler mpicc("mpicc");
	//mpicc.addFlag("-x c");
	mpicc.addFlag("-Wall");
	mpicc.addFlag("--std=gnu99");
	mpicc.addFlag("-DIRT_USE_MPI");
	std::map<string, string> env;
	// fix number of worker threads to 1, relying on mpirun alone seems insufficient for thread concurrency control
	env["IRT_NUM_WORKERS"] = "1";
	env["IRT_INST_REGION_INSTRUMENTATION"] = "enabled";
	// rely on mpirun to do process-core mapping
	const string mpiRun = "mpirun -np 2 --map-by core";
	
	auto data = dm::measure(mpiCallsInLoops, metrics, 1, std::make_shared<dm::LocalExecutor>(mpiRun), mpicc, env);

	/*
	 * TODO:
	 * - Crucial
	 *   - fix instrumentation to include MPI_Wait after non-blocking MPI calls 
	 *   - do region analysis, obtain features
	 * - Automated measurements
	 *   x add wrapper for program execution, probably just a string (do setup + parameter generation in driver)
	 *   - verify that all IRT instances are correctly mapped to cores
	 *   x IRT_NUM_WORKERS=1
	 *   x add suffix for worker efficiency log
	 *   - do data aggregation for worker efficiency log
	 * - Cleanup and nice-to-have
	 *   - add mpi compiler to utils?
	 *   - cleanup driver, remove non-functional cmd options, etc...
	 *   - set IRT_INST_REGION_INSTRUMENTATION=enabled env var even if there are no PAPI counters (c.f. measure.cpp)
	 **/

	std::cout << data << "\n";

	exit(42);

	return 0;
}
