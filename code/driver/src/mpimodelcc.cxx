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
#include <boost/format.hpp>

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/version.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/measure/measure.h"
#include "insieme/driver/object_file_utils.h"

#include "insieme/core/analysis/region/for_selector.h"
#include "insieme/core/analysis/region/fun_call_selector.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/lang/instrumentation_extension.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace dr = insieme::driver;
namespace cmd = insieme::driver::cmd;
namespace dm = insieme::driver::measure;
namespace pt = insieme::core::pattern;

struct Region {
	core::StatementAddress start;
	core::StatementAddress end;
	dm::region_id regionID;
	std::string label;

	Region(core::StatementAddress start, core::StatementAddress end, dm::region_id regionID, std::string label)
		: start(start), end(end), regionID(regionID), label(label) {}

	// sort according to addresses
	bool operator<(Region other) const { return start < other.start || (start >= other.start && end < other.end); }
};

std::string getNameForFunCall(core::NodePtr funPtr) {
	std::string retVal("");
	if(auto exprPtr = funPtr.isa<core::CallExprPtr>()) { funPtr = exprPtr->getFunctionExpr(); }
	if(auto litPtr = funPtr.isa<core::LiteralPtr>()) { retVal = litPtr->getStringValue(); }
	return utils::demangle(retVal);
}

core::NodePtr encloseInInstrumentation(const core::NodePtr& root, const core::NodeAddress& begin, const core::NodeAddress& end, const unsigned regionID) {
	// obtain references to primitives
	auto myRoot = root;
	auto& manager = myRoot.getNodeManager();
	core::IRBuilder builder(manager);
	const auto& basic = manager.getLangBasic();
	const auto& unit = basic.getUnit();
	auto& instExt = manager.getLangExtension<insieme::core::lang::InstrumentationExtension>();

	// build instrumented code section using begin/end markers
	auto regionIDLit = builder.uintLit(regionID);
	auto region_inst_start_call = builder.callExpr(unit, instExt.getInstrumentationRegionStart(), regionIDLit);
	auto region_inst_end_call = builder.callExpr(unit, instExt.getInstrumentationRegionEnd(), regionIDLit);

	core::NodeAddress newEnd = end.switchRoot(myRoot);
	myRoot = core::transform::insertAfter(manager, newEnd.as<core::StatementAddress>(), region_inst_end_call);

	core::NodeAddress newStart = begin.switchRoot(myRoot);
	myRoot = core::transform::insertBefore(manager, newStart.as<core::StatementAddress>(), region_inst_start_call);

	return myRoot;
}

std::vector<Region> findMPICalls(const core::NodeAddress& root) {
	std::vector<Region> retVal;

	// ensures that all MPI calls refer to the same MPI_Request object
	auto mpiRequest = pt::irp::variable(pt::var("request"), pt::var("x"));

	// matches non-blocking mpi send/receive with a defined request object
	pt::TreePattern mpiTransport = pt::var("start", pt::irp::callExpr(pt::lambda([](const core::NodePtr& nodePtr) {
		                                                                  return getNameForFunCall(nodePtr).find("MPI_I") == 0
		                                                                         && getNameForFunCall(nodePtr).find("MPI_Init") == std::string::npos;
		                                                              }),
	                                                                  pt::ListPattern(*pt::any << pt::irp::ptrFromRef(mpiRequest))));

	// matches MPI_Test with a given request object
	pt::TreePattern mpiTest = pt::irp::callExpr(pt::lambda([](const core::NodePtr& nodePtr) { return getNameForFunCall(nodePtr).find("MPI_Test") == 0; }),
	                                            pt::ListPattern(pt::irp::ptrFromRef(mpiRequest) << pt::any << pt::any));

	// matches MPI_Wait with a given request object
	pt::TreePattern mpiWait = pt::irp::callExpr(pt::lambda([](const core::NodePtr& nodePtr) { return getNameForFunCall(nodePtr).find("MPI_Wait") == 0; }),
	                                            pt::ListPattern(pt::irp::ptrFromRef(mpiRequest) << pt::any));

	// look for a while loop containing an MPI_test
	pt::TreePattern whileTestPattern = pt::irp::whileStmt(pt::any, pt::irp::compoundStmt(*pt::any << mpiTest << *pt::any));

	// matches a full non-blocking communication construct up to the blocking call
	pt::TreePattern fullMPI = pt::irp::compoundStmt(*pt::any << mpiTransport << *pt::any << pt::var("end", (mpiWait | whileTestPattern)) << *pt::any);

	unsigned index = 0;

	auto myRoot = root.getRootNode();

	pt::irp::matchAllPairsReverse(fullMPI, root, [&](const core::NodeAddress addr, pt::AddressMatch match) {
		auto startAddress = addr.concat(match["start"].getValue()).as<core::StatementAddress>();
		auto endAddress = addr.concat(match["end"].getValue()).as<core::StatementAddress>();

		retVal.push_back(Region(startAddress, endAddress, index++, "MPI"));
	});

	return retVal;
}

/*std::vector<core::StatementAddress> findLoopsWithMPI(const core::NodeAddress& addr) {
	std::vector<core::StatementAddress> retVal;

	// find all for loops
	core::analysis::region::ForSelector forSelector;
	auto loopList = forSelector.getRegions(addr);

	// keep only the ones containing MPI calls
	for(auto loop : loopList) {
		if(!findMPICalls(loop).empty()) { retVal.push_back(loop); }
	}

	LOG(INFO) << "Total number of for loops " << loopList.size() << ", of which " << retVal.size() << " contain(s) MPI calls";

	std::sort(retVal.begin(), retVal.end());
	
	return retVal;
}*/

std::vector<Region> getInterestingRegions(const core::NodeAddress& addr) {
	std::vector<Region> regions;
	//dm::region_id index = 0;

	regions = findMPICalls(addr);

	std::sort(regions.rbegin(), regions.rend());

	core::NodePtr root = addr.getRootNode();

	for(const auto& e : regions) {
		root = encloseInInstrumentation(root, e.start, e.end, e.regionID);
	}

	// look for regions again, since the addresses have changed with instrumentation
	// TODO: find a better way of doing this?
	regions = findMPICalls(core::NodeAddress(root));

	return regions;
}

int main(int argc, char** argv) {
	// Step 1: parse input parameters
	std::vector<std::string> arguments(argv, argv + argc);
	cmd::Options options = cmd::Options::parse(arguments);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }

	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	std::cout << "Insieme MPI model builder - Version: " << utils::getVersion() << "\n";

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

	if(!program || program->getEntryPoints().empty()) {
		LOG(ERROR) << "Unable to parse program or find entry point";
		return 1;
	};

	auto entryPoint = core::ExpressionAddress(program->getEntryPoints()[0]);

	const std::vector<Region> regionsWithName = getInterestingRegions(entryPoint);
	assert_false(regionsWithName.empty()) << "No region found!";
	const core::NodePtr root = regionsWithName.front().start.getRootNode();

	dumpPretty(root);

	// make sure instrumented IR is correct
	auto semas = core::checks::check(root);

	if(!semas.empty()) {
		LOG(ERROR) << "Found semantic errors, bailing out";
		return 1;
	}

	std::vector<std::string> metricString = { "total_wall_time(ns)" };
	std::vector<dm::MetricPtr> metrics = transform(metricString, [](const std::string& cur) { return dm::Metric::getForNameAndUnit(cur); });
	LOG(INFO) << "selected metrics: " << metrics;
	utils::compiler::Compiler mpicc("mpicc");
	mpicc.addFlag("-x c");
	mpicc.addFlag("-Wall");
	mpicc.addFlag("--std=gnu99");
	mpicc.addFlag("-DIRT_USE_MPI");
	std::map<string, string> env;
	// fix number of worker threads to 1, relying on mpirun alone seems insufficient for thread concurrency control
	env["IRT_NUM_WORKERS"] = "1";
	env["IRT_INST_REGION_INSTRUMENTATION"] = "enabled";
	// rely on mpirun to do process-core mapping
	const string mpiRun = "mpirun -np 2 --map-by core";
	const unsigned numRuns = 1;
	
	const auto result = dm::measurePreinstrumented(root, metrics, numRuns, std::make_shared<dm::LocalExecutor>(mpiRun), mpicc, env);

	assert_eq(result.size(), numRuns) << "Unexpected result structure!\n";

	auto resultPrinter = [&](std::vector<std::map<dm::region_id, std::map<dm::MetricPtr, dm::Quantity>>> data) {
		for(auto& run : data) {
			for(auto& region : run) {
				std::cout << boost::format("%4d\t(%24s)\t") % region.first % regionsWithName.at(region.first).label;
				std::cout << region.second << "\n";
			}
		}
	};

	resultPrinter(result);


	/*
	 * TODO:
	 * - Crucial
	 *   - region definition
	 *		- all mpi calls in loops
	 *		- instrument blocking MPI!
	 *		- all loop bodies holding mpi calls
	 *   - fix instrumentation to include MPI_Wait after non-blocking MPI calls 
	 *		- look for MPI statements, but distinguish blocking and non-blocking ("MPI_I" prefix?)
	 *			- if blocking: instrument statement, done
	 *			- if non-blocking: look for the next MPI_Wait or while with MPI_Test with matching MPI_request object, 
	 *							   outline code in between and instrument call to outlined function
	 *	- TF: Include MPI outside but adjacent to loops
	 *   - do region analysis, obtain features
	 * - Automated measurements
	 *   x add wrapper for program execution, probably just a string (do setup + parameter generation in driver)
	 *   - verify that all IRT instances are correctly mapped to cores
	 *   x IRT_NUM_WORKERS=1
	 *   x add suffix for worker efficiency log
	 *   o do data aggregation for worker efficiency log
	 * - Cleanup and nice-to-have
	 *   - add mpi compiler to utils?
	 *   - cleanup driver, remove non-functional cmd options, etc...
	 *   - set IRT_INST_REGION_INSTRUMENTATION=enabled env var even if there are no PAPI counters (c.f. measure.cpp)
	 **/

	return 0;
}
