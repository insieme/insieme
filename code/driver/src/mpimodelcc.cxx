/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <string>
#include <iomanip>

#include <boost/algorithm/string.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/format.hpp>
#include <boost/filesystem.hpp>

#include "insieme/core/analysis/region/for_selector.h"
#include "insieme/core/analysis/region/fun_call_selector.h"
#include "insieme/core/analysis/region/mpi_selector.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/instrumentation_extension.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/driver/measure/measure.h"
#include "insieme/driver/measure/dump.h"
#include "insieme/driver/utils/driver_utils.h"
#include "insieme/driver/utils/object_file_utils.h"
#include "insieme/driver/cmd/commandline_options.h"

#include "insieme/frontend/frontend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/config.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/version.h"

using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace cr = insieme::core::analysis::region;
namespace dr = insieme::driver;
namespace cmd = insieme::driver::cmd;
namespace dm = insieme::driver::measure;
namespace pt = insieme::core::pattern;
namespace du = insieme::driver::utils;

class RegionAttribute : public utils::Printable {
public:
	cr::Region region;
	std::string label;

	RegionAttribute(const cr::Region region, const std::string label)
		: region(region), label(label) { }

	// sort according to addresses
	bool operator<(const RegionAttribute other) const { return std::tie(region, label) < std::tie(other.region, other.label); }

	virtual std::ostream& printTo(std::ostream& out) const {
		return out << "[" << region << ", " << label << "]";
	}
};

using RegionMap = std::map<dm::region_id, const RegionAttribute>;

class RegionDatabase : public utils::Printable {
private:
	RegionMap regions;
	unsigned index;

public:
	RegionDatabase() : regions(), index(0) { }

	void addRegion(const cr::Region region, const std::string label) {
		auto uniqueLabel = label;
		unsigned counter = 0;
		auto hasLabel = [&](const auto& entry) {
			if(entry.second.label == uniqueLabel) { return true; }
			return false;
		};
		while(any(regions, hasLabel)) {
			uniqueLabel = boost::str(boost::format("%s_%u") % label % ++counter);
		}

		regions.insert(std::make_pair(index++, RegionAttribute(region, uniqueLabel)));
	}

	const RegionMap& getAllRegions() const {
		return regions;
	}

	bool empty() const {
		return regions.empty();
	}

	std::size_t size() const {
		return regions.size();
	}

	void mergeIn(const RegionDatabase& other) {
		for(const auto& e : other.getAllRegions()) {
			addRegion(e.second.region, e.second.label);
		}
	}

	virtual std::ostream& printTo(std::ostream& out) const {
		return out << "{" << join("|", regions) << "}";
	}
};

std::string getNameForAddress(const core::NodeAddress& funAddr) {
	std::string retVal("");
	auto funPtr = funAddr.getAddressedNode();
	if(auto exprPtr = funPtr.isa<core::CallExprPtr>()) { funPtr = exprPtr->getFunctionExpr(); }
	if(auto litPtr = funPtr.isa<core::LiteralPtr>()) { retVal = utils::demangle(litPtr->getStringValue()); }

	if(auto loc = core::annotations::getLocation(funAddr)) { retVal += "_" + toString(loc.get()); }
	return retVal;
}

core::NodePtr encloseInInstrumentation(const core::NodePtr& root, const core::NodeAddress& begin, const core::NodeAddress& finish, const unsigned regionID) {
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

	// instrument bottom up
	core::NodeAddress newFinish = finish.switchRoot(myRoot);
	myRoot = core::transform::insertAfter(manager, newFinish.as<core::StatementAddress>(), region_inst_end_call);

	core::NodeAddress newStart = begin.switchRoot(myRoot);
	myRoot = core::transform::insertBefore(manager, newStart.as<core::StatementAddress>(), region_inst_start_call);

	return myRoot;
}

RegionDatabase findMPICalls(const core::NodeAddress& root) {
	RegionDatabase retVal;

	core::analysis::region::MPISelector mpiSelector(true);
	//core::analysis::region::FunctionCallSelector funCallSelector("MPI_All");

	//auto regions = funCallSelector.getRegions(root);
	auto regions = mpiSelector.getRegions(root);

	for(const auto& e : regions) {
		retVal.addRegion(e, getNameForAddress(e.getBegin()));
	}

	return retVal;
}

RegionDatabase findLoopsWithMPI(const core::NodeAddress& addr) {
	RegionDatabase retVal;
	// use visitor to find for loops rather than pattern matcher for performance reasons
	std::vector<core::StatementAddress> forLoops;
	core::visitDepthFirstPrunable(addr,
	                              [&](const core::StatementAddress& cur) -> core::Action {
		                              auto nT = cur.getAddressedNode()->getNodeType();
		                              if(nT != core::NT_ForStmt && nT != core::NT_WhileStmt) { return core::Action::Descent; }
		                              forLoops.push_back(cur);
		                              return core::Action::Prune;
		                          },
	                              false);

	// core::analysis::region::MPISelector mpiSelector;
	// select everything but MPI_Wtime, which is not of interest
	// (and is frequently seen within assignment expressions, and hence cannot be properly instrumented)
	//core::analysis::region::FunctionCallSelector funCalSelector("MPI_");
	core::analysis::region::FunctionCallSelector funCalSelector("MPI_All|MPI_Gather|MPI_Scatter|MPI_Send|MPI_Recv|MPI_Isend|MPI_Irecv|MPI_Barrier|MPI_Test|MPI_Wait|MPI_Reduce");
	//auto mpiAsyncPattern = mpiSelector.getMPIAsyncPattern();

	// check all for loops for MPI calls
	for(const auto& loop : forLoops) {
		auto regions = funCalSelector.getRegions(loop);
		//auto match = pt::aT(mpiAsyncPattern).match(loop);
		if(!regions.empty()) {
			retVal.addRegion(cr::Region(loop), "for" + getNameForAddress(loop));
		}
	}

	return retVal;
}

RegionDatabase getInterestingRegions(const core::NodeAddress& addr) {
	RegionDatabase retVal;

	const auto mpiCalls = findMPICalls(addr);
	retVal.mergeIn(mpiCalls);
	const auto loops = findLoopsWithMPI(addr);
	retVal.mergeIn(loops);

	return retVal;
}

using problemType = unsigned;
using machineSizeType = unsigned;
using machineType = dm::Machine;

template <typename MetricType, typename QuantityType>
class resultType : public std::map<machineSizeType, std::map<problemType, std::vector<std::map<dm::region_id, std::map<MetricType, QuantityType>>>>> {
  private:
	friend class boost::serialization::access;

	template <class Archive>
	void serialize(Archive& ar, const unsigned int version) {
		ar& boost::serialization::base_object<std::map<machineSizeType,
		                                               std::map<problemType, std::vector<std::map<dm::region_id, std::map<MetricType, QuantityType>>>>>>(*this);
	}
};

template<typename MetricType, typename QuantityType>
resultType<MetricType, QuantityType> aggregateData(const resultType<MetricType, QuantityType>& data) {
	resultType<MetricType, QuantityType> aggregatedData;

	for(const auto& machine : data) {
		for(const auto& problem : machine.second) {
			std::map<dm::region_id, std::map<MetricType, std::vector<QuantityType>>> tempData;
			// convert structure to be able to sort lists of metric data
			for(unsigned run = 0; run < problem.second.size(); ++run) {
				const auto program = problem.second.at(run);
				for(const auto& region : program) {
					for(const auto& metric : region.second) {
						tempData[region.first][metric.first].push_back(metric.second);
					}
				}
			}
			// sort quantities per metric in ascending order
			for(auto& region : tempData) {
				for(auto& metric : region.second) {
					std::sort(metric.second.begin(), metric.second.end(), [](const dm::Quantity& a, const dm::Quantity& b) { return a < b; });
				}
			}
			// take median
			std::map<dm::region_id, std::map<MetricType, QuantityType>> tempMap;
			for(const auto& region : tempData) {
				for(const auto& metric : region.second) {
					tempMap[region.first][metric.first] = tempData[region.first][metric.first][tempData[region.first][metric.first].size() / 2];
				}
			}
			aggregatedData[machine.first][problem.first].push_back(tempMap);
		}
	}

	return aggregatedData;
}

resultType<dm::LightweightMetric, dm::LightweightQuantity> convertToLW(const resultType<dm::MetricPtr, dm::Quantity>& data) {
	resultType<dm::LightweightMetric, dm::LightweightQuantity> retVal;
	for(const auto& machine : data) {
		for(const auto& problem : machine.second) {
			for(const auto& run : problem.second) {
				std::map<dm::region_id, std::map<dm::LightweightMetric, dm::LightweightQuantity>> temp;
				for(const auto& region : run) {
					for(const auto& metric : region.second) {
						temp[region.first][dm::LightweightMetric(metric.first)] = dm::LightweightQuantity(metric.second);
					}
				}
				retVal[machine.first][problem.first].push_back(temp);
			}
		}
	}
	return retVal;
}

template <typename MetricType, typename QuantityType>
void resultPrinterAggregated(const resultType<MetricType, QuantityType>& data, const RegionDatabase& regionDatabase) {
	du::openBoxTitle("Aggregated Results (median)");
	std::cout << "machine\tproblem\tregion_id\tregion_name\t";
	// print all metrics in header (assumes all regions hold the same metrics)
	for(const auto& metric : data.begin()->second.begin()->second.front().begin()->second) {
		std::cout << metric.first << "\t";
	}
	std::cout << "\n";
	for(const auto& machine : data) {
		for(const auto& problem : machine.second) {
			assert_eq(problem.second.size(), 1) << "Aggregated results printer expects a single set of data per region but received " << problem.second.size()
			                                    << " instead !";
			for(const auto& region : problem.second.front()) {
				std::cout << boost::format("%3d %10d %4d\t%24s\t") % machine.first % problem.first % region.first
				                 % regionDatabase.getAllRegions().at(region.first).label;
				for(const auto& metric : region.second) {
					std::cout << boost::format(" %20s") % metric.second;
				}
				std::cout << "\n";
			}
		}
	}
}

template <typename MetricType, typename QuantityType>
void resultPrinterFull(const resultType<MetricType, QuantityType>& data, const RegionDatabase& regionDatabase) {
	du::openBoxTitle("Full Results");
	for(const auto& machine : data) {
		for(const auto& problem : machine.second) {
			std::cout << "Data for problem size: " << problem.first << "\n";
			for(unsigned run = 0; run < problem.second.size(); ++run) {
				const auto program = problem.second.at(run);
				for(const auto& region : program) {
					std::cout << boost::format("%3d %10d %2d %4d\t%24s\t %s") % machine.first % problem.first % run % region.first
						% regionDatabase.getAllRegions().at(region.first).label % regionDatabase.getAllRegions().at(region.first).region;
					std::cout << region.second << "\n";
				}
			}
		}
	}
}

int main(int argc, char** argv) {
	bool compileOnly = false;
	frontend::path outFile, dumpIR;

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	parser.addFlag(     "compile,c", compileOnly,                          "compilation only");
	parser.addParameter("outfile,o", outFile,     frontend::path("a.out"), "output file");
	parser.addParameter("dump-ir",   dumpIR,      frontend::path(),        "dump intermediate representation");
	auto options = parser.parse(argc, argv);

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
			if(du::isInsiemeLib(cur)) {
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
	bool createSharedObject = fs::extension(outFile) == ".so";

	// update input files
	options.job.setFiles(inputs);

	// Step 3: load input code
	co::NodeManager mgr;

	// load libraries
	options.job.setLibs(::transform(libs, [&](const fe::path& cur) {
		std::cout << "Loading " << cur << " ...\n";
		return du::loadLib(mgr, cur);
	}));

	// if it is compile only or if it should become an object file => save it
	if(compileOnly || createSharedObject) {
		auto res = options.job.toIRTranslationUnit(mgr);
		std::cout << "Saving object file ...\n";
		du::saveLib(res, outFile);
		return du::isInsiemeLib(outFile) ? 0 : 1;
	}

	std::cout << "Extracting executable ...\n";

	// convert src file to target code

	LOG(INFO) << "Looking for regions ...\n";
	utils::Timer timer = insieme::utils::Timer("Frontend conversion");
	auto program = options.job.execute(mgr);
	timer.stop(); LOG(INFO) << timer;

	assert_true(program && !program->getEntryPoints().empty()) << "Unable to parse program or find entry point";

	auto rootAddr = core::ExpressionAddress(program->getEntryPoints()[0]);
	core::NodePtr rootPtr = rootAddr.getAddressedNode();

	timer = insieme::utils::Timer("Region search");

	RegionDatabase regionDatabase;

	// works with addresses
	regionDatabase = getInterestingRegions(rootAddr);
	// works with pointers
	//regionDatabase = getInterestingRegions(rootPtr);

	timer.stop(); LOG(INFO) << timer;

	assert_false(regionDatabase.empty()) << "No regions found!";

	LOG(INFO) << "Found " << regionDatabase.size() << " region(s)";

	timer = insieme::utils::Timer("Region instrumentation");

	std::map<RegionAttribute, dm::region_id> attributeMap;

	for(const auto& e : regionDatabase.getAllRegions()) {
		attributeMap[e.second] = e.first;
	}

	LOG(INFO) << "Instrumenting regions ...\n";

	for(auto it = attributeMap.rbegin(); it != attributeMap.rend(); ++it) {
		rootPtr = encloseInInstrumentation(rootPtr, it->first.region.getBegin(), it->first.region.getEnd(), it->second);
	}

	timer.stop(); LOG(INFO) << timer;

	// dump IR code
	if(!dumpIR.empty()) {
		std::cout << "Dumping intermediate representation ...\n";
		std::ofstream out(dumpIR.string());
		out << co::printer::PrettyPrinter(rootPtr, co::printer::PrettyPrinter::PRINT_DEREFS);
	}

	//LOG(INFO) << "Running semantic checks, just to be safe ...\n";

	// make sure instrumented IR is correct
	//auto semas = core::checks::check(rootPtr);
	//assert_true(semas.empty()) << "Found semantic errors, please check instrumented IR";

	LOG(INFO) << "Compiling and measuring ...\n";

	std::vector<dm::MetricPtr> metrics = {dm::Metric::TOTAL_WALL_TIME,     dm::Metric::TOTAL_NUM_EXEC,       dm::Metric::TOTAL_CPU_ENERGY,
	                                      dm::Metric::TOTAL_AVG_WALL_TIME, dm::Metric::TOTAL_AVG_CPU_ENERGY/*, dm::Metric::TOTAL_PAPI_LD_INS,
					      dm::Metric::TOTAL_PAPI_SR_INS, dm::Metric::TOTAL_PAPI_L3_TCM*/
					      /*dm::Metric::TOTAL_PAPI_TOT_INS,
	                                      dm::Metric::TOTAL_PAPI_L3_TCM,   dm::Metric::TOTAL_PAPI_L2_TCM,    dm::Metric::TOTAL_PAPI_BR_INS,
	                                      dm::Metric::TOTAL_PAPI_STL_ICY*/};

	LOG(INFO) << "Selected metrics: " << metrics;

	auto measurementSetup = driver::measure::MeasurementSetup();
	measurementSetup.compiler = utils::compiler::Compiler("mpicc");
	measurementSetup.compiler.addFlag("-x c");
	measurementSetup.compiler.addFlag("-Wall");
	measurementSetup.compiler.addFlag("--std=gnu99");
	measurementSetup.compiler.addFlag("-DIRT_USE_MPI");
	measurementSetup.compiler.addFlag("-DIRT_USE_PAPI");
	measurementSetup.compiler.addFlag("-O3");
	measurementSetup.compiler.addFlag(string("-Wl,-rpath,") + utils::getPapiRootDir() + "lib -lpapi -lpfm");

	// compile binary
	const auto binary = dm::buildBinary(rootPtr, measurementSetup);

	vector<problemType> problemSizes;
	vector<machineType> machines;
	const unsigned firstParam = 256;
	//const unsigned secondParam = 128;

	for(unsigned i = 1; i <= 1; ++i) {
		problemSizes.push_back(firstParam * std::pow(2, i)/*, secondParam*/);
	}

	machines.push_back(dm::Machine("ortler", { dm::Node("o4", 1), dm::Node("o5", 1) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o4", 2), dm::Node("o5", 2) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o4", 4), dm::Node("o5", 4) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o4", 8), dm::Node("o5", 8) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o4", 16), dm::Node("o5", 16) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o4", 32), dm::Node("o5", 32) }));

	//machines.push_back(dm::Machine("ortler", { dm::Node("o4", 8), dm::Node("o5", 8) }));

	//machines.push_back(dm::Machine("ortler", { dm::Node("o5", 2) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o5", 4) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o5", 8) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o5", 16) }));
	//machines.push_back(dm::Machine("ortler", { dm::Node("o5", 32) }));

	resultType<dm::MetricPtr, dm::Quantity> overallResults;

	for(const auto& machine : machines) {
		const unsigned machineSize = machine.getNumCores();
		for(const auto& problemSize : problemSizes) {

			// fix number of worker threads to 1, relying on mpirun alone seems insufficient for thread concurrency control
			measurementSetup.executionSetup.env["IRT_NUM_WORKERS"] = "1";
			measurementSetup.executionSetup.env["IRT_INST_REGION_INSTRUMENTATION"] = "enabled";
			// TODO: needs fixing, since it might clash with mpirun affinity masks
			measurementSetup.executionSetup.env["IRT_AFFINITY_POLICY"] = "IRT_AFFINITY_FILL";
			// set problem sizes
			measurementSetup.executionSetup.params = { toString(problemSize), "1" };
			measurementSetup.executionSetup.machine = machine;
			measurementSetup.executor = dm::makeMPIExecutor("--bind-to core --map-by core");
			std::cout << measurementSetup << std::endl;

			const unsigned numRuns = 1;

			std::vector<std::map<dm::region_id, std::map<dm::MetricPtr, dm::Quantity>>> result;

			while(result.size() < numRuns) {
				measurementSetup.numRuns = numRuns - result.size();
				timer = insieme::utils::Timer("Running measurement");
				const auto remainingResults = dm::measure(binary, metrics, measurementSetup);
				timer.stop(); LOG(INFO) << timer;
				assert_gt(remainingResults.size(), 0) << "Expected at least a single result set but found none, bailing out";
				addAll(result, remainingResults);
			}

			assert_eq(result.size(), numRuns) << "Unexpected result structure!\n";

			LOG(INFO) << "Measurement done for machine " << machine << " and problem size " << problemSize << ", processing data ...\n";

			overallResults[machineSize][problemSize] = result;

		}
	}

	//std::cout << overallResults << std::endl;

	std::cout << "Dumping Data..." << std::endl;

	std::ofstream ofs("measurements.dump");
	// save data to archive
	{
		boost::archive::text_oarchive oa(ofs);
		auto temp = convertToLW(aggregateData(overallResults));
		oa << temp;
	}

	std::ifstream ifs("measurements.dump");
	// load data from archive
	resultType<dm::LightweightMetric, dm::LightweightQuantity> loadedData;
	{
		boost::archive::text_iarchive ia(ifs);
		ia >> loadedData;
	}

	//std::cout << loadedData << "\n";

	resultPrinterFull(overallResults, regionDatabase);
	std::cout << "############################################################\n";
	resultPrinterAggregated(loadedData, regionDatabase);

	/*
	 * TODO:
	 * - Crucial
	 *   - region definition
	 *      x instrument blocking MPI patterns and test them!
	 *		- what to do with async MPI primitives followed by barrier?
	 *		- all mpi calls in loops
	 *		x all loop bodies holding mpi calls
	 *   x fix instrumentation to include MPI_Wait after non-blocking MPI calls
	 *		x look for MPI statements, but distinguish blocking and non-blocking ("MPI_I" prefix?)
	 *			x if blocking: instrument statement, done
	 *			x if non-blocking: look for the next MPI_Wait or while with MPI_Test with matching MPI_request object,
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
	 *   - move getMPIAsyncPattern to anonymous namespace, lookup mpi loops by comparing addresses and not searching again for mpi inside them
	 *   - clean up includes
	 *   - unit tests!
	 *   - add mpi compiler to utils?
	 *   - cleanup driver, remove non-functional cmd options, etc...
	 *   - set IRT_INST_REGION_INSTRUMENTATION=enabled env var even if there are no PAPI counters (c.f. measure.cpp)
	 **/

	return 0;
}
