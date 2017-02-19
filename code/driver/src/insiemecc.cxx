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
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/version.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/utils/file_extensions.h"

#include "insieme/backend/backend.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/cmd/common_options.h"
#include "insieme/driver/utils/driver_utils.h"
#include "insieme/driver/utils/object_file_utils.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/dump/json_dump.h"

#include "insieme/transform/tasks/granularity_tuning.h"

using namespace std;
using namespace insieme;


int main(int argc, char** argv) {
	std::cout << "Insieme compiler - Version: " << utils::getVersion() << "\n";

	// object holding common command line options
	driver::cmd::CommonOptions commonOptions;
	// additional fields we need to store parser options
	bool benchmarkCore = false;
	bool showStatistics = false;
	bool taskGranularityTuning = false;
	std::string backendString;
	frontend::path dumpCFG, dumpJSON, dumpTree, dumpTU, dumpOclKernel;
	std::vector<std::string> optimizationFlags;

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	// register common options and flags needed by more then one driver
	commonOptions.addFlagsAndParameters(parser);

	// register insiemecc specific flags and parameters
	parser.addFlag(     "show-stats",              showStatistics,                                "computes statistics regarding the composition of the IR");
	parser.addFlag(     "benchmark-core",          benchmarkCore,                                 "benchmarking of some standard core operations on the intermediate representation");
	parser.addFlag(     "task-granularity-tuning", taskGranularityTuning,                         "enables multiverisoning of parallel tasks");
	parser.addParameter("backend",                 backendString,     std::string("runtime"),     "backend selection");
	parser.addParameter("dump-cfg",                dumpCFG,           frontend::path(),           "print dot graph of the CFG");
	parser.addParameter("dump-tree",               dumpTree,          frontend::path(),           "dump intermediate representation (Tree)");
	parser.addParameter("dump-json",               dumpJSON,          frontend::path(),           "dump intermediate representation (JSON)");
	parser.addParameter("dump-tu",                 dumpTU,            frontend::path(),           "dump translation unit");
	parser.addParameter("dump-kernel",             dumpOclKernel,     frontend::path(),           "dump OpenCL kernel");
	parser.addParameter("fopt",                    optimizationFlags, std::vector<std::string>(), "optimization flags");
	auto options = parser.parse(argc, argv);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }
	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }


	// Step 2: filter input files
	core::NodeManager mgr;
	if(!driver::utils::filterInputFiles(mgr, options.job)) {
		return 1;
	}

	// Step 3: load input code

	// indicates that a shared object file should be created
	bool createSharedObject = boost::filesystem::extension(commonOptions.outFile) == ".so";


	// if it is compile only or if it should become an object file => save it
	if(commonOptions.compileOnly || createSharedObject) {
		auto res = options.job.toIRTranslationUnit(mgr);
		std::cout << "Saving object file ...\n";
		driver::utils::saveLib(res, commonOptions.outFile);
		return driver::utils::isInsiemeLib(commonOptions.outFile) ? 0 : 1;
	}

	// dump the translation unit file (if needed for de-bugging)
	if(!dumpTU.empty()) {
		std::cout << "Dumping Translation Unit ...\n";
		auto tu = options.job.toIRTranslationUnit(mgr);
		std::ofstream out(dumpTU.string());
		out << tu;
	}


	std::cout << "Extracting executable ...\n";

	// convert src file to target code
	auto program = options.job.execute(mgr);

	// dump IR code
	if(!commonOptions.dumpIR.empty()) {
		std::cout << "Dumping intermediate representation ...\n";
		std::ofstream out(commonOptions.dumpIR.string());
		out << core::printer::PrettyPrinter(program, core::printer::PrettyPrinter::PRINT_DEREFS);
	}

	// dump JSON IR representation
	if(!dumpJSON.empty()) {
		std::cout << "Dumping JSON representation ...\n";
		std::ofstream out(dumpJSON.string());
		core::dump::json::dumpIR(out, program);
	}

	// dump Text IR representation
	if(!dumpTree.empty()) {
		std::cout << "Dumping Text representation ...\n";
		std::ofstream out(dumpTree.string());
		dumpText(program, out);
	}

	// perform semantic checks
	if(commonOptions.checkSema || commonOptions.checkSemaOnly) {
		core::checks::MessageList errors;
		int retval = driver::utils::checkSema(program, errors);
		if(commonOptions.checkSemaOnly) { return retval; }
	}

	if(showStatistics) { driver::utils::showStatistics(program); }

	if(benchmarkCore) { driver::utils::benchmarkCore(program); }

	if(taskGranularityTuning) { program = insieme::transform::tasks::applyTaskOptimization(program); }

	// Step 3: produce output code
	std::cout << "Creating target code ...\n";
	backend::BackendPtr backend = driver::utils::getBackend(backendString, dumpOclKernel.string());
	if(!backend) { return 1; }
	auto targetCode = backend->convert(program);

	// dump source file if requested, exit if requested
	frontend::path filePath = commonOptions.dumpTRG;
	if(!commonOptions.dumpTRGOnly.empty()) { filePath = commonOptions.dumpTRGOnly; }
	if(!filePath.empty()) {
		std::cout << "Dumping target code ...\n";
		std::ofstream out(filePath.string());
		out << *targetCode;
		if(!commonOptions.dumpTRGOnly.empty()) { return 0; }
	}

	// Step 4: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	//		if any of the translation units is has cpp belongs to cpp code, we'll use the
	//		cpp compiler, C otherwise
	insieme::utils::compiler::Compiler compiler = driver::utils::getCompiler(backendString, options.job.isCxx());

	// add needed external library flags
	for(auto cur : options.job.getExtLibs()) {
		string libname = cur.filename().string();
		// add libraries by splitting their paths, truncating the filename of the library in the process (lib*.so*)
		compiler.addExternalLibrary(cur.parent_path().string(), libname.substr(3, libname.find(".") - 3));
	}

	// add library flags
	for(auto lib : options.settings.libraryFiles) {
		compiler.addLibrary(lib.string());
	}

	// add needed includeDirs for intercepted stuff
	for(auto cur : options.job.getInterceptedHeaderDirs()) {
		compiler.addIncludeDir(cur.string());
	}

	// add the given optimization flags (-f flags)
	for(auto optFlag : optimizationFlags) {
		std::string&& s = "-f" + optFlag;
		compiler.addFlag(s);
	}

	// add unknown options - might be used by backend compiler
	for(auto cur : options.job.getUnparsedOptions()) {
		compiler.addFlag(cur);
	}

	// add definitions
	for(auto cur : options.job.getDefinitions()) {
		compiler.addFlag(std::string("-D" + cur.first));
	}

	// if we are compiling C++ code, we need to set the backend compiler standard
	if(options.job.isCxx()) {
		compiler.addFlag("-std=c++14");
	} else {
		compiler.addFlag("-std=c99");
	}

	return !insieme::utils::compiler::compileToBinary(*targetCode, commonOptions.outFile.string(), compiler);
}
