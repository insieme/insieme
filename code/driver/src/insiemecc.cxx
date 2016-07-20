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

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/version.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/utils/file_extensions.h"

#include "insieme/backend/backend.h"

#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/object_file_utils.h"
#include "insieme/driver/utils/driver_utils.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/dump/json_dump.h"

#include "insieme/transform/tasks/granularity_tuning.h"

using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace dr = insieme::driver;
namespace cp = insieme::utils::compiler;
namespace cmd = insieme::driver::cmd;
namespace du = insieme::driver::utils;

int main(int argc, char** argv) {
	// Step 1: parse input parameters
	std::vector<std::string> arguments(argv, argv + argc);
	cmd::Options options = cmd::Options::parse(arguments);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }

	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	std::cout << "Insieme compiler - Version: " << utils::getVersion() << "\n";

	// Step 2: filter input files
	vector<fe::path> inputs;
	vector<fe::path> libs;
	vector<fe::path> extLibs;

	for(const fe::path& cur : options.job.getFiles()) {
		auto ext = fs::extension(cur);
		if(ext == ".o" || ext == ".so") {
			if(dr::isInsiemeLib(cur)) {
				libs.push_back(cur);
			} else {
				extLibs.push_back(cur);
			}
		} else if (fe::utils::cExtensions.count(ext) || fe::utils::cxxExtensions.count(ext)) {
			inputs.push_back(cur);
		} else {
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

	// dump the translation unit file (if needed for de-bugging)
	if(!options.settings.dumpTU.empty()) {
		std::cout << "Dumping Translation Unit ...\n";
		auto tu = options.job.toIRTranslationUnit(mgr);
		std::ofstream out(options.settings.dumpTU.string());
		out << tu;
	}


	std::cout << "Extracting executable ...\n";

	// convert src file to target code
	auto program = options.job.execute(mgr);

	// dump IR code
	if(!options.settings.dumpIR.empty()) {
		std::cout << "Dumping intermediate representation ...\n";
		std::ofstream out(options.settings.dumpIR.string());
		out << co::printer::PrettyPrinter(program, co::printer::PrettyPrinter::PRINT_DEREFS);
	}

	if(!options.settings.dumpJSON.empty()) {
		std::ofstream out(options.settings.dumpJSON.string());
		co::dump::json::dumpIR(out, program);
	}

	core::checks::MessageList errors;
	if(options.settings.checkSema || options.settings.checkSemaOnly) {
		int retval = du::checkSema(program, errors);
		if(options.settings.checkSemaOnly) { return retval; }
	}

	if(options.settings.showStatistics) { du::showStatistics(program); }

	if(options.settings.benchmarkCore) { du::benchmarkCore(program); }

	if(options.settings.taskGranularityTuning) { program = insieme::transform::tasks::applyTaskOptimization(program); }

	// Step 3: produce output code
	std::cout << "Creating target code ...\n";
	backend::BackendPtr backend = du::getBackend(program, options);
	auto targetCode = backend->convert(program);

	// dump source file if requested, exit if requested
	fe::path filePath = options.settings.dumpTRG;
	if(!options.settings.dumpTRGOnly.empty()) { filePath = options.settings.dumpTRGOnly; }
	if(!filePath.empty()) {
		std::cout << "Dumping target code ...\n";
		std::ofstream out(filePath.string());
		out << *targetCode;
		if(!options.settings.dumpTRGOnly.empty()) { return 0; }
	}

	// Step 4: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	//		if any of the translation units is has cpp belongs to cpp code, we'll use the
	//		cpp compiler, C otherwise
	cp::Compiler compiler = (options.job.isCxx()) ? cp::Compiler::getDefaultCppCompiler() : cp::Compiler::getDefaultC99Compiler();

	switch(options.backendHint.backend) {
	case cmd::BackendEnum::Sequential: break;
	case cmd::BackendEnum::OpenCL: compiler = cp::Compiler::getOpenCLCompiler(compiler); break;
	case cmd::BackendEnum::Runtime:
	case cmd::BackendEnum::Pthreads:
	default: compiler = cp::Compiler::getRuntimeCompiler(compiler);
	}

	// add needed library flags
	for(auto cur : extLibs) {
		string libname = cur.filename().string();
		// add libraries by splitting their paths, truncating the filename of the library in the process (lib*.so*)
		compiler.addExternalLibrary(cur.parent_path().string(), libname.substr(3, libname.find(".") - 3));
	}

	// add needed includeDirs for intercepted stuff
	for(auto cur : options.job.getInterceptedHeaderDirs()) {
		compiler.addIncludeDir(cur.string());
	}

	// add the given optimization flags (-f flags)
	for(auto cur : options.job.getFFlags()) {
		compiler.addFlag(cur);
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
		compiler.addFlag("-std=c++11");
	} else {
		compiler.addFlag("-std=c99");
	}

	return !cp::compileToBinary(*targetCode, options.settings.outFile.string(), compiler);
}
