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
 */

#include "insieme/backend/backend.h"
#include "insieme/backend/addons/progress_reporting.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/cmd/common_options.h"
#include "insieme/driver/utils/driver_utils.h"
#include "insieme/driver/utils/object_file_utils.h"

#include "insieme/transform/progress_estimation.h"

#include "insieme/utils/version.h"


using namespace insieme;

int main(int argc, char** argv) {
	std::cout << "Insieme Progress Estimation - Version: " << utils::getVersion() << "\n";

	// object holding common command line options
	driver::cmd::CommonOptions commonOptions;

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	// register common options and flags needed by more than one driver
	commonOptions.addFlagsAndParameters(parser);
	transform::ProgressReportingType reportingLimit;
	float maxIfElseReportingDifferenceFactor = 0.02;
	transform::ProgressReportingType reportingMinimum;
	std::string backendString;
	parser.addParameter("reporting-limit", reportingLimit, transform::ProgressReportingType{ 1000 }, "progress reporting limit");
	parser.addParameter("reporting-max-branch-difference-factor", maxIfElseReportingDifferenceFactor, 0.02f, "0.2", "factor of reporting limit, above which reportings in if and else bodies will not be merged/averaged");
	parser.addParameter("reporting-minimum", reportingMinimum, transform::ProgressReportingType{ 20 }, "minimum value required for reportings to be generated (use 0 to emit all reportings)");
	parser.addParameter("backend",         backendString,  std::string(""),                          "backend selection (for compatibility reasons - ignored)");
	auto options = parser.parse(argc, argv);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }
	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	if(!backendString.empty()) {
		std::cout << "WARNING: The --backend option has been specified. this option is supported only for compatibility reasons and will be ignored." << std::endl;
	}

	// Step 2: filter input files
	core::NodeManager mgr;
	if(!driver::utils::filterInputFiles(mgr, options.job)) {
		return 1;
	}

	// Step 3: load input code

	// convert src file to IR
	std::cout << "Converting input code to IR ...\n";
	auto program = options.job.execute(mgr);

	// ----- progress estimation ------

	std::cout << "Applying progress estimation with a reporting limit of " << reportingLimit << " ...\n";
	std::cout << "  If/Else reportings below a factor of " << maxIfElseReportingDifferenceFactor << " (="
			<< (reportingLimit * maxIfElseReportingDifferenceFactor) << ") of the reporting limit will be merged/averaged\n";
	std::cout << "  Emitting only reportings with progress >= " << reportingMinimum << "\n";
	auto outputProgram = insieme::transform::applyProgressEstimation(program, reportingLimit, maxIfElseReportingDifferenceFactor, reportingMinimum);

	// Note: The code below has been more or less copied directly from insiemecc

	// Step 4: produce output code
	std::cout << "Creating target code ...\n";
	backend::BackendPtr backend = driver::utils::getBackend("run", "");
	if(!backend) { return 1; }

	// install the required backend addon which converts reporting calls
	backend->addAddOn<backend::addons::ProgressReporting>();

	auto targetCode = backend->convert(outputProgram);

	// dump target code
	{
		frontend::path filePath = commonOptions.outFile;
		// we append a suffix with the same extension to the output filename if we shouldn't stop after dumping
		if(!commonOptions.dumpTRGOnly) {
			filePath = filePath.concat(std::string("_generated") + (commonOptions.outFile.has_extension() ? commonOptions.outFile.extension().string() : ""));
		}
		std::cout << "Dumping target code to " << filePath << " ...\n";
		std::ofstream out(filePath.string());
		out << *targetCode;
		// and exit if requested
		if(commonOptions.dumpTRGOnly) { return 0; }
	}

	// Step 5: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	//		if any of the translation units is has cpp belongs to cpp code, we'll use the
	//		cpp compiler, C otherwise
	insieme::utils::compiler::Compiler compiler = driver::utils::getCompiler("run", options.job.isCxx());

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

	// add unknown options - might be used by backend compiler
	for(auto cur : options.job.getUnparsedOptions()) {
		compiler.addFlag(cur);
	}

	// add definitions
	for(auto cur : options.job.getDefinitions()) {
		compiler.addFlag(std::string("-D" + cur.first));
	}

	// add define to enable progress reporting in the runtime system
	compiler.addFlag("-DIRT_ENABLE_PROGRESS_REPORTING");

	// if we are compiling C++ code, we need to set the backend compiler standard
	if(options.job.isCxx()) {
		compiler.addFlag("-std=c++14");
	} else {
		compiler.addFlag("-std=c99");
	}

	return !insieme::utils::compiler::compileToBinary(*targetCode, commonOptions.outFile.string(), compiler);
}
