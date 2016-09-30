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

#include <iostream>
#include <string>
#include <sstream>

#include "insieme/driver/perf_reg/structures.h"
#include "insieme/driver/perf_reg/toolbox.h"
#include "insieme/driver/perf_reg/options.h"
#include "insieme/driver/perf_reg/log_parsing.h"
#include "insieme/driver/perf_reg/data_processing.h"
#include "insieme/driver/perf_reg/analyzer.h"
#include "insieme/driver/perf_reg/html_gen.h"

using namespace std;
using namespace insieme::driver::perf_reg;


double smallTestsThreshold(double percent, double absolute)
{
	// Return x where x * percent == x + absolute
	return (100.0 * absolute) / percent;
}


bool runtimeCheck(Analyzer::ArgType in)
{
	double percentTolerance    = 15.0,  // %
	       secondsTolerance    =  0.8;  // sec

	double currSec = in.vec.at(0).val;
	double avgSec  = in.avg;

	double factorTolerance = 1.0 + (percentTolerance / 100.0);

	// Small values: Check time drift in absolute values
	if (currSec < smallTestsThreshold(percentTolerance, secondsTolerance))
		return avgSec + secondsTolerance > currSec;

	// Larger values: Check using percentage
	return avgSec * factorTolerance > currSec;
}


bool memoryUsageCheck(Analyzer::ArgType in)
{
	double percentTolerance     = 10.0,  // %
	       mbTolerance          =  1.2;  // MB

	double currMem = in.vec.at(0).val;
	double avgMem  = in.avg;

	double factorTolerance = 1.0 + (percentTolerance / 100.0);

	// Small values: Use absolute values to check
	if (currMem < smallTestsThreshold(percentTolerance, mbTolerance))
		return avgMem + mbTolerance > currMem;

	// Larger values: Check using percentage
	return avgMem * factorTolerance > currMem;
}


int main(int argc, char **argv)
{
	// Read command line options
	Options &args = Options::getInstance();
	if (!args.parseCommandLine(argc, argv)) {
		LOG(ERROR) << "Could not parse command line arguments";
		return EXIT_FAILURE;
	}

	// Check if input files exist
	if (!tools::allFilesExist(args.inputFiles)) {
		LOG(ERROR) << "Not all given input files exist";
		return EXIT_FAILURE;
	}

	// Scan timestamps of files to determine order
	if (!scanTimestamps()) {
		LOG(ERROR) << "Not all given input files have a timestamp in their first line";
		return EXIT_FAILURE;
	}

	// Load file contents
	if (!loadLogfiles()) {
		LOG(ERROR) << "Could not parse given log files";
		return EXIT_FAILURE;
	}

	// Preprocess log data
	if (!preprocessData()) {
		LOG(ERROR) << "Error while preprocessing data";
		return EXIT_FAILURE;
	}

	// Perform actual performance regression check
	auto &analyzer = Analyzer::getInstance();
	analyzer.mapFunction("RuntimeSec", runtimeCheck);
	analyzer.mapFunction("MemoryMB", memoryUsageCheck);

	performanceRegressionCheck();

	// Write out results
	map<string,string> units = {
	        {"RuntimeSec", "seconds"},
	        {"MemoryMB"  , "MB"}
	};

	if (!writeHTML(units)) {
		LOG(ERROR) << "Unable to write out results!";
		return EXIT_FAILURE;
	}

	// Create or delete "fail file" if requested
	if (args.failFile.size() > 0) {
		auto &data = Data::getInstance();
		if (data.result.size() > 0) {
			tools::quickWriteFile(args.failFile, "\n");
		} else {
			tools::quickRemoveFile(args.failFile);
		}
	}

	return EXIT_SUCCESS;
}
