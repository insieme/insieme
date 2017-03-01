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
#include <boost/filesystem.hpp>

#include "insieme/utils/version.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/utils/file_extensions.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/utils/driver_utils.h"
#include "insieme/driver/utils/object_file_utils.h"

#include "insieme/analysis/features/effort_estimation.h"
#include "insieme/analysis/features/stack_size_estimation.h"
#include "insieme/analysis/features/task_utils.h"

using namespace insieme;


int main(int argc, char** argv) {
	std::cout << "Insieme Task Analysis - Version: " << utils::getVersion() << "\n";

	// Step 1: parse input parameters
	std::vector<std::string> arguments(argv, argv + argc);
	auto options = driver::cmd::Options::parse(arguments);

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

	// convert src file to IR
	auto program = options.job.execute(mgr);

	// ----- quick and dirty effort estimation ------

	//in order to have thousand's separators for printed numbers
	std::cout.imbue(std::locale(""));

	// first collect all the tasks
	std::vector<core::NodeAddress> parallelCalls = analysis::features::getParallelTasks(core::NodeAddress(program));

	if(parallelCalls.empty()) {
		std::cerr << "Couldn't find any task to estimate effort for..." << std::endl;
		return 1;
	}

	// the function we are looking for
	auto ancestor = core::analysis::getLowestCommonAncestor(parallelCalls);
	const core::LambdaExprAddress fun = core::analysis::getLowestUserDefinedFunctionAbove(ancestor);

	if(!fun) {
		std::cerr << "Couldn't find function to estimate effort for..." << std::endl;
		return 1;
	}
	std::cout << "Found function to estimate effort for with name: " << fun->getReference()->getNameAsString() << std::endl;

	// identify how the tasks are called (i.e. recursively or in a loop)
	std::cout << "Checking for parallels in loops..." << std::endl;
	for(const auto& call : parallelCalls) {
		if(core::analysis::hasLoopInBetween(fun, call)) {
			std::cout << "  found parallel within a loop" << std::endl;
		}
	}

	std::vector<std::pair<core::NodeAddress, core::NodeAddress>> results = analysis::features::getRecursiveTasks(fun, parallelCalls);
	std::cout << "Checking for recursions..." << std::endl;
	for(const auto& task : results) {
		std::cout << "  Recursive call in " << *task.second << " from " << task.second << " to " << task.first << std::endl;
		if(core::analysis::hasLoopInBetween(fun, task.second)) {
			std::cout << "    with a loop in between" << std::endl;
		}
	}

	// Estimate maximum stack size
	std::cout << "Maximum stack size: " << analysis::features::estimateStackSize(fun).second << " bytes" << std::endl;

	// Estimate effort
	std::cout << "Estimated Effort: " << analysis::features::estimateEffort(fun) << std::endl;
	return 0;
}
