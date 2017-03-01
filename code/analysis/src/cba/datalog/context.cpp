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
#include "insieme/analysis/cba/datalog/context.h"

#include "souffle/SouffleInterface.h"

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"
#include "insieme/analysis/cba/datalog/framework/toolbox.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	void Context::entry::clear() {
		delete analysis;
	}


	void Context::runAnalysis(souffle::SouffleProgram& analysis, const core::NodePtr& root, bool debug) {

		auto& nodeIndex = nodeIndexes[root];
		auto& idIndex = idIndexes[root];
		nodeIndex.clear();
		idIndex.clear();

		// export facts
		framework::extractAddressFacts(analysis, root, [&](const core::NodeAddress& addr, int id) {
			nodeIndex[addr] = id;
			idIndex[id] = addr;
		}, debug);

		if (debug) {
			std::cout << "-------- Input relations: --------" << std::endl;
			analysis.dumpInputs();
		}

		// run the analysis
		analysis.run();

		if (debug) {
			std::cout << std::endl << "-------- Output relations: --------" << std::endl;
			analysis.dumpOutputs();
		}

		// check for failures in analysis
		framework::checkForFailures(analysis);
	}

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
