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

#include "insieme/analysis/cba/datalog/alias_analysis.h"

#include "insieme/analysis/cba/datalog/framework/analysis_base.h"

#include "souffle/gen/alias_analysis.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	using Locations = std::set<int>;
	using LocationMap = std::map<core::ExpressionAddress,Locations>;

	LocationMap getReferencedLocations(const std::vector<core::ExpressionAddress>& exprs) {
		const bool debug = false;

		// check whether there is something to do
		if (exprs.empty()) {
			return LocationMap();
		}

		// instantiate the analysis
		souffle::Sf_alias_analysis analysis;

		std::map<int,core::ExpressionAddress> index;

		// fill in facts
		framework::extractAddressFacts(analysis, exprs[0].getRootNode(), [&](const core::NodeAddress& addr, int id) {
			for(const auto& expr : exprs) {
				if (expr == addr) {
					// remember id
					index[id] = addr.as<core::ExpressionAddress>();

					// add targeted node
					analysis.rel_targets.insert(id);

					// done
					return;
				}
			}
		});

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// check for failures in analysis
		framework::checkForFailures(analysis);

		// read result
		LocationMap res;
		for(const auto& cur : analysis.rel_result) {
			res[index[cur[0]]].insert(cur[1]);
		}
		return res;
	}


	bool mayAlias(Context&, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		auto data = getReferencedLocations({ a, b });
		/* Note: Removing the asserts because they may wrongly report failures if there's dead code */
		// assert_false(data[a].empty()) << "Failure in analysis!";
		// assert_false(data[b].empty()) << "Failure in analysis!";
		for(const auto& cur : data[a]) {
			if (contains(data[b], cur)) return true;
		}
		return false;
	}

	bool areAlias(Context&, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		auto data = getReferencedLocations({ a, b });
		/* Note: Removing the asserts because they may wrongly report failures if there's dead code */
		// assert_false(data[a].empty()) << "Failure in analysis!";
		// assert_false(data[b].empty()) << "Failure in analysis!";
		return data[a].size() == 1 && data[b].size() == 1 && data[a] == data[b];
	}

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
