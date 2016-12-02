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

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"
#include "insieme/analysis/cba/datalog/framework/toolbox.h"

#include "souffle/gen/alias_analysis.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	using Locations = std::set<int>;
	using LocationMap = std::map<core::ExpressionAddress,Locations>;

	LocationMap getReferencedLocations(Context &context, const std::vector<core::ExpressionAddress>& exprs) {
		const bool debug = false;

		// Check whether there is something to do
		if (exprs.empty()) {
			return LocationMap();
		}

		// Instantiate the analysis
		auto &analysis = context.getAnalysis<souffle::Sf_alias_analysis>(exprs[0].getRootNode(), debug);

		// Get IDs of target expressions
		std::map<int,core::ExpressionAddress> index;

		for (const auto &expr : exprs) {
			auto id = context.getNodeID(expr, debug);
			index[id] = expr;
		}

		// Read result
		LocationMap res;
		auto& resultRelation = analysis.rel_result;

		for(const auto& cur : resultRelation) {
			auto &id = cur[0];
			auto &values = cur[1];

			if (index.find(id) == index.end())
				continue;

			res[index[id]].insert(values);
		}

		return res;
	}


	bool mayAlias(Context& context, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		auto data = getReferencedLocations(context, { a, b });
		/* Note: Removing the asserts because they may wrongly report failures if there's dead code */
		// assert_false(data[a].empty()) << "Failure in analysis!";
		// assert_false(data[b].empty()) << "Failure in analysis!";
		for(const auto& cur : data[a]) {
			if (contains(data[b], cur)) return true;
		}
		return false;
	}

	bool areAlias(Context& context, const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		auto data = getReferencedLocations(context, { a, b });
		/* Note: Removing the asserts because they may wrongly report failures if there's dead code */
		// assert_false(data[a].empty()) << "Failure in analysis!";
		// assert_false(data[b].empty()) << "Failure in analysis!";
		return data[a].size() == 1 && data[b].size() == 1 && data[a] == data[b];
	}

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
