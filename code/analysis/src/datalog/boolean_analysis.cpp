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

#include "insieme/analysis/datalog/boolean_analysis.h"

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include "souffle/gen/boolean_analysis.h"

namespace insieme {
namespace analysis {
namespace datalog {

	enum Result {
		TRUE,
		FALSE,
		TRUE_OR_FALSE
	};

	Result getBoolValue(const core::ExpressionAddress& expr) {
		const bool debug = false;

		// instantiate the analysis
		souffle::Sf_boolean_analysis analysis;

		int targetID = 0;

		// fill in facts
		framework::extractAddressFacts(analysis, expr.getRootNode(), [&](const core::NodeAddress& addr, int id) {
			if (addr == expr) targetID = id;
		});

		// add targeted node
		analysis.rel_target_expr.insert(targetID);

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// Check for failures in analysis and print them
		auto failures = analysis.getRelation("D474L06_utils_failure_dl_failure");
		if (failures != nullptr && failures->size() > 0) {
			for(auto it = failures->begin(); it != failures->end(); ++it) {
				std::string msg;
				int target_rel;
				*it >> msg >> target_rel;
				std::cerr << msg << " at relation " << target_rel << "." << std::endl;
			}
		}
		assert_eq(0, failures->size()) << "Failures in analysis detected, aborting...";

		// read result
		auto& result = analysis.rel_result;
		assert_le(1, result.size()) << "Incomplete analysis!";

		// if it is true or false => we don't know
		if (result.size() != 1) return TRUE_OR_FALSE;

		// read the single entry in the result set
		auto value = (*result.begin())[0];
		return (value) ? TRUE : FALSE;
	}


	bool isTrue(const core::ExpressionAddress& expr) {
		return getBoolValue(expr) == TRUE;
	}

	bool isFalse(const core::ExpressionAddress& expr) {
		return getBoolValue(expr) == FALSE;
	}

	bool mayBeTrue(const core::ExpressionAddress& expr) {
		return getBoolValue(expr) != FALSE;
	}

	bool mayBeFalse(const core::ExpressionAddress& expr) {
		return getBoolValue(expr) != TRUE;
	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
