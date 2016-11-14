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

#include "insieme/analysis/cba/datalog/code_properties.h"

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"

#include "insieme/core/ir_address.h"

#include "souffle/gen/polymorph_types_analysis.h"
#include "souffle/gen/top_level_term.h"
#include "souffle/gen/exit_point_analysis.h"

#include "souffle/gen/definition_point.h"
#include "souffle/gen/happens_before_analysis.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	/**
	 * Determines whether the given type is a polymorph type.
	 */
	bool isPolymorph(Context& context, const core::TypePtr& type, bool debug)
	{
		// Instantiate analysis
		auto &analysis = context.getAnalysis<souffle::Sf_polymorph_types_analysis>(type, debug);

		// Get target node ID for which to check -> the root node
		int targetID = 0;

		// Get result
		auto &resultRel = analysis.rel_result;
		auto resultRelContents = resultRel.template equalRange<0>({{targetID}});

		// If result is non-empty, return true
		return resultRelContents.begin() != resultRelContents.end();
	}

	/**
	 * Determine top level nodes
	 */
	bool getTopLevelNodes(Context& context, const core::NodePtr& root, bool debug)
	{
		// Instantiate analysis
		auto &analysis = context.getAnalysis<souffle::Sf_top_level_term>(root, debug);

		// Get target node ID for which to check -> the root node
		int targetID = 0;

		// Get result
		auto &resultRel = analysis.rel_TopLevel;

		return resultRel.size() != 0 && resultRel.contains(targetID);
	}

	/**
	 * Get exit points from a given lambda function
	 */
	std::set<core::ReturnStmtAddress> performExitPointAnalysis(Context& context, const core::LambdaAddress& lambda, bool debug)
	{
		// Instantiate analysis
		auto &analysis = context.getAnalysis<souffle::Sf_exit_point_analysis>(lambda.getRootNode(), debug);

		// Get ID for given Lambda
		int targetLambdaID = context.getNodeID(lambda);

		// Get result
		auto &resultRel = analysis.rel_ExitPoints;

		// Now map the exit point IDs from root Lambda to actual ReturnStmts
		std::set<core::ReturnStmtAddress> res;

		for (const auto &cur : resultRel) {
			int lambdaID = cur[0];
			int returnStmtID = cur[1];

			if (lambdaID != targetLambdaID) {
				continue;
			}

			auto rsa = context.getNodeForID(lambda.getRootNode(), returnStmtID, debug).as<core::ReturnStmtAddress>();

			// Crop addresses to start at given lambda
			rsa = core::cropRootNode(rsa, lambda);

			res.insert(rsa);
		}

		return res;
	}


	core::VariableAddress getDefinitionPoint(Context& context, const core::VariableAddress& var, bool debug)
	{
		// Instantiate analysis
		auto &analysis = context.getAnalysis<souffle::Sf_definition_point>(var.getRootNode(), debug);

		// Get ID for variable we are interested in
		int targetVariableID = context.getNodeID(var);

		// Get result
		auto &resultRel = analysis.rel_Result;

		auto defPoint = resultRel.template equalRange<0>({{targetVariableID,0}});

		if (defPoint.empty()) {
			if (debug)
				std::cout << "Debug: Could not find definition point for variable " << var << "...";
			return {};
		}

		auto defPointID = (*defPoint.begin())[1];

		assert_true(++defPoint.begin() == defPoint.end())
		                << "Analysis failed: Multiple definition points found for var " << var << "!";

		return context.getNodeForID(var.getRootNode(), defPointID, debug).as<core::VariableAddress>();
	}

	bool happensBefore(Context &context, const core::StatementAddress& a, const core::StatementAddress& b) {
		static const bool debug = false;
		assert_eq(a.getRootNode(), b.getRootNode());

		// instantiate the analysis
		souffle::Sf_happens_before_analysis analysis;

		int startID = 0;
		int endID = 0;

		// fill in facts
		framework::extractAddressFacts(analysis, a.getRootNode(), [&](const core::NodeAddress& addr, int id) {
			if (addr == a) startID = id;
			if (addr == b) endID = id;
		});

		// add start and end
		analysis.rel_start.insert(startID);
		analysis.rel_end.insert(endID);

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// read result
		return !analysis.rel_result.empty();
	}

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
