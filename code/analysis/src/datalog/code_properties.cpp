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

#include "insieme/analysis/datalog/code_properties.h"

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include "souffle/gen/polymorph_types_analysis.h"
#include "souffle/gen/top_level_term.h"
#include "souffle/gen/exit_point_analysis.h"

namespace insieme {
namespace analysis {
namespace datalog {


	/**
	 * Determines whether the given type is a polymorph type.
	 */
	bool isPolymorph(const core::TypePtr& type, bool debug) {

		// instantiate the analysis
		souffle::Sf_polymorph_types_analysis analysis;

		// fill in facts
		int rootNodeID = framework::extractFacts(analysis, type);

		// add start node
		analysis.rel_rootNode.insert(rootNodeID);

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// read result
		auto& rel = analysis.rel_result;
		return rel.size() > 0;
	}

	/**
	 * Determine top level nodes
	 */
	bool getTopLevelNodes(const core::NodePtr& root, bool debug)
	{
		// instantiate the analysis
		souffle::Sf_top_level_term analysis;

		// fill in facts
		framework::extractFacts(analysis, root);

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// read result
		auto& rel = analysis.rel_TopLevel;
		bool result = rel.size() && rel.contains(1);
		if (!result) analysis.dumpOutputs();
		return result;
	}

	/**
	 * Get exit points from a given lambda function
	 */
	bool performExitPointAnalysis(const core::NodePtr& rootLambda, bool debug)
	{
		// instantiate the analysis
		souffle::Sf_exit_point_analysis analysis;

		// fill in facts
		framework::extractFacts(analysis, rootLambda);

		// Add the lambda which we are interested in as 'top level'
		analysis.rel_TopLevelLambda.insert(rootLambda);

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// read result
		auto& rel = analysis.rel_ExitPoints;
		bool result = rel.size() && rel.contains(1);
		if (!result) analysis.dumpOutputs();
		return result;
	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
