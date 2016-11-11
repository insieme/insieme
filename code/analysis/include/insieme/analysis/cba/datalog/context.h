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

#pragma once

#include <map>
#include <typeindex>

#include "insieme/analysis/cba/datalog/framework/forward_decls.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"


namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	/**
	 * The context for datalog analysis.
	 */
	struct Context {

		struct entry {
			core::NodePtr root;
			souffle::Program* analysis;
			std::map<core::ExpressionAddress,int> index;

			entry() : analysis(nullptr) {}
			~entry() { clear(); }

			void clear();
		};

		std::map<std::type_index,entry> cache;

		std::map<core::NodePtr,std::map<core::ExpressionAddress,int>> nodeIndexes;

		Context() {}

		Context(Context&&) = delete;
		Context(const Context&) = delete;

		Context& operator=(const Context&) = delete;
		Context& operator=(Context&&) = delete;

		// -- general context interface requirements --

		void dumpStatistics() const {
			// this feature is not yet implemented
			std::cout << "No statistic data available.\n";
		}

		void dumpSolution() const {
			// this feature is not yet implemented
			std::cout << "Sorry, but the solution dump is not yet implemented for the Datalog engine.\n";
		}

		template<typename Analysis>
		Analysis& getAnalysis(const core::NodePtr& root, bool debug = false) {
			auto& entry = cache[typeid(Analysis)];

			// Check whether this is a hit
			if (entry.analysis && *entry.root == *root) {

				if (debug)
					std::cout << "Cache hit, returning existing analysis!" << std::endl;

				return static_cast<Analysis&>(*entry.analysis);
			}

			// We have to produce a new analysis instance
			entry.clear();
			entry.analysis = new Analysis();
			entry.root = root;

			if (debug)
				std::cout << "Cache miss, running new analysis now";

			// Execute the analyis
			runAnalysis(*entry.analysis, root, debug);

			// Done
			return getAnalysis<Analysis>(root);
		}

		int getNodeID(const core::ExpressionAddress& expr) const {

			// Search for root node of this expr first
			auto pos = nodeIndexes.find(expr.getRootNode());
			assert_true(pos != nodeIndexes.end())
				<< "Trying to access ID of node not previously indexed!";

			// Now search for the expr itself
			auto res_pos = (pos->second).find(expr);
			assert_true(res_pos != pos->second.end())
				<< "Trying to access ID of node not previously indexed!";

			// Return the ID for expr given at traversal from it's root node
			return res_pos->second;
		}

	private:

		void runAnalysis(souffle::Program& analysis, const core::NodePtr& root, bool debug);

	};

} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
