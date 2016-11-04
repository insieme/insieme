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

//#include "insieme/analysis/cba/datalog/framework/analysis_base.h"

#include <iostream>

#include <souffle/SouffleInterface.h>

#include "insieme/analysis/cba/datalog/framework/analysis_base.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	class Inserter {
		souffle::Program *analysis;

	public:
		Inserter() {}

		void setAnalysis(souffle::Program &analysis) {
			this->analysis = &analysis;
		}

		void fill(souffle::tuple&) {}

		template<typename F, typename ... Rest>
		void fill(souffle::tuple& tuple, const F& first, const Rest& ... rest) {
			tuple << first;
			fill(tuple,rest...);
		}

		void print() {
			std::cout << std::endl;
		}

		template <typename F, typename ...Rest>
		void print(const F& first, const Rest& ... rest) {
			std::cout << " - " << first;
			print(rest...);
		}

		template<typename ... Args>
		void insert(const std::string& relationName, const Args& ... args ) {
			// get relation
			auto rel = analysis->getRelation(relationName);
			// std::cout << "Inserting " << relationName; print(args...);
			if (!rel) return;

			// insert data
			souffle::tuple tuple(rel);
			fill(tuple, args...);
			rel->insert(tuple);
		}

	};

	int extractFacts(souffle::Program& analysis, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer) {
		FactExtractor<core::Pointer,Inserter> extractor(nodeIndexer);
		extractor.getInserter().setAnalysis(analysis);
		return extractor.visit(root);
	}

	int extractAddressFacts(souffle::Program& analysis, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer) {
		FactExtractor<core::Address,Inserter> extractor(nodeIndexer);
		extractor.getInserter().setAnalysis(analysis);
		return extractor.visit(core::NodeAddress(root));
	}

} // end namespace framework
} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
