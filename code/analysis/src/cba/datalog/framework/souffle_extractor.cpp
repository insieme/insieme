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
 */

#include <iostream>

#include <souffle/SouffleInterface.h>

#include "insieme/analysis/cba/datalog/framework/analysis_base.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	namespace {

		class Inserter {
			souffle::SouffleProgram *analysis;
			bool debug = false;

		public:
			Inserter() {}

			void setAnalysis(souffle::SouffleProgram &analysis) {
				this->analysis = &analysis;
			}

			void setDebug(bool debug) {
				this->debug = debug;
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

				if (debug) {
					std::cout << "Inserting " << relationName << "  ";
					print(args...);
				}

				// get relation
				auto rel = analysis->getRelation(relationName);
				if (!rel) return;

				// insert data
				souffle::tuple tuple(rel);
				fill(tuple, args...);
				rel->insert(tuple);
			}

		};

	} // end anonymous namespace

	int extractFacts(souffle::SouffleProgram& analysis, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer, bool debug) {
		FactExtractor<core::Pointer,Inserter> extractor(nodeIndexer);
		extractor.getInserter().setAnalysis(analysis);
		extractor.getInserter().setDebug(debug);
		return extractor.visit(root);
	}

	int extractAddressFacts(souffle::SouffleProgram& analysis, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer, bool debug) {
		FactExtractor<core::Address,Inserter> extractor(nodeIndexer);
		extractor.getInserter().setAnalysis(analysis);
		extractor.getInserter().setDebug(debug);
		return extractor.visit(core::NodeAddress(root));
	}

} // end namespace framework
} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
