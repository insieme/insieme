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

#include "insieme/analysis/cba/datalog/facts_to_files_extractor.h"

#include <iostream>
#include <sstream>

#include "insieme/analysis/cba/datalog/framework/file_extractor.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	bool extractPointerFactsToFiles(const TargetRelations &targets, const string &edCmds, const std::string &outputDir, bool debug, std::ostream &debugOut)
	{
		if (targets.size() == 0) {
			std::cerr << "Need at least one target node...not able to deduce root now. Aborting...." << std::endl;
			return -1;
		}

		std::map<string,std::set<const TargetRelationEntry*>> targetFacts;
		int ret;

		auto isTarget = [&](const std::string &relation, const core::NodeAddress &x) -> const TargetRelationEntry* {
			for (const auto &trg : targets.at(relation))
				if (trg == x) return &trg;
			return nullptr;
		};

		const auto &root = ((targets.begin())->second.begin())->getRootAddress();

		ret = framework::extractAddressFactsToFiles(outputDir, root, [&](const core::NodeAddress &curr, int id) {
			for (const auto &trgPair : targets) {
				const string &relation = trgPair.first;
				if (const auto *entry = isTarget(relation, curr)) {
					entry->setID(id);
					targetFacts[relation].insert(entry);
				}
			}
		}, debug, debugOut);

		if (ret == -1)
			return false;

		for (const auto &trgPair : targetFacts) {
			const string &relation = trgPair.first;
			const auto &entries = trgPair.second;

			// Build output lines from target relation entries
			std::set<std::string> values;
			for (const auto &e : entries) {
				std::stringstream ss;
				ss << e->getID() << "\t";
				for (const auto &info : e->getInfo())
					ss << info.second << "\t";
				ss << e->getNode();
				values.insert(ss.str());
			}

			ret = framework::addFactsManually(outputDir, relation, values, debug, debugOut);
			if (ret == -1)
				return false;
		}

		if (edCmds.size() > 0) {
			ret = framework::addFactsManually(outputDir, "__ED_CMDS", {edCmds}, debug, debugOut);
			if (ret == -1)
				return false;
		}

		return true;
	}

	#undef convertValue

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
