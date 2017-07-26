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

#pragma once

#include <map>
#include <set>
#include <string>
#include <ostream>

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {


	struct TargetRelationEntry {
		using Key = std::string;
		using Value = std::string;
		using AdditionalInfo = std::pair<Key,Value>;

		TargetRelationEntry(core::NodeAddress node)
		        : id(-1), node(node) {}

		void setID(const int id) const {
			this->id = id;
		}

		int getID() const {
			return id;
		}

		TargetRelationEntry &addInfo(const Key key, const Value value) {
			auto entry = std::make_pair(key, value);
			info.push_back(entry);
			return *this;
		}

		TargetRelationEntry &addInfo(const Key &key, const int &value) {
			return addInfo(key, std::to_string(value));
		}

		const std::vector<AdditionalInfo> &getInfo() const {
			return info;
		}

		const core::NodeAddress &getNode() const {
			return node;
		}

		core::NodePtr getRootAddress() const {
			return node.getRootAddress();
		}

		bool operator ==(const core::NodeAddress &otherNode) const {
			return node == otherNode;
		}

		bool operator <(const TargetRelationEntry &other) const {
			return this->node < other.node;
		}

	private:
		mutable int id;
		const core::NodeAddress node;
		std::vector<AdditionalInfo> info;
	};

	using TargetRelations = std::map<string,std::set<TargetRelationEntry>>;

	/**
	 * Extract the facts (including 'targets') to fact files (to be used with external Soufflé)
	 * @param targets Target nodes from which we want the values (mapped to the fact file to write them in)
	 * @param edCmds some 'ed' commands that will be printed in a special file. Useful for post-processing scripts to alter generated files
	 * @param outputDir Directory where fact files are created
	 * @return true if successful
	 */
	bool extractPointerFactsToFiles(const TargetRelations &targets, const string &edCmds = "", const std::string &outputDir = "/tmp/facts", bool debug = false, std::ostream &debugOut = std::cout);

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
