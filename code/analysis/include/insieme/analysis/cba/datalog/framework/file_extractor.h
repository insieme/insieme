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

#include <set>
#include <string>
#include <fstream>

#include "insieme/analysis/cba/datalog/framework/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	/**
	 * Extracts facts from the given root node and save them in fact files (so that they can be used as input for stand-alone Soufflé)
	 */
	int extractFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer = [](const core::NodePtr&,int){}, bool debug = false, std::ostream &debugOut = std::cout);

	/**
	 * Extracts facts from the given root node and save them in fact files (so that they can be used as input for stand-alone Soufflé)
	 */
	int extractAddressFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer = [](const core::NodeAddress&,int){}, bool debug = false, std::ostream &debugOut = std::cout);

	/**
	 * Add facts manually to fact file. Useful to add facts that cannot be extracted from the IR itself
	 */
	bool addFactsManually(const std::string &folder, const std::string &relationName, const std::set<std::string> &facts, bool debug = false, std::ostream &debugOut = std::cout);


} // end namespace framework
} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
