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
 *
 */
#include "insieme/core/dump/json_dump.h"

#include <string>
#include <vector>
#include <fstream>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#include "insieme/core/ir_visitor.h"
#include "insieme/utils/character_escaping.h"

using namespace std;
using namespace boost::property_tree;

namespace insieme {
namespace core {
namespace dump {
namespace json {


	void dumpIR(const std::string& filename, const NodePtr& ir) {
		std::ofstream out(filename);
		dumpIR(out, ir);
	}

	void dumpIR(std::ostream& out, const NodePtr& ir) {
		// create list of all nodes
		std::vector<NodePtr> nodes;
		visitDepthFirstOnce(ir,[&](const NodePtr& cur) {
			nodes.push_back(cur);
		},true,true);

		// put root node
		ptree root;
		root.put<string>("", toString(ir.ptr));

		// put node list
		ptree dump;
		dump.push_back(make_pair("root", root));
		for(auto& node : nodes) {
			ptree e;
			e.put<string>("Kind", toString(node->getNodeType()));

			if(auto val = node.isa<ValuePtr>()) e.put<string>("Value", toString(val->getValue()));

			ptree children;
			for(auto& child : node->getChildList()) {
				ptree e;
				e.put<string>("", toString(child.ptr));
				children.push_back(make_pair("", e));
			}
			if(!children.empty()) e.push_back(make_pair("Children", children));

			dump.push_back(make_pair(toString(node.ptr), e));
		}

		write_json(out, dump);
	}

} // end namespace json
} // end namespace dump
} // end namespace core
} // end namespace insieme
