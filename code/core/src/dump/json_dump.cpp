/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/dump/json_dump.h"

#include <string>
#include <vector>

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

	void dumpIR(std::ostream& out, const NodePtr& ir, const std::function<std::string(NodeAddress)>& infoAnnotator) {
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

		// put annotations (if any)
		ptree annotations;
		visitDepthFirst(NodeAddress(ir), [&] (const NodeAddress& node) {
			// read info
			auto annotation = infoAnnotator(node);
			if(annotation.empty()) return;

			// add info
			ptree info;
			info.put<string>("", annotation);
			annotations.push_back(make_pair(toString(node), info));
		}, true, true);
		if(!annotations.empty()) dump.push_back(make_pair("annotations", annotations));

		write_json(out, dump);
	}

} // end namespace json
} // end namespace dump
} // end namespace core
} // end namespace insieme
