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

#include <vector>

#include "boost/algorithm/string/replace.hpp"

#include "insieme/core/ir_visitor.h"
#include "insieme/utils/character_escaping.h"

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

		// IR dump
		out << "{\n\t";

		// mark root note
		out << "\"root\" : \"" << ir.ptr << "\",\n\t";

		// and the rest
		out << join("\t",nodes,[](std::ostream& out, const NodePtr& cur) {
			out << "\"" << cur.ptr << "\" : {\n";
				out << "\t\t\"Kind\" : \"" << cur->getNodeType() << "\",\n";
				if(auto val = cur.isa<ValuePtr>()) {
					std::string value = toString(val->getValue());
					boost::replace_all(value, "'", "`");
					out << "\t\t\"Value\" : \"" << utils::escapeString(value) << "\",\n";
				}
				out << "\t\t\"Children\" : [" << join(",",cur->getChildList(), [](std::ostream& out, const NodePtr& child){
					out << "\"" << child.ptr << "\"";
				}) << "]\n";
			out << "\t},\n";
		});

		// Annotation dump:
		out << "\"annotations\": {";
		bool first = true;
		visitDepthFirst(NodeAddress(ir),[&](const NodeAddress& cur) {
			// read info
			auto annotation = infoAnnotator(cur);
			if (annotation.empty()) return;

			// add information
			if (!first) { out << ","; } else { first = false; }
			out << "\n\t\"" << cur << "\" : \"" << annotation << "\"";

		},true,true);
		out << "\n\t}\n";

		out << "}\n";
	}

} // end namespace binary

} // end namespace dump
} // end namespace core
} // end namespace insieme
