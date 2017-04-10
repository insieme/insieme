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
#include "insieme/core/inspyer/inspyer.h"

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/string_utils.h"

using namespace boost::property_tree;
using namespace std;

namespace insieme {
namespace core {
namespace inspyer {

	MetaGenerator::MetaGenerator() = default;

	MetaGenerator::MetaGenerator(const NodePtr root) : root(root) {};

	void MetaGenerator::checkRoot(const NodePtr root) {
		if(!this->root) {
			this->root = root;
		} else {
			assert_eq(this->root, root) << "Root Node does not match";
		}
	}

	void MetaGenerator::addBookmark(const NodeAddress addr) {
		checkRoot(addr.getRootNode());
		bookmarks.insert(addr);
	}

	void MetaGenerator::addExpand(const NodeAddress addr) {
		checkRoot(addr.getRootNode());
		expands.insert(addr);
	}

	void MetaGenerator::addHighlight(const NodeAddress addr) {
		checkRoot(addr.getRootNode());
		highlights.insert(addr);
	}

	void MetaGenerator::addLabel(const NodeAddress addr, const string label) {
		checkRoot(addr.getRootNode());
		labels[addr] = label;
	}

	void MetaGenerator::addBody(const NodeAddress addr, const string body) {
		checkRoot(addr.getRootNode());
		bodies[addr] = body;
	}

	namespace detail {
		ptree set2ptree(const set<NodeAddress>& addrs) {
			ptree ret;
			for(auto& addr : addrs) {
				ptree e;
				e.put<string>("", toString(addr));
				ret.push_back(make_pair("", e));
			}
			return ret;
		}
		ptree map2ptree(const map<NodeAddress, string>& container) {
			ptree ret;
			for(auto& kv: container) {
				ret.put<string>(toString(kv.first), kv.second);
			}
			return ret;
		}
	}

	void MetaGenerator::dump(ostream& out) {
		ptree meta;
		meta.put_child("bookmarks", detail::set2ptree(bookmarks));
		meta.put_child("expands", detail::set2ptree(expands));
		meta.put_child("highlights", detail::set2ptree(highlights));
		meta.put_child("labels", detail::map2ptree(labels));
		meta.put_child("bodies", detail::map2ptree(bodies));
		write_json(out, meta);
	}

} // end namespace inspyer
} // end namespace core
} // end namespace insieme
