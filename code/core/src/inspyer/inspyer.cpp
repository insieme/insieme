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

#include "insieme/core/inspyer/inspyer.h"

#include "insieme/core/dump/json_dump.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/string_utils.h"

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

using namespace boost::property_tree;
using namespace std;

namespace insieme {
namespace core {
namespace inspyer {

	MetaGenerator default_meta {nullptr};

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

	void addBookmark(const NodeAddress addr) {
		default_meta.addBookmark(addr);
	}

	void addExpand(const NodeAddress addr) {
		default_meta.addExpand(addr);
	}

	void addHighlight(const NodeAddress addr) {
		default_meta.addHighlight(addr);
	}

	void addLabel(const NodeAddress addr, const string label) {
		default_meta.addLabel(addr, label);
	}

	void addBody(const NodeAddress addr, const string body) {
		default_meta.addBody(addr, body);
	}

	void dumpTree(ostream& out, const NodePtr root) {
		dump::json::dumpIR(out, root);
	}

	void dumpMeta(ostream& out) {
		default_meta.dump(out);
	}

} // end namespace inspyer
} // end namespace core
} // end namespace insieme
