/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/transform/pattern/match.h"

#include <iostream>

namespace insieme {
namespace transform {
namespace pattern {

	const TreePtr& MatchValue::getTree() const {
		assert(depth == 0 && "Only works on level 0!");
		return tree;
	}

	TreeList MatchValue::getTreeList() const {
		static auto extractor = [](const MatchValue& value) { return value.getTree(); };

		assert(depth == 1 && "Only works on level 1!");
		TreeList res;
		::transform(children, std::back_inserter(res), extractor);
		return res;
	}

	bool MatchValue::hasTreeValue(const MatchPath& path) const {
		return path.getDepth() >= depth && hasTreeValue(path.begin(), path.end());
	}

	bool MatchValue::hasTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
		assert(begin <= end && "Path was too short!");
		if (depth == 0) {
			return (tree)?true:false;
		}

		auto index = *begin;
		return index < children.size() && children[index].hasTreeValue(begin+1, end);
	}

	bool MatchValue::hasListValue(const MatchPath& path) const {
		return path.getDepth()+1 >= depth && hasListValue(path.begin(), path.end());
	}

	bool MatchValue::hasListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
		assert(begin <= end && "Path was too short!");
		if (depth == 1) {
			return true;
		}

		auto index = *begin;
		return index < children.size() && children[index].hasListValue(begin+1, end);
	}


	const TreePtr& MatchValue::getTreeValue(const MatchPath& path) const {
		assert(path.getDepth() >= depth && "Path not matching value type!");
		return getTreeValue(path.begin(), path.end());
	}

	const TreePtr& MatchValue::getTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
		// always: begin <= end ... ensured by guard
		assert(begin <= end && "Path was too short!");

		if (depth == 0) {
			return getTree();
		}
		auto index = *begin;
		assert(index < children.size() && "Index out of range!");
		return children[index].getTreeValue(begin+1, end);
	}


	TreeList MatchValue::getListValue(const MatchPath& path) const {
		assert(path.getDepth() + 1 >= depth && "Path not matching value type!");
		return getListValue(path.begin(), path.end());
	}

	TreeList MatchValue::getListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
		// always: begin < end ... ensured by guard
		assert(begin <= end && "Path was too short!");

		if (depth == 1) {
			return getTreeList();
		}
		auto index = *begin;
		assert(index < children.size() && "Index out of range!");
		return children[index].getListValue(begin+1, end);
	}

	void MatchValue::addTreeValue(const MatchPath& path, const TreePtr& value) {
		assert(path.getDepth() == depth && "Path not matching value type!");
		if (depth == 0) {
			tree = value;
		} else {
			addTreeValue(path.begin(), path.end(), value);
		}
	}

	void MatchValue::addTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const TreePtr& value) {

		// pick or create inner node
		auto index = *begin;
		if (index >= children.size()) {
			children.resize(index+1, MatchValue(depth-1));
		}

		if (begin+1 == end) {
			assert(depth == 1 && "Path length not correct!");
			children[index] = MatchValue(value);
		} else {
			children[index].addTreeValue(begin+1, end, value);
		}
	}


	void MatchValue::addListValue(const MatchPath& path, const TreeListIterator& begin, const TreeListIterator& end) {
		assert(path.getDepth()+1 == depth && "Path not matching value type!");
		static const auto constructor = [](const TreePtr& cur){ return MatchValue(cur); };

		if (depth == 1) {
			assert(children.empty() && "Not allowed to override existing data!");
			std::transform(begin, end, std::back_inserter(children), constructor);
		} else {
			addListValue(path.begin(), path.end(), begin, end);
		}
	}


	void MatchValue::addListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const TreeListIterator& left, const TreeListIterator& right) {
		static const auto constructor = [](const TreePtr& cur){ return MatchValue(cur); };

		// check for terminal condition
		if (begin == end) {
			assert(depth == 1 && "Path length not correct!");
			assert(children.empty() && "Not allowed to override existing data!");
			std::transform(left, right, std::back_inserter(children), constructor);
			return;
		}

		// pick or create inner node
		auto index = *begin;
		if (index >= children.size()) {
			children.resize(index+1, MatchValue(depth-1));
		}
		children[index].addListValue(begin+1, end, left, right);
	}

	std::ostream& MatchValue::printTo(std::ostream& out) const {
		// check for terminal case
		if (depth == 0) {
			if (tree) {
				return out << tree;
			}
			return out << "null";
		}
		// print rest recursively
		return out << children;
	}

	std::ostream& Match::printTo(std::ostream& out) const {
		return out << "Match(" << map << ")";
	}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
