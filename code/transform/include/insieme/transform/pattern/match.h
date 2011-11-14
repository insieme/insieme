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

#pragma once

#include <boost/optional.hpp>

#include "insieme/transform/pattern/structure.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace transform {
namespace pattern {

	using std::vector;


	class MatchPath : public utils::Printable {

	public:

		typedef vector<std::size_t>::const_iterator iterator;

	private:

		vector<std::size_t> path;

	public:

		MatchPath() {}

		void push(std::size_t index = 0) {
			path.push_back(index);
		}

		void pop() {
			path.pop_back();
		}

		void inc() {
			path.back()++;
		}

		void dec() {
			assert(path.back() > 0);
			path.back()--;
		}

		iterator begin() const {
			return path.begin();
		}

		iterator end() const {
			return path.end();
		}

		std::size_t getDepth() const {
			return path.size();
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << path;
		}
	};


	class MatchValue : public utils::Printable {

		/**
		 * The tree this value is representing - if the match is on level 0.
		 */
		TreePtr tree;

		/**
		 * The child nodes of this value if this value has depth > 0.
		 */
		vector<MatchValue> children;

		unsigned depth;

	public:

		MatchValue(unsigned depth) : depth(depth) { }

		MatchValue(const TreePtr& tree) : tree(tree), depth(0) {}

		MatchValue(const TreeList& list) : children(::transform(list, [](const TreePtr& cur) { return MatchValue(cur); })), depth(1) {}

		MatchValue(const vector<MatchValue>& children) : children(children), depth(children[0].getDepth()+1) {
			assert(all(children, [&](const MatchValue& cur) { return cur.getDepth() == depth-1; })
					&& "All children have to be of the same level!");
		}

		unsigned getDepth() const {
			return depth;
		}

		const vector<MatchValue>& getValues() const {
			assert(depth > 0 && "Cannot access child-values of leaf node!");
			return children;
		}

		const MatchValue& getValue(unsigned index) const {
			assert(index < children.size() && "Invalid index!");
			return getValues()[index];
		}

		// only supported for depth = 0
		const TreePtr& getTree() const;

		// only supported for depth = 1
		TreeList getTreeList() const;


		bool hasTreeValue(const MatchPath& path) const;

		const TreePtr& getTreeValue(const MatchPath& path) const;

		void addTreeValue(const MatchPath& path, const TreePtr& value);


		bool hasListValue(const MatchPath& path) const;

		TreeList getListValue(const MatchPath& path) const;

		void addListValue(const MatchPath& path, const TreeList& list) { addListValue(path, list.begin(), list.end()); }

		void addListValue(const MatchPath& path, const TreeListIterator& begin, const TreeListIterator& end);



		virtual std::ostream& printTo(std::ostream& out) const;

	private:

		bool hasTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;
		bool hasListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;

		const TreePtr& getTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;
		TreeList getListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;

		void addTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const TreePtr& value);
		void addListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const TreeListIterator& left, const TreeListIterator& right);

	};

	template<typename ... Matches>
	inline MatchValue makeMatchValue(Matches ... matches) {
		return MatchValue(toVector<MatchValue>(matches ...));
	}

	class Match : public utils::Printable {

	public:

		typedef std::unordered_map<string, MatchValue> ValueMap;

	private:

		TreePtr root;

		ValueMap map;

	public:

		TreePtr getRoot() const {
			return root;
		}

		void setRoot(const TreePtr& tree) {
			root = tree;
		}

		const ValueMap& getValueMap() const {
			return map;
		}

		bool isVarBound(const std::string& var) const {
			return map.find(var) != map.end();
		}

		const MatchValue& getVarBinding(const std::string& var) const {
			assert(isVarBound(var) && "Requesting unbound variable!");
			return map.find(var)->second;
		}

		void bindVar(const std::string& var, const MatchValue& value) {
			auto pos = map.find(var);
			if (pos != map.end()) {
				// update existing value
				pos->second = value;
				return;
			}
			// add new value
			map.insert(ValueMap::value_type(var, value));
		}

		bool isTreeVarBound(const MatchPath& path, const std::string& var) const {
			auto pos = map.find(var);
			return pos != map.end() && pos->second.hasTreeValue(path);
		}

		bool isListVarBound(const MatchPath& path, const std::string& var) const {
			auto pos = map.find(var);
			return pos != map.end() && pos->second.hasListValue(path);
		}

		void bindTreeVar(const MatchPath& path, const std::string& var, const TreePtr match) {
			assert(!isTreeVarBound(path, var) && "Variable bound twice");
			auto pos = map.find(var);
			if (pos == map.end()) {
				pos = map.insert(ValueMap::value_type(var, MatchValue(path.getDepth()))).first;
			}
			pos->second.addTreeValue(path, match);
		}

		void bindListVar(const MatchPath& path, const std::string& var, const TreeListIterator& begin, const TreeListIterator& end) {
			assert(!isListVarBound(path, var) && "Variable bound twice");
			auto pos = map.find(var);
			if (pos == map.end()) {
				pos = map.insert(ValueMap::value_type(var, MatchValue(path.getDepth()+1))).first;
			}
			pos->second.addListValue(path, begin, end);
		}

		TreePtr getTreeVarBinding(const MatchPath& path, const std::string& var) const {
			assert(isTreeVarBound(path, var) && "Requesting bound value for unbound tree variable");
			auto pos = map.find(var);
			return map.find(var)->second.getTreeValue(path);
		}

		TreeList getListVarBinding(const MatchPath& path, const std::string& var) const {
			assert(isListVarBound(path, var) && "Requesting bound value for unbound list variable");
			auto pos = map.find(var);
			return map.find(var)->second.getListValue(path);
		}

		virtual std::ostream& printTo(std::ostream& out) const;
	};

	typedef boost::optional<Match> MatchOpt;

	// a super type for value manipulating operations
	typedef std::function<MatchValue(const MatchValue&)> ValueOp;

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
