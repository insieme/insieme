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

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/printable.h"

#include "insieme/transform/pattern/match_target.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

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

	template<typename T>
	class MatchValue : public utils::Printable {

		typedef typename T::value_type value_type;
		typedef typename T::list_type list_type;
		typedef typename T::list_iterator list_iterator;

		/**
		 * The tree this value is representing - if the match is on level 0.
		 */
		value_type tree;

		/**
		 * The child nodes of this value if this value has depth > 0.
		 */
		vector<MatchValue<T>> children;

		/**
		 * The depth of this value.
		 */
		unsigned depth;

	public:

		MatchValue(unsigned depth) : depth(depth) { }

		MatchValue(const value_type& tree) : tree(tree), depth(0) {}

		MatchValue(const list_type& list) : children(::transform(list, [](const value_type& cur) { return MatchValue<T>(cur); })), depth(1) {}

		MatchValue(const vector<MatchValue<T>>& children) : children(children), depth(children[0].getDepth()+1) {
			assert(all(children, [depth](const MatchValue<T>& cur)->bool { return cur.getDepth() == depth-1; })
					&& "All children have to be of the same level!");
		}

		unsigned getDepth() const {
			return depth;
		}

		const vector<MatchValue<T>>& getValues() const {
			assert(depth > 0 && "Cannot access child-values of leaf node!");
			return children;
		}

		const MatchValue<T>& getValue(unsigned index) const {
			assert(index < children.size() && "Invalid index!");
			return getValues()[index];
		}

		// only supported for depth = 0
		const value_type& getTree() const;

		// only supported for depth = 1
		list_type getTreeList() const;


		bool hasTreeValue(const MatchPath& path) const;

		const value_type& getTreeValue(const MatchPath& path) const;

		void addTreeValue(const MatchPath& path, const value_type& value);


		bool hasListValue(const MatchPath& path) const;

		list_type getListValue(const MatchPath& path) const;

		void addListValue(const MatchPath& path, const list_type& list) { addListValue(path, list.begin(), list.end()); }

		void addListValue(const MatchPath& path, const list_iterator& begin, const list_iterator& end);

		virtual std::ostream& printTo(std::ostream& out) const;

	private:

		bool hasTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;
		bool hasListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;

		const value_type& getTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;
		list_type getListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const;

		void addTreeValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const value_type& value);
		void addListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const list_iterator& left, const list_iterator& right);

	};

	template<typename T, typename ... Matches>
	inline MatchValue<T> makeMatchValue(Matches ... matches) {
		return MatchValue<T>(toVector<MatchValue<T>>(matches ...));
	}

	template<typename T>
	class Match : public utils::Printable {

		typedef typename T::value_type value_type;
		typedef typename T::list_type list_type;
		typedef typename T::list_iterator list_iterator;

	public:

		typedef std::unordered_map<string, MatchValue<T>> ValueMap;

	private:

		value_type root;

		ValueMap map;

	public:

		const value_type& getRoot() const {
			return root;
		}

		void setRoot(const value_type& tree) {
			root = tree;
		}

		const ValueMap& getValueMap() const {
			return map;
		}

		bool isVarBound(const std::string& var) const {
			return map.find(var) != map.end();
		}

		const MatchValue<T>& getVarBinding(const std::string& var) const {
			assert(isVarBound(var) && "Requesting unbound variable!");
			return map.find(var)->second;
		}

		void bindVar(const std::string& var, const MatchValue<T>& value) {
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

		void bindTreeVar(const MatchPath& path, const std::string& var, const value_type& match) {
			assert(!isTreeVarBound(path, var) && "Variable bound twice");
			auto pos = map.find(var);
			if (pos == map.end()) {
				pos = map.insert(std::make_pair(var, MatchValue<T>(path.getDepth()))).first;
			}
			pos->second.addTreeValue(path, match);
		}

		void bindListVar(const MatchPath& path, const std::string& var, const list_iterator& begin, const list_iterator& end) {
			assert(!isListVarBound(path, var) && "Variable bound twice");
			auto pos = map.find(var);
			if (pos == map.end()) {
				pos = map.insert(std::make_pair(var, MatchValue<T>(path.getDepth()+1))).first;
			}
			pos->second.addListValue(path, begin, end);
		}

		const value_type& getTreeVarBinding(const MatchPath& path, const std::string& var) const {
			assert(isTreeVarBound(path, var) && "Requesting bound value for unbound tree variable");
			// auto pos = map.find(var);
			return map.find(var)->second.getTreeValue(path);
		}

		list_type getListVarBinding(const MatchPath& path, const std::string& var) const {
			assert(isListVarBound(path, var) && "Requesting bound value for unbound list variable");
			// auto pos = map.find(var);
			return map.find(var)->second.getListValue(path);
		}

		virtual std::ostream& printTo(std::ostream& out) const;
	};

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
