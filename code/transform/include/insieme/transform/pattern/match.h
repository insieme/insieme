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

	/**
	 * A path is used to address a sub-set of the data represented within a match value.
	 * Since a match value is the balanced, recursive composition of values, each value
	 * can be addressed by its path within this tree.
	 *
	 * The Match Path can also be used as a 2-dimensional iterator (it is used like this
	 * within the pattern matcher implementation). You can visit siblings on the same level
	 * using the inc() and dec() operator, navigate to a child node using push() and to the
	 * parent element using pop().
	 */
	class MatchPath : public utils::Printable {

	public:

		/**
		 * The iterator type which can be used to iterate over components of this path.
		 */
		typedef vector<std::size_t>::const_iterator iterator;

	private:

		/**
		 * The internal storage of the represented path.
		 */
		vector<std::size_t> path;

	public:

		/**
		 * Creates a default, empty path.
		 */
		MatchPath() {}

		/**
		 * Descents to the given child node of the current node.
		 *
		 * @param index the index of the child node to be descending to
		 */
		void push(std::size_t index = 0) {
			path.push_back(index);
		}

		/**
		 * Moves to the parent of the currently addressed element.
		 */
		void pop() {
			path.pop_back();
		}

		/**
		 * Moves to the next element on the same layer.
		 */
		void inc() {
			path.back()++;
		}

		/**
		 * Moves to the previous element on the same layer. The current
		 * index must not be 0.
		 */
		void dec() {
			assert(path.back() > 0);
			path.back()--;
		}

		/**
		 * Obtain the current index this path is pointing to (inner most).
		 */
		std::size_t get() const {
			return path.back();
		}

		/**
		 * Updates the current index this path is pointing to.
		 */
		void set(std::size_t index) {
			assert(path.size() > 0);
			assert(path.back() < index);
			path.back() = index;
		}

		/**
		 * Obtains an iterator pointing to the root node of this path.
		 */
		iterator begin() const {
			return path.begin();
		}

		/**
		 * Obtains an iterator pointing to the position after the last component
		 * of this path.
		 */
		iterator end() const {
			return path.end();
		}

		/**
		 * Obtains the depth of this path - hence the number of steps between the root
		 * node and the addressed node.
		 */
		std::size_t getDepth() const {
			return path.size();
		}

		/**
		 * Prints a string-representation of this path to the given output stream.
		 */
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

		MatchValue(const list_type& list)
			: children(::transform(list, [](const value_type& cur) { return MatchValue<T>(cur); })), depth(1) {}

		MatchValue(const vector<MatchValue<T>>& children) : children(children), depth(children[0].getDepth()+1) {
			assert(all(children, [this](const MatchValue<T>& cur)->bool { return cur.getDepth() == this->depth-1; })
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
		const value_type& getValue() const {
			assert(depth == 0 && "Only works on level 0!");
			return tree;
		}


		// only supported for depth = 1
		list_type getList() const {
			// static const auto extractor = [](const MatchValue& value) { return value.getValue(); };

			assert(depth == 1 && "Only works on level 1!");
			list_type res;
			std::for_each( children.begin(), children.end(), [&] (const MatchValue& value) { 
					auto&& match = value.getValue();
					if (match) { res.push_back(match); } 
				} );
			return res;
		}

		operator const value_type&() {
			return getValue();
		}

		operator list_type() {
			return getList();
		}


		bool hasValue(const MatchPath& path) const {
			return path.getDepth() >= depth && hasValue(path.begin(), path.end());
		}

		const value_type& getValue(const MatchPath& path) const {
			assert(path.getDepth() >= depth && "Path not matching value type!");
			return getValue(path.begin(), path.end());
		}

		void addValue(const MatchPath& path, const value_type& value) {
			assert(path.getDepth() == depth && "Path not matching value type!");
			if (depth == 0) {
				tree = value;
			} else {
				addValue(path.begin(), path.end(), value);
			}
		}


		bool hasListValue(const MatchPath& path) const {
			return path.getDepth()+1 >= depth && hasListValue(path.begin(), path.end());
		}

		list_type getListValue(const MatchPath& path) const {
			assert(path.getDepth() + 1 >= depth && "Path not matching value type!");
			return getListValue(path.begin(), path.end());
		}

		void addListValue(const MatchPath& path, const list_type& list) { addListValue(path, list.begin(), list.end()); }

		void addListValue(const MatchPath& path, const list_iterator& begin, const list_iterator& end) {
			assert(path.getDepth()+1 == depth && "Path not matching value type!");
			static const auto constructor = [](const value_type& cur){ return MatchValue<T>(cur); };

			if (depth == 1) {
				assert(children.empty() && "Not allowed to override existing data!");
				std::transform(begin, end, std::back_inserter(children), constructor);
			} else {
				addListValue(path.begin(), path.end(), begin, end);
			}
		}

		virtual std::ostream& printTo(std::ostream& out) const {
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

	private:

		bool hasValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
			assert(begin <= end && "Path was too short!");
			if (depth == 0) return (tree)?true:false;

			auto index = *begin;
			return index < children.size() && children[index].hasValue(begin+1, end);
		}

		bool hasListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const{
			assert(begin <= end && "Path was too short!");
			if (depth == 1) return true;

			auto index = *begin;
			return index < children.size() && children[index].hasListValue(begin+1, end);
		}


		const value_type& getValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
			// always: begin <= end ... ensured by guard
			assert(begin <= end && "Path was too short!");

			if (depth == 0) {
				return getValue();
			}
			auto index = *begin;
			assert(index < children.size() && "Index out of range!");
			return children[index].getValue(begin+1, end);
		}


		list_type getListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end) const {
			// always: begin < end ... ensured by guard
			assert(begin <= end && "Path was too short!");

			if (depth == 1) {
				return getList();
			}
			auto index = *begin;
			assert(index < children.size() && "Index out of range!");
			return children[index].getListValue(begin+1, end);
		}


		void addValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const value_type& value) {

			// pick or create inner node
			auto index = *begin;
			if (index >= children.size()) {
				children.resize(index+1, MatchValue(depth-1));
			}

			if (begin+1 == end) {
				assert(depth == 1 && "Path length not correct!");
				assert(!children[index].tree && "Value must not be set!");
				children[index].tree = value;
			} else {
				children[index].addValue(begin+1, end, value);
			}
		}

		void addListValue(const MatchPath::iterator& begin, const MatchPath::iterator& end, const list_iterator& left, const list_iterator& right) {
			static const auto constructor = [](const value_type& cur){ return MatchValue<T>(cur); };

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
			map.insert(std::make_pair(var, value));
		}

		bool isTreeVarBound(const MatchPath& path, const std::string& var) const {
			auto pos = map.find(var);
			return pos != map.end() && pos->second.hasValue(path);
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
			pos->second.addValue(path, match);
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
			return map.find(var)->second.getValue(path);
		}

		list_type getListVarBinding(const MatchPath& path, const std::string& var) const {
			assert(isListVarBound(path, var) && "Requesting bound value for unbound list variable");
			// auto pos = map.find(var);
			return map.find(var)->second.getListValue(path);
		}

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "Match(" << map << ")";
		}
	};

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
