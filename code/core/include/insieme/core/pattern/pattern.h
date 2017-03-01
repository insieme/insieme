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
#pragma once

#include <string>
#include <memory>

#include <boost/optional.hpp>

#include "insieme/core/forward_decls.h"

#include "insieme/core/pattern/structure.h"
#include "insieme/core/pattern/match.h"
#include "insieme/core/pattern/match_target.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace pattern {

namespace impl {

	// --- forward declarations (pimpl) ---

	struct TreePattern;
	typedef std::shared_ptr<TreePattern> TreePatternPtr;

	struct ListPattern;
	typedef std::shared_ptr<ListPattern> ListPatternPtr;
}

// TODO: clean this up
typedef Match<ptr_target> NodeMatch;
typedef boost::optional<NodeMatch> MatchOpt;
typedef Match<address_target> AddressMatch;
typedef boost::optional<AddressMatch> AddressMatchOpt;
typedef Match<tree_target> TreeMatch;
typedef boost::optional<TreeMatch> TreeMatchOpt;


/**
 * The type utilized to represent tree patterns matching tree structures.
 */
class TreePattern : public utils::Printable {
	/**
	 * The internal implementation of the represented pattern.
	 */
	impl::TreePatternPtr pattern;

  public:
	/**
	 * A default constructed pattern matching everything -- equals the any pattern.
	 */
	TreePattern();

	/**
	 * A copy constructor for patterns.
	 */
	TreePattern(const TreePattern& other) : pattern(other.pattern) {
		assert_true(pattern) << "Pattern pointer must not be null!";
	}

	/**
	 * A r-value move constructor for patterns.
	 */
	TreePattern(TreePattern&& other) : pattern(other.pattern) {
		assert_true(pattern) << "Pattern pointer must not be null!";
	}

	/**
	 * A constructor to be utilized by factories to create a pattern based on the implementation.
	 */
	TreePattern(const impl::TreePatternPtr& pattern) : pattern(pattern) {
		assert_true(pattern) << "Pattern pointer must not be null!";
	}


	/**
	 * An implicit conversion to the internal implementation structure.
	 */
	operator const impl::TreePatternPtr&() const {
		return pattern;
	}


	bool match(const core::NodePtr& node) const {
		return matchPointer(node);
	}

	MatchOpt matchPointer(const core::NodePtr& node) const;

	AddressMatchOpt matchAddress(const core::NodeAddress& node) const;

	TreeMatchOpt matchTree(const TreePtr& tree) const;

	/**
	 * Enable default handling of assignments.
	 */
	TreePattern& operator=(const TreePattern&) = default;


	/**
	 * Prints this pattern to the given output stream -- implements the Printable interface.
	 */
	std::ostream& printTo(std::ostream& out) const;
};


/**
 * The type utilized to represent list patterns matching list of trees (forests).
 */
class ListPattern : public utils::Printable {
	/**
	 * The internal implementation of the represented pattern.
	 */
	impl::ListPatternPtr pattern;

  public:
	/**
	 * A default constructed pattern matching every list.
	 */
	ListPattern();

	/**
	 * A copy constructor for list patterns.
	 */
	ListPattern(const ListPattern& other) : pattern(other.pattern) {
		assert_true(pattern) << "Pattern pointer must not be null!";
	}

	/**
	 * A r-value move constructor for patterns.
	 */
	ListPattern(ListPattern&& other) : pattern(other.pattern) {
		assert_true(pattern) << "Pattern pointer must not be null!";
	}

	/**
	 * A constructor to be utilized by factories to create a pattern based on the implementation.
	 */
	ListPattern(const impl::ListPatternPtr& pattern) : pattern(pattern) {
		assert_true(pattern) << "Pattern pointer must not be null!";
	}

	/**
	 * Utilize default assignment operator.
	 */
	ListPattern& operator=(const ListPattern&) = default;

	/**
	 * An implicit conversion to the internal implementation structure.
	 */
	operator const impl::ListPatternPtr() const {
		return pattern;
	}

	TreeMatchOpt match(const vector<TreePtr>& trees) const;

	/**
	 * Prints this pattern to the given output stream -- implements the Printable interface.
	 */
	std::ostream& printTo(std::ostream& out) const;
};


// -- some constants --

extern const TreePattern any;
extern const TreePattern recurse;

extern const ListPattern anyList;
extern const ListPattern empty;


// -- some factories --

TreePattern value(const core::NodeValue& value);


TreePattern atom(const TreePtr& tree);

TreePattern atom(const core::NodePtr& tree);

TreePattern lazyAtom(const std::function<core::NodePtr(core::NodeManager&)>& factory);

// The following piece of code resolves the issue of overloads on std::function being ambiguous
namespace impl {
	insieme::core::pattern::TreePattern lambda(const std::function<bool(const core::NodePtr&)>& condition);
	insieme::core::pattern::TreePattern lambda(const std::function<bool(const core::NodeAddress&)>& condition);
}
template <typename T>
typename std::enable_if<std::is_same<typename lambda_traits<T>::argument_type, const core::NodePtr&>::value, TreePattern>::type lambda(const T& condition) {
	return impl::lambda(static_cast<const std::function<bool(const core::NodePtr&)>&>(condition));
}
template <typename T>
typename std::enable_if<std::is_same<typename lambda_traits<T>::argument_type, const core::NodeAddress&>::value, TreePattern>::type lambda(const T& condition) {
	return impl::lambda(static_cast<const std::function<bool(const core::NodeAddress&)>&>(condition));
}

ListPattern single(const TreePattern& pattern);

inline ListPattern single(const TreePtr& tree) {
	return single(atom(tree));
}


TreePattern operator!(const TreePattern& a);

TreePattern operator&(const TreePattern& a, const TreePattern& b);

TreePattern operator|(const TreePattern& a, const TreePattern& b);


TreePattern node(const ListPattern& pattern = empty);

TreePattern node(const char id, const ListPattern& pattern = empty);

TreePattern node(const core::NodeType type, const ListPattern& pattern = empty);

inline TreePattern node(const char id, const TreePattern& pattern) {
	return node(id, single(pattern));
}

inline TreePattern node(const core::NodeType type, const TreePattern& pattern) {
	return node(type, single(pattern));
}

TreePattern var(const std::string& name, const TreePattern& pattern = any);

inline TreePattern treeVar(const std::string& name, const TreePattern& pattern = any) {
	return var(name, pattern);
}

ListPattern listVar(const std::string& name, const ListPattern& pattern = anyList);


TreePattern aT(const std::vector<TreePattern>& patterns);

template <typename... Patterns>
inline TreePattern aT(Patterns... patterns) {
	return aT(toVector<TreePattern>(patterns...));
}

TreePattern rT(const TreePattern& pattern, const string& varName = "x");

TreePattern rT(const ListPattern& pattern, const string& varName = "x");

TreePattern rec(const string& varName = "x");

ListPattern operator|(const ListPattern& a, const ListPattern& b);

ListPattern operator|(const TreePattern& a, const ListPattern& b);

ListPattern operator|(const ListPattern& a, const TreePattern& b);

inline ListPattern opt(const ListPattern& pattern) {
	return empty | pattern;
}
inline ListPattern opt(const TreePattern& pattern) {
	return opt(single(pattern));
}
inline ListPattern opt(const TreePtr& tree) {
	return opt(atom(tree));
}

ListPattern operator*(const ListPattern& pattern);

ListPattern operator*(const TreePattern& pattern);

ListPattern operator+(const ListPattern& pattern);

ListPattern operator+(const TreePattern& pattern);

ListPattern operator<<(const ListPattern& a, const ListPattern& b);

ListPattern operator<<(const TreePattern& a, const ListPattern& b);

ListPattern operator<<(const ListPattern& a, const TreePattern& b);

ListPattern operator<<(const TreePattern& a, const TreePattern& b);

ListPattern operator>>(const ListPattern& a, const ListPattern& b);

ListPattern operator>>(const TreePattern& a, const ListPattern& b);

ListPattern operator>>(const ListPattern& a, const TreePattern& b);

ListPattern operator>>(const TreePattern& a, const TreePattern& b);

// more complex stuff ...

inline TreePattern step(const TreePattern& a) {
	return node(anyList << a << anyList);
}

inline TreePattern all(const TreePattern& a, const TreePattern& limit = !any) {
	// collect all occurs of pattern a
	return rT((a & !limit & node(*recurse)) | ((!a) & !limit & node(*recurse)));
}

inline TreePattern outermost(const TreePattern& a) {
	// it is the outer most or not, then the next is nested
	return rT(a | ((!a) & node(*recurse)));
}

inline TreePattern innermost(const TreePattern& a) {
	// select all that do not contain another a
	return rT(((!step(aT(a))) & a) | node(*recurse));
}

} // end namespace pattern
} // end namespace core
} // end namespace insieme
