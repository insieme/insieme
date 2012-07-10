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


#include "insieme/core/ir_node.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/printable.h"

#include "insieme/transform/pattern/pattern.h"

namespace insieme {
namespace transform {
namespace filter {

	/**
	 * This Header file contains a definition for several node filters to be used within
	 * several transformations e.g. for specifying potential transformation targets.
	 *
	 * Beside the pure definition of the filter types, a list of utilities for defining
	 * default filters or combining filters is provided.
	 */

	/**
	 * The type defining the base type for all kind of unary node functions (hence filters).
	 */
	template<typename T>
	class UnaryNodeFunction : public std::unary_function<T, const core::NodePtr&> {

		/**
		 * A string-based description for this unary function.
		 */
		std::string desc;

		/**
		 * The actual unary function to be used internally.
		 */
		std::function<T(const core::NodePtr&)> fun;

	public:

		UnaryNodeFunction(const std::string& desc, const std::function<T(const core::NodePtr&)>& fun)
			: desc(desc), fun(fun) {}

		T operator()(const core::NodePtr& node) const {
			return fun(node);
		}

		bool operator==(const UnaryNodeFunction<T>& other) const {
			return desc == other.desc;
		}

		bool operator!=(const UnaryNodeFunction<T>& other) const {
			return !(*this == other);
		}

		bool operator<(const UnaryNodeFunction<T>& other) const {
			return desc < other.desc;
		}

		const std::string& getDescription() const {
			return desc;
		}
	};


	/**
	 * The type defining a filter.
	 */
	class Filter : public UnaryNodeFunction<bool> {
	public:

		Filter(const std::string& desc, const std::function<bool(const core::NodePtr&)>& filter)
			: UnaryNodeFunction<bool>(desc, filter) {}

	};

	/**
	 * The type defining a target filter
	 */
	class TargetFilter : public UnaryNodeFunction<vector<core::NodeAddress>> {
	public:

		TargetFilter(const std::string& desc, const std::function<vector<core::NodeAddress>(const core::NodePtr&)>& filter)
			: UnaryNodeFunction<vector<core::NodeAddress>>(desc, filter) {}

	};


	// -- Utilities --------------------------------------------------------------

	// accept all nodes
	extern const Filter all;

	// don't accept any node
	extern const Filter none;


	// boolean operators:

	inline Filter operator!(const Filter& a) {
		return Filter(format("!%s", a.getDescription().c_str()), [=](const core::NodePtr& node) { return !a(node); });
	}

	inline Filter operator&(const Filter& a, const Filter& b) {
		return Filter(format("(%s & %s)", a.getDescription().c_str(), b.getDescription().c_str()),
				[=](const core::NodePtr& node) { return a(node) && b(node); });
	}

	inline Filter operator|(const Filter& a, const Filter& b) {
		return Filter(format("(%s | %s)", a.getDescription().c_str(), b.getDescription().c_str()),
				[=](const core::NodePtr& node) { return a(node) || b(node); });
	}


	// pattern based filter
	inline Filter pattern(const string& name, const pattern::TreePatternPtr& pattern) {
		return Filter(name, [=](const core::NodePtr& node)->bool { return pattern->matchPointer(node); });
	}

	inline Filter pattern(const pattern::TreePatternPtr& treePattern) {
		return pattern(format("pattern(%s)", toString(treePattern).c_str()), treePattern);
	}


	// -- target filter ---

	// produces an empty list
	extern const TargetFilter empty;

	// takes the root node and returns it as a result
	extern const TargetFilter root;

	/**
	 * This pattern based filter is trying to match against a potential target node. If
	 * it matches, it returns the list of matches bound to the given variable.
	 */
	TargetFilter pattern(const string& name, const pattern::TreePatternPtr& pattern, const string& var);

	/**
	 * This pattern based filter is trying to match against a potential target node. If
	 * it matches, it returns the list of matches bound to the given variable.
	 */
	inline TargetFilter pattern(const pattern::TreePatternPtr& treePattern, const string& var) {
		return pattern(format("all %s within (%s)", var.c_str(), toString(treePattern).c_str()), treePattern, var);
	}

	/**
	 * Creates a filter searching all sub-structures matching the given pattern.
	 */
	TargetFilter allMatches(const string& name, const pattern::TreePatternPtr& pattern, bool ignoreTypes = true);

	inline TargetFilter allMatches(const pattern::TreePatternPtr& pattern, bool ignoreTypes = true) {
		return allMatches(format("all matching (%s)", toString(pattern).c_str()), pattern, ignoreTypes);
	}

} // end namespace filter
} // end namespace transform
} // end namespace insieme

namespace std {

	template<typename T>
	inline std::ostream& operator<<(std::ostream& out, const insieme::transform::filter::UnaryNodeFunction<T>& filter) {
		return out << filter.getDescription();
	}

} // end namespace std
