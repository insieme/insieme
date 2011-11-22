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
#include "insieme/transform/pattern/irconvert.h"

namespace insieme {
namespace transform {
namespace filter {

	/**
	 * This Header file contains a definition for a node filter to be used within
	 * several transformations e.g. for specifying potential transformation targets.
	 *
	 * Beside the pure definition of the filter type, a list of utilities for defining
	 * default filters or combining filters is provided.
	 */


	/**
	 * The type defining a filter.
	 */
	class Filter : public std::unary_function<bool, const core::NodePtr&> {

		/**
		 * A descriptive string-based expression for this filter.
		 */
		std::string strExpr;

		/**
		 * The actual filter mechanism.
		 */
		std::function<bool(const core::NodePtr&)> filter;

	public:

		Filter(const std::string& strExpr, const std::function<bool(const core::NodePtr&)> filter)
			: strExpr(strExpr), filter(filter) {}

		bool operator()(const core::NodePtr& node) const {
			return filter(node);
		}

		bool operator==(const Filter& other) const {
			return strExpr == other.strExpr;
		}

		bool operator!=(const Filter& other) const {
			return !(*this == other);
		}

		const std::string& getStrExpr() const {
			return strExpr;
		}
	};


	// -- Utilities --------------------------------------------------------------

	// accept all nodes
	extern const Filter all;

	// don't accept any node
	extern const Filter none;


	// boolean operators:

	inline Filter operator!(const Filter& a) {
		return Filter(format("!%s", a.getStrExpr().c_str()), [=](const core::NodePtr& node) { return !a(node); });
	}

	inline Filter operator&(const Filter& a, const Filter& b) {
		return Filter(format("(%s & %s)", a.getStrExpr().c_str(), b.getStrExpr().c_str()),
				[=](const core::NodePtr& node) { return a(node) && b(node); });
	}

	inline Filter operator|(const Filter& a, const Filter& b) {
		return Filter(format("(%s | %s)", a.getStrExpr().c_str(), b.getStrExpr().c_str()),
				[=](const core::NodePtr& node) { return a(node) || b(node); });
	}


	// pattern based filter
	inline Filter pattern(const pattern::TreePatternPtr& pattern) {
		return Filter(format("pattern(%s)", toString(pattern).c_str()),
				[=](const core::NodePtr& node)->bool { return pattern->match(pattern::toTree(node)); });
	}

} // end namespace filter
} // end namespace transform
} // end namespace insieme

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const insieme::transform::filter::Filter& filter) {
		return out << filter.getStrExpr();
	}

} // end namespace std
