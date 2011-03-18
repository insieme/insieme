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

#include "insieme/transform/pattern/pattern.h"


namespace insieme {
namespace transform {
namespace pattern {

	const TreePatternPtr any = std::make_shared<trees::Wildcard>();

	std::ostream& operator<<(std::ostream& out, const PatternPtr& pattern) {
		return (pattern)?(pattern->printTo(out)):(out << "null");
	}

	std::ostream& operator<<(std::ostream& out, const TreePatternPtr& pattern) {
		return (pattern)?(pattern->printTo(out)):(out << "null");
	}

	std::ostream& operator<<(std::ostream& out, const NodePatternPtr& pattern) {
		return (pattern)?(pattern->printTo(out)):(out << "null");
	}

	namespace trees {

		bool contains(const TreePtr& tree, const TreePatternPtr& pattern) {
			bool res = false;
			res = res || pattern->match(tree);
			for_each(tree->getSubTrees(), [&](const TreePtr& cur) {
				res = res || contains(cur, pattern);
			});
			return res;
//			return pattern->match(tree) || any(tree->getSubTrees(), [&](const TreePtr& cur) {
//				return contains(cur, pattern);
//			});
		}

		bool Descendant::match(const TreePtr& tree) const {
			// search for all patterns occuring in the sub-trees
			return all(subPatterns, [&](const TreePatternPtr& cur) {
				return contains(tree, cur);
			});
		}


	}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
