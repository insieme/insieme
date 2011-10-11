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

#include "insieme/transform/pattern/structure.h"

#include <boost/lexical_cast.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {
namespace pattern {

	const int Tree::VALUE_ID = -1;

	namespace {

		struct ValuePrinter {
			typedef string result_type;

			string operator()(bool value) const {
				return (value)?"true":"false";
			}

			string operator()(const string& value) const {
				return "\"" + value + "\"";
			}

			template<typename T>
			string operator()(const T& value) const {
				return boost::lexical_cast<string>(value);
			}
		};

	}


	std::ostream& Tree::printTo(std::ostream& out) const {

		// handle values differently
		if (id == VALUE_ID) {
			return out << boost::apply_visitor(ValuePrinter(), value);
		}

		// print symbol if present
		if (id) out << (char)id;

		// add sub-trees
		if (!subTrees.empty()) {
			out << "(" << join(",", subTrees, print<deref<TreePtr>>()) << ")";
		}

		// in case neither a symbol nor subtrees are given
		if (!id && subTrees.empty()) {
			out << "()";
		}
		return out;
	}

	bool Tree::operator==(const Tree& other) const {
		if (this == &other) {
			return true;
		}
		return id == other.id && equals(getSubTrees(), other.getSubTrees(), equal_target<TreePtr>()) && value == other.value;
	}

	std::ostream& operator<<(std::ostream& out, const Tree& tree) {
		return tree.printTo(out);
	}

	std::ostream& operator<<(std::ostream& out, const TreePtr& tree) {
		return out << *tree;
	}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
