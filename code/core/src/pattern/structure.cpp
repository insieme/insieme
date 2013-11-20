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

#include "insieme/core/pattern/structure.h"

#include <boost/lexical_cast.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace pattern {

	const char Tree::VALUE_ID = 'v';

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

	namespace {

		struct InvalidTreeException : public std::exception {
			virtual const char* what() const throw() {
				return "Unable to parse input tree!";
			}
		};

		typedef string::iterator iter;

		TreePtr parseTree(const iter begin, const iter end);

		TreeList parseList(const iter begin, const iter end) {
			TreeList res;

			// shortcut
			if (begin == end) return res;

			// search commas and create trees
			iter cur = begin;
			while(true) {

				// find next comma
				int nesting = 0;
				iter it = cur;
				while (it != end && (nesting != 0 || *it != ',')) {
					if (*it == '(') nesting++;
					if (*it == ')') nesting--;
					it++;
				}

				// nesting level has to 0!
				if (nesting != 0) throw InvalidTreeException();

				res.push_back(parseTree(cur, it));

				if(it == end) break;
				cur = it+1;
			}


			return res;
		}

		TreePtr parseTree(const iter begin, const iter end) {
			auto length = end - begin;

			// if there is only one character left => done
			if (length == 1) {
				return makeTree(*begin);
			}

			// it has to end with a parentheses
			if (length < 3 || *(begin+1) != '(' || *(end-1) != ')' ) {
				throw InvalidTreeException();
			}

			// create resulting tree
			return makeTree(*begin, parseList(begin+2, end-1));
		}

	}

	TreePtr parseTree(const string& tree) {

		// replace all blanks
		string normalized = tree;

		auto a = normalized.begin();
		auto b = std::remove(a, normalized.end(), ' ');

		// parse tree
		return parseTree(a,b);
	}

} // end namespace pattern
} // end namespace core
} // end namespace insieme
