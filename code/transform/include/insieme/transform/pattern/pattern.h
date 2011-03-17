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

#include <string>
#include <memory>
#include <ostream>

#include "insieme/transform/pattern/structure.h"

#include "insieme/core/ast_node.h"

namespace insieme {
namespace transform {
namespace pattern {



	class Pattern;
	typedef std::shared_ptr<Pattern> PatternPtr;

	class ListPattern;
	typedef std::shared_ptr<ListPattern> ListPatternPtr;

	class Match;
	typedef std::shared_ptr<Match> MatchPtr;

	class Filter;
	typedef std::shared_ptr<Filter> FilterPtr;

	class Tree;
	typedef std::shared_ptr<Tree> TreePtr;

//	MatchPtr match(PatternPtr, TreePtr);

	bool match(PatternPtr, TreePtr);


	// The abstract base class
	class Pattern {
//		const std::vector<FilterPtr> filter;
	public:
		virtual bool match(const TreePtr& tree) const =0;
		virtual std::ostream& printTo(std::ostream& out) const =0;
	};

	// An atomic value - a pure IR node
	class Atom : public Pattern {
		const TreePtr atom;
	public:
		Atom(const TreePtr& atom) : atom(atom) {}
		virtual bool match(const TreePtr& tree) const {
			return atom == tree;
		}
		virtual std::ostream& printTo(std::ostream& out) const {
			return atom->printTo(out);
		}
	};

	// A simple variable
	class Variable : public Pattern {
		const std::string name;
	};

	// Alternative
	class Alternative : public Pattern {
		const PatternPtr alternative1;
		const PatternPtr alternative2;
	public:
		Alternative(const PatternPtr& a, const PatternPtr& b) : alternative1(a), alternative2(b) {}

		virtual bool match(const TreePtr& tree) const {
			return alternative1->match(tree) || alternative2->match(tree);
		}
		virtual std::ostream& printTo(std::ostream& out) const {
			alternative1->printTo(out);
			out << " | ";
			alternative2->printTo(out);
			return out;
		}
	};

	// Negation
	class Negation : public Pattern {
		const PatternPtr pattern;
	public:
		Negation(const PatternPtr& pattern) : pattern(pattern) {}

		virtual bool match(const TreePtr& tree) const {
			return !pattern->match(tree);
		}
		virtual std::ostream& printTo(std::ostream& out) const {
			out << "!(";
			pattern->printTo(out);
			out << ")";
			return out;
		}
	};


	PatternPtr atom(const TreePtr& tree) {
		return std::make_shared<Atom>(tree);
	}

	PatternPtr operator|(const PatternPtr& a, const PatternPtr& b) {
		return std::make_shared<Alternative>(a,b);
	}

	PatternPtr operator!(const PatternPtr& a) {
		return std::make_shared<Negation>(a);
	}


	std::ostream& operator<<(std::ostream& out, const PatternPtr& pattern);


//	// A pattern for a list
//	class ListPattern : public Pattern {
//
//	};
//
//	// A simple pattern for a single list element
//	class ListPatternElement : public ListPattern {
//		const PatternPtr pattern;
//	};
//
//	class ListPatternSequence : public ListPattern {
//		const ListPatternPtr partA;
//		const PatternPtr partB;
//	};
//
//	class ListPatternStar : public ListPattern {
//		const ListPatternPtr subPattern;
//	};

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
