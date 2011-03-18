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

	class TreePattern;
	typedef std::shared_ptr<TreePattern> TreePatternPtr;

	class NodePattern;
	typedef std::shared_ptr<NodePattern> NodePatternPtr;

	class Match;
	typedef std::shared_ptr<Match> MatchPtr;

	class Filter;
	typedef std::shared_ptr<Filter> FilterPtr;

	class Tree;
	typedef std::shared_ptr<Tree> TreePtr;

//	MatchPtr match(PatternPtr, TreePtr);

//	bool match(PatternPtr, TreePtr);


	class Pattern {
	public:
		virtual std::ostream& printTo(std::ostream& out) const =0;
	};


	// The abstract base class for tree patterns
	class TreePattern : public Pattern {
//		const std::vector<FilterPtr> filter;
	public:
		virtual bool match(const TreePtr& tree) const =0;
	};


	// An abstract base node for node patterns
	class NodePattern : public Pattern {
	public:
		/**
		 * Consumes parts of the vector starting from the given position. The return value
		 * points to the next element not consumed so far or -1 if the pattern is not matching.
		 */
		virtual int match(const std::vector<TreePtr>& trees, int start) const =0;
	};


	namespace trees {

		// An atomic value - a pure IR node
		class Atom : public TreePattern {
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
		class Variable : public TreePattern {
			const std::string name;
		};

		// A wildcard for the pattern matching of a tree - accepts everything
		class Wildcard : public TreePattern {
		public:
			virtual bool match(const TreePtr& tree) const {
				return true;
			}
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "_";
			}
		};


		// bridge to Node Pattern
		class NodeTreePattern : public TreePattern {
			const NodePatternPtr pattern;
		public:
			NodeTreePattern(const NodePatternPtr& pattern) : pattern(pattern) {}

			virtual bool match(const TreePtr& tree) const {
				// match list of sub-nodes
				auto list = tree->getSubTrees();
				return pattern->match(list, 0) == static_cast<int>(list.size());
			}
			virtual std::ostream& printTo(std::ostream& out) const {
				out << "(";
				pattern->printTo(out);
				return out << ")";
			}
		};


		// Alternative
		class Alternative : public TreePattern {
			const TreePatternPtr alternative1;
			const TreePatternPtr alternative2;
		public:
			Alternative(const TreePatternPtr& a, const TreePatternPtr& b) : alternative1(a), alternative2(b) {}

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
		class Negation : public TreePattern {
			const TreePatternPtr pattern;
		public:
			Negation(const TreePatternPtr& pattern) : pattern(pattern) {}

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

		class Descendant : public TreePattern {
			const std::vector<TreePatternPtr> subPatterns;
		public:
			template<typename ... Patterns>
			Descendant(Patterns ... patterns) : subPatterns(toVector<TreePatternPtr>(patterns...)) {};

			virtual bool match(const TreePtr& tree) const;
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "t**(" << join(",", subPatterns, print<id<TreePatternPtr>>()) << ")";
			}

		};

	}

	namespace nodes {

		// The most simple node pattern covering a single tree
		class Single : public NodePattern {
			const TreePatternPtr element;
		public:
			Single(const TreePatternPtr& element) : element(element) {}
			virtual int match(const std::vector<TreePtr>& trees, int start) const {
				if (start < 0 || start >= static_cast<int>(trees.size())) {
					return -1;
				}
				if (!element->match(trees[start])) {
					return -1;
				}
				return start+1;
			}
			virtual std::ostream& printTo(std::ostream& out) const {
				return element->printTo(out);
			}
		};

		// A sequence node pattern representing the composition of two node patterns
		class Sequence : public NodePattern {
			const NodePatternPtr left;
			const NodePatternPtr right;
		public:
			Sequence(const NodePatternPtr& left, const NodePatternPtr& right) : left(left), right(right) {}

			virtual int match(const std::vector<TreePtr>& trees, int start) const {
				// just concatenate the two patterns
				return right->match(trees, left->match(trees, start));
			}
			virtual std::ostream& printTo(std::ostream& out) const {
				left->printTo(out);
				out << ",";
				right->printTo(out);
				return out;
			}
		};

		// A node pattern alternative
		class Alternative : public NodePattern {
			const NodePatternPtr alternative1;
			const NodePatternPtr alternative2;
		public:
			Alternative(const NodePatternPtr& A, const NodePatternPtr& B) : alternative1(A), alternative2(B) {}

			virtual int match(const std::vector<TreePtr>& trees, int start) const {
				int res = alternative1->match(trees, start);
				if (res >= 0) {
					return res;
				}
				return alternative2->match(trees, start);
			}
			virtual std::ostream& printTo(std::ostream& out) const {
				alternative1->printTo(out);
				out << "|";
				alternative2->printTo(out);
				return out;
			}
		};

		// Realizes the star operator for node patterns
		class Repetition : public NodePattern {
			const NodePatternPtr pattern;
		public:
			Repetition(const NodePatternPtr& pattern) : pattern(pattern) {}

			virtual int match(const std::vector<TreePtr>& trees, int start) const {
				int last = start;
				int cur = start;
				// match greedy
				while (cur != -1) {
					last = cur;
					cur = pattern->match(trees, cur);
				}
				return last;
			}
			virtual std::ostream& printTo(std::ostream& out) const {
				out << "[";
				pattern->printTo(out);
				out << "]*";
				return out;
			}
		};

	}

	extern const TreePatternPtr any;

	TreePatternPtr atom(const TreePtr& tree) {
		return std::make_shared<trees::Atom>(tree);
	}

	TreePatternPtr operator|(const TreePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<trees::Alternative>(a,b);
	}

	TreePatternPtr operator!(const TreePatternPtr& a) {
		return std::make_shared<trees::Negation>(a);
	}

	TreePatternPtr node(const NodePatternPtr& pattern) {
		return std::make_shared<trees::NodeTreePattern>(pattern);
	}

	template<typename ... Patterns>
	TreePatternPtr aT(Patterns ... patterns) {
		return std::make_shared<trees::Descendant>(patterns...);
	}

	NodePatternPtr single(const TreePatternPtr& pattern) {
		return std::make_shared<nodes::Single>(pattern);
	}

	NodePatternPtr single(const TreePtr& tree) {
		return single(atom(tree));
	}

	NodePatternPtr operator|(const NodePatternPtr& a, const NodePatternPtr& b) {
		return std::make_shared<nodes::Alternative>(a,b);
	}

	NodePatternPtr operator*(const NodePatternPtr& pattern) {
		return std::make_shared<nodes::Repetition>(pattern);
	}

	NodePatternPtr operator,(const NodePatternPtr& a, const NodePatternPtr& b) {
		return std::make_shared<nodes::Sequence>(a,b);
	}

	std::ostream& operator<<(std::ostream& out, const PatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const TreePatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const NodePatternPtr& pattern);


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
