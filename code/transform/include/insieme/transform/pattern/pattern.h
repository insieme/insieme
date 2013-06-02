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
#include <unordered_map>

#include <boost/optional.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/transform/pattern/structure.h"
#include "insieme/transform/pattern/match.h"
#include "insieme/transform/pattern/match_target.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {
namespace pattern {

	class Pattern;
	typedef std::shared_ptr<Pattern> PatternPtr;

	class TreePattern;
	typedef std::shared_ptr<TreePattern> TreePatternPtr;

	class ListPattern;
	typedef std::shared_ptr<ListPattern> ListPatternPtr;

//	class Filter;
//	typedef std::shared_ptr<Filter> FilterPtr;

	namespace details {

		bool isTypeOrValueOrParam(const core::NodeType type);
		inline bool isTypeOrValueOrParam(const int type) { return false; }

		inline bool isTypeOrValueOrParam(const core::NodeAddress node) {
			return isTypeOrValueOrParam(node->getNodeType());
		}

		inline bool isTypeOrValueOrParam(const TreePtr&) { return false; }

	}

	typedef boost::optional<Match<ptr_target>> MatchOpt;
	typedef boost::optional<Match<address_target>> AddressMatchOpt;
	typedef boost::optional<Match<tree_target>> TreeMatchOpt;


	struct Pattern : public utils::Printable {

		/**
		 * A flag indicating that this pattern does not contain any variables.
		 */
		bool isVariableFree;

		Pattern(bool isVariableFree) : isVariableFree(isVariableFree) {}

		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};


	// The abstract base class for tree patterns
	struct TreePattern : public Pattern {

		// a list of all types of tree pattern constructs
		enum Type {
			Value, Constant, Variable, Wildcard, Node, Negation, Conjunction, Disjunction, Descendant, Recursion
		};

		/**
		 * The type of this tree pattern
		 */
		const Type type;

		/**
		 * A flag indicating whether the matched tree may be a type or a type parameter.
		 */
		bool mayBeType;

		TreePattern(const Type type, bool isVariableFree, bool mayBeType = true) : Pattern(isVariableFree), type(type), mayBeType(mayBeType) {};

		bool match(const core::NodePtr& node) const {
			return matchPointer(node);
		}

		MatchOpt matchPointer(const core::NodePtr& node) const;

		AddressMatchOpt matchAddress(const core::NodeAddress& node) const;

		TreeMatchOpt matchTree(const TreePtr& tree) const;
	};


	// An abstract base node for node patterns
	struct ListPattern : public Pattern {

		enum Type {
			Empty, Single, Variable, Alternative, Sequence, Repetition
		};

		const Type type;

		ListPattern(const Type type, bool isVariableFree) : Pattern(isVariableFree), type(type) {};

		TreeMatchOpt match(const vector<TreePtr>& trees) const;
	};


	namespace tree {

		// For convenience - could also be handled by a Constant but would require a NodeManager
		struct Value : public TreePattern {

			const core::NodeValue value;

			Value(const core::NodeValue& value) : TreePattern(TreePattern::Value, true), value(value) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << value;
			}

		};

		// An constant value - a pure IR node
		struct Constant : public TreePattern {

			// kind of a hack - but the easiest solution
			const core::NodePtr nodeAtom;
			const TreePtr treeAtom;

			Constant(const core::NodePtr& atom)
				: TreePattern(TreePattern::Constant, true, details::isTypeOrValueOrParam(atom->getNodeType())), nodeAtom(atom) {}
			Constant(const TreePtr& atom) : TreePattern(TreePattern::Constant, true), treeAtom(atom) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if (nodeAtom) {
					return out << *nodeAtom;
				}
				return out << *treeAtom;
			}
		};

		// A wildcard for the pattern matching of a tree - accepts everything
		struct Wildcard : public TreePattern {

			Wildcard() : TreePattern(TreePattern::Wildcard, true) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "_";
			}
		};


		// A simple variable
		struct Variable : public TreePattern {
			const static TreePatternPtr any;
			const std::string name;
			const TreePatternPtr pattern;

			Variable(const std::string& name, const TreePatternPtr& pattern = any)
				: TreePattern(TreePattern::Variable, false, pattern->mayBeType), name(name), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				out << "$" << name;
				if(pattern && pattern != any) {
					out << ":" << *pattern;
				}
				return out;
			}

		};

		// Depth recursion (downward * operator)
		struct Recursion : public TreePattern {
			const string name;
			const bool terminal;
			const TreePatternPtr pattern;

			Recursion(const string& name)
				: TreePattern(TreePattern::Recursion, false), name(name), terminal(true) {}
			Recursion(const string& name, const TreePatternPtr& pattern)
				: TreePattern(TreePattern::Recursion, pattern->isVariableFree, pattern->mayBeType), name(name), terminal(false), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(terminal) return out << "rec." << name;
				return out << "rT." << name << "(" << *pattern << ")";
			}
		};

		// bridge to Node Pattern
		struct Node : public TreePattern {

			const int id;
			const int type;
			const ListPatternPtr pattern;

			Node(const ListPatternPtr& pattern)
				: TreePattern(TreePattern::Node, pattern->isVariableFree), id(-1), type(-1), pattern(pattern) {}

			Node(char id, const ListPatternPtr& pattern)
				: TreePattern(TreePattern::Node, pattern->isVariableFree), id(id), type(-1), pattern(pattern) {}

			Node(const core::NodeType type, const ListPatternPtr& pattern)
				: TreePattern(TreePattern::Node, pattern->isVariableFree, details::isTypeOrValueOrParam(type)), id(-1), type(type), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(id != -1) {
					out << "(" << id << "|";
				} else if (type != -1) {
					out << "(" << ((core::NodeType)type) << "|";
				} else {
					out << "(";
				}
				return out << *pattern << ")";
			}

		};


		// Negation
		struct Negation : public TreePattern {
			const TreePatternPtr pattern;

			Negation(const TreePatternPtr& pattern)
				: TreePattern(TreePattern::Negation, pattern->isVariableFree), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "!(" << *pattern << ")";
			}

		};

		// Conjunction
		struct Conjunction : public TreePattern {
			const TreePatternPtr pattern1;
			const TreePatternPtr pattern2;

			Conjunction(const TreePatternPtr& a, const TreePatternPtr& b)
				: TreePattern(TreePattern::Conjunction, a->isVariableFree && b->isVariableFree, a->mayBeType && b->mayBeType), pattern1(a), pattern2(b) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *pattern1 << " & " << *pattern2;
			}

		};

		// Disjunction
		struct Disjunction : public TreePattern {
			const TreePatternPtr pattern1;
			const TreePatternPtr pattern2;

			Disjunction(const TreePatternPtr& a, const TreePatternPtr& b)
				: TreePattern(TreePattern::Disjunction, a->isVariableFree && b->isVariableFree, a->mayBeType || b->mayBeType), pattern1(a), pattern2(b) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *pattern1 << " | " << *pattern2;
			}

		};


		struct Descendant : public TreePattern {
			const std::vector<TreePatternPtr> subPatterns;

			template<typename ... Patterns>
			Descendant(Patterns ... patterns)
				: TreePattern(TreePattern::Descendant, all(toVector<TreePatternPtr>(patterns...), [](const TreePatternPtr& cur) { return cur->isVariableFree; })),
				  subPatterns(toVector<TreePatternPtr>(patterns...)) {};

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "aT(" << join(",", subPatterns, print<id<TreePatternPtr>>()) << ")";
			}
		};

	}

	namespace list {

		struct Empty : public ListPattern {
			Empty() : ListPattern(ListPattern::Empty, true) {}
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "[]";
			}
		};

		// The most simple node pattern covering a single tree
		struct Single : public ListPattern {
			const TreePatternPtr element;

			Single(const TreePatternPtr& element)
			: ListPattern(ListPattern::Single, element->isVariableFree), element(element) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *element;
			}

		};

		// A sequence node pattern representing the composition of two node patterns
		struct Sequence : public ListPattern {
			const ListPatternPtr left;
			const ListPatternPtr right;

			Sequence(const ListPatternPtr& left, const ListPatternPtr& right)
				: ListPattern(ListPattern::Sequence, left->isVariableFree && right->isVariableFree), left(left), right(right) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *left << "," << *right;
			}

		};

		// A node pattern alternative
		struct Alternative : public ListPattern {
			const ListPatternPtr alternative1;
			const ListPatternPtr alternative2;

			Alternative(const ListPatternPtr& A, const ListPatternPtr& B)
				: ListPattern(ListPattern::Alternative, A->isVariableFree && B->isVariableFree), alternative1(A), alternative2(B) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *alternative1 << "|" << *alternative2;
			}

		};

		// Realizes the star operator for node patterns
		struct Repetition : public ListPattern {
			const ListPatternPtr pattern;
			const unsigned minRep;			// minimum number of repetitions

			Repetition(const ListPatternPtr& pattern, unsigned minRep = 0)
				: ListPattern(ListPattern::Repetition, pattern->isVariableFree), pattern(pattern), minRep(minRep) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if (minRep == 0) {
					return out << "[" << *pattern << "]*";
				}
				if (minRep == 1) {
					return out << "[" << *pattern << "]+";
				}
				return out << "[" << *pattern << "]*{" << minRep << "}";
			}

		};

		// A simple variable
		struct Variable : public ListPattern {
			const static ListPatternPtr any;
			const std::string name;
			const ListPatternPtr pattern;

			Variable(const std::string& name, const ListPatternPtr& pattern = any)
				: ListPattern(ListPattern::Variable, false), name(name), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				out << "$" << name;
				if(pattern && pattern != any) {
					out << ":" << *pattern;
				}
				return out;
			}

		};

	}
	
	extern const TreePatternPtr any;
	extern const TreePatternPtr recurse;

	extern const ListPatternPtr anyList;
	extern const ListPatternPtr empty;

	inline TreePatternPtr value(const core::NodeValue& value) {
		return std::make_shared<tree::Value>(value);
	}

	inline TreePatternPtr atom(const TreePtr& tree) {
		return std::make_shared<tree::Constant>(tree);
	}
	inline TreePatternPtr atom(const core::NodePtr& tree) {
		return std::make_shared<tree::Constant>(tree);
	}

	inline ListPatternPtr single(const TreePatternPtr& pattern) {
		return std::make_shared<list::Single>(pattern);
	}
	inline ListPatternPtr single(const TreePtr& tree) {
		return single(atom(tree));
	}

	inline TreePatternPtr operator!(const TreePatternPtr& a) {
		return std::make_shared<tree::Negation>(a);
	}
	
	inline TreePatternPtr operator&(const TreePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<tree::Conjunction>(a,b);
	}

	inline TreePatternPtr operator|(const TreePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<tree::Disjunction>(a,b);
	}

	inline TreePatternPtr node(const ListPatternPtr& pattern = empty) {
		return std::make_shared<tree::Node>(pattern);
	}
	inline TreePatternPtr node(const char id, const ListPatternPtr& pattern = empty) {
		return std::make_shared<tree::Node>(id, pattern);
	}
	inline TreePatternPtr node(const char id, const TreePatternPtr& pattern) {
		return node(id, single(pattern));
	}
	inline TreePatternPtr node(const core::NodeType type, const ListPatternPtr& pattern = empty) {
		return std::make_shared<tree::Node>(type, pattern);
	}
	inline TreePatternPtr node(const core::NodeType type, const TreePatternPtr& pattern) {
		return node(type, single(pattern));
	}

	inline TreePatternPtr var(const std::string& name, const TreePatternPtr& pattern = any) {
		return std::make_shared<tree::Variable>(name, pattern);
	}

	inline TreePatternPtr treeVar(const std::string& name, const TreePatternPtr& pattern = any) {
		return var(name, pattern);
	}

	inline ListPatternPtr listVar(const std::string& name, const ListPatternPtr& pattern = anyList) {
		return std::make_shared<list::Variable>(name, pattern);
	}

	template<typename ... Patterns>
	inline TreePatternPtr aT(Patterns ... patterns) {
		return std::make_shared<tree::Descendant>(patterns...);
	}

	inline TreePatternPtr rT(const TreePatternPtr& pattern, const string& varName = "x") {
		return std::make_shared<tree::Recursion>(varName, pattern);
	}
	inline TreePatternPtr rT(const ListPatternPtr& pattern, const string& varName = "x") {
		return std::make_shared<tree::Recursion>(varName, node(pattern));
	}
	inline TreePatternPtr rec(const string& varName = "x") {
		return std::make_shared<tree::Recursion>(varName);
	}

	inline ListPatternPtr operator|(const ListPatternPtr& a, const ListPatternPtr& b) {
		return std::make_shared<list::Alternative>(a,b);
	}
	inline ListPatternPtr operator|(const TreePatternPtr& a, const ListPatternPtr& b) {
		return std::make_shared<list::Alternative>(single(a),b);
	}
	inline ListPatternPtr operator|(const ListPatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<list::Alternative>(a,single(b));
	}

	inline ListPatternPtr opt(const ListPatternPtr& pattern) {
		return empty | pattern;
	}
	inline ListPatternPtr opt(const TreePatternPtr& pattern) {
		return opt(single(pattern));
	}
	inline ListPatternPtr opt(const TreePtr& tree) {
		return opt(atom(tree));
	}

	inline ListPatternPtr operator*(const ListPatternPtr& pattern) {
		return std::make_shared<list::Repetition>(pattern);
	}
	inline ListPatternPtr operator*(const TreePatternPtr& pattern) {
		return std::make_shared<list::Repetition>(single(pattern));
	}

	inline ListPatternPtr operator+(const ListPatternPtr& pattern) {
		return std::make_shared<list::Repetition>(pattern, 1);
	}
	inline ListPatternPtr operator+(const TreePatternPtr& pattern) {
		return std::make_shared<list::Repetition>(single(pattern), 1);
	}

	inline ListPatternPtr operator<<(const ListPatternPtr& a, const ListPatternPtr& b) {
		return std::make_shared<list::Sequence>(a,b);
	}
	inline ListPatternPtr operator<<(const TreePatternPtr& a, const ListPatternPtr& b) {
		return std::make_shared<list::Sequence>(single(a),b);
	}
	inline ListPatternPtr operator<<(const ListPatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<list::Sequence>(a,single(b));
	}
	inline ListPatternPtr operator<<(const TreePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<list::Sequence>(single(a),single(b));
	}

	// more complex stuff ...

	inline TreePatternPtr all(const TreePatternPtr& a) {
		// collect all occurs of pattern a
		return rT((a & node(*recurse)) | (!a & node(*recurse)));
	}

	inline TreePatternPtr outermost(const TreePatternPtr& a) {
		// it is the outer most or not, then the next is nested
		return rT(a | (!a & node(*rec("_outermost"))), "_outermost");
	}

	inline TreePatternPtr step(const TreePatternPtr& a) {
		return node(anyList << a << anyList);
	}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::PatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::TreePatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::ListPatternPtr& pattern);

} // end namespace std
