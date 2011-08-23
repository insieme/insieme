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

#include "insieme/transform/pattern/structure.h"

#include "insieme/utils/logging.h"

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

//	bool match(TreePatternPtr, TreePtr);
//	bool match(NodePatternPtr, TreePtr);

	class MatchContext {
		typedef std::unordered_map<std::string, const TreePtr> VarMap;
		VarMap boundVariables;
		TreePatternPtr recursion;

	public:
		MatchContext() { }

		MatchContext(const MatchContext& from)  {
			boundVariables = VarMap(from.boundVariables);
		}

		MatchContext& operator=(const MatchContext& from) {
			boundVariables = from.boundVariables;
			return *this;
		}

		void bindVar(const std::string& var, const TreePtr match) {
			assert(!isBound(var) && "Variable bound twice");
			boundVariables.insert(VarMap::value_type(var, match));
		}
		bool isBound(const std::string& var) {
			return boundVariables.find(var) != boundVariables.end();
		}
		TreePtr getBound(const std::string& var) {
			assert(isBound(var) && "Requesting bound value for unbound var");
			return boundVariables[var];
		}

		const TreePatternPtr& getRecursion() {
			return recursion;
		}
		void setRecursion(const TreePatternPtr& rec) {
			recursion = rec;
		}
	};


	class Pattern {
	public:
		virtual std::ostream& printTo(std::ostream& out) const =0;
	};

	namespace nodes {
		class Single;
	}

	// The abstract base class for tree patterns
	class TreePattern : public Pattern {
		friend class nodes::Single;
//		const std::vector<FilterPtr> filter;
	public:
		bool match(const TreePtr& tree) const;
	// protected: TODO: make this protected again
		virtual bool match(MatchContext& context, const TreePtr& tree) const =0;
	};

	namespace trees {
		class NodeTreePattern;
	}

	// An abstract base node for node patterns
	class NodePattern : public Pattern {
		friend class trees::NodeTreePattern;
	public:
		bool match(const TreePtr& tree) const;
	// protected: TODO: make this protected again
		/**
		 * Consumes parts of the vector starting from the given position. The return value
		 * points to the next element not consumed so far or -1 if the pattern is not matching.
		 */
		virtual int match(MatchContext& context, const std::vector<TreePtr>& trees, int start) const =0;
	};


	namespace trees {

		// An atomic value - a pure IR node
		class Atom : public TreePattern {
			const TreePtr atom;
		public:
			Atom(const TreePtr& atom) : atom(atom) {}
			virtual std::ostream& printTo(std::ostream& out) const {
				return atom->printTo(out);
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				return *atom == *tree;
			}
		};

		// A simple variable
		class Variable : public TreePattern {
			const std::string name;
		public:
			Variable(const std::string& name) : name(name) {}
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "%" << name << "%";
			}
			const std::string& getName() {
				return name;
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				if(context.isBound(name)) {
					return *context.getBound(name) == *tree;
				} else {
					context.bindVar(name, tree);
				}
				return true;
			}
		};

		// A wildcard for the pattern matching of a tree - accepts everything
		class Wildcard : public TreePattern {
		public:
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "_";
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				return true;
			}
		};

		// Depth recursion (downward * operator)
		class Recursion : public TreePattern {
			bool terminal;
			const TreePatternPtr pattern;
		public:
			Recursion() : terminal(true) {}
			Recursion(const TreePatternPtr& pattern) : terminal(false), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(terminal) return out << "#recurse";
				else {
					out << "rT(";
					pattern->printTo(out);
					return out << ")";
				}
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				if(terminal) {
					return context.getRecursion()->match(context, tree);
				} else {
					context.setRecursion(pattern);
					return pattern->match(context, tree);
				}
			}
		};


		// bridge to Node Pattern
		class NodeTreePattern : public TreePattern {
			const NodePatternPtr pattern;
			const int id;
		public:
			NodeTreePattern(const NodePatternPtr& pattern) : pattern(pattern), id(-1) {}
			NodeTreePattern(const int id, const NodePatternPtr& pattern) : pattern(pattern), id(id) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(id != -1) {
					out << "(id:" << id << "|";
				} else out << "(";
				pattern->printTo(out);
				return out << ")";
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				if(id != -1 && id != tree->getId()) return false;
				// match list of sub-nodes
				auto list = tree->getSubTrees();
				return pattern->match(context, list, 0) == static_cast<int>(list.size());
			}
		};


		// Alternative
		class Alternative : public TreePattern {
			const TreePatternPtr alternative1;
			const TreePatternPtr alternative2;
		public:
			Alternative(const TreePatternPtr& a, const TreePatternPtr& b) : alternative1(a), alternative2(b) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				alternative1->printTo(out);
				out << " | ";
				alternative2->printTo(out);
				return out;
			}

		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				// create context copy for rollback
				MatchContext contextCopy(context);
				if(alternative1->match(context, tree)) return true;
				// restore context
				context = contextCopy;
				return alternative2->match(context, tree);
			}
		};

		// Negation
		class Negation : public TreePattern {
			const TreePatternPtr pattern;
		public:
			Negation(const TreePatternPtr& pattern) : pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				out << "!(";
				pattern->printTo(out);
				out << ")";
				return out;
			}

		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				return !pattern->match(context, tree);
			}
		};

		class Descendant : public TreePattern {
			const std::vector<TreePatternPtr> subPatterns;
		public:
			template<typename ... Patterns>
			Descendant(Patterns ... patterns) : subPatterns(toVector<TreePatternPtr>(patterns...)) {};

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "aT(" << join(",", subPatterns, print<id<TreePatternPtr>>()) << ")";
			}

		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const;
		};

	}

	namespace nodes {

		// The most simple node pattern covering a single tree
		class Single : public NodePattern {
			const TreePatternPtr element;
		public:
			Single(const TreePatternPtr& element) : element(element) {}
			virtual std::ostream& printTo(std::ostream& out) const {
				return element->printTo(out);
			}
		protected:
			virtual int match(MatchContext& context, const std::vector<TreePtr>& trees, int start) const {
				if (start < 0 || start >= static_cast<int>(trees.size())) {
					return -1;
				}
				if (!element->match(context, trees[start])) {
					return -1;
				}
				return start+1;
			}
		};

		// A sequence node pattern representing the composition of two node patterns
		class Sequence : public NodePattern {
			const NodePatternPtr left;
			const NodePatternPtr right;
		public:
			Sequence(const NodePatternPtr& left, const NodePatternPtr& right) : left(left), right(right) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				left->printTo(out);
				out << ",";
				right->printTo(out);
				return out;
			}
		protected:
			virtual int match(MatchContext& context, const std::vector<TreePtr>& trees, int start) const {
				// just concatenate the two patterns
				return right->match(context, trees, left->match(context, trees, start));
			}
		};

		// A node pattern alternative
		class Alternative : public NodePattern {
			const NodePatternPtr alternative1;
			const NodePatternPtr alternative2;
		public:
			Alternative(const NodePatternPtr& A, const NodePatternPtr& B) : alternative1(A), alternative2(B) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				alternative1->printTo(out);
				out << "|";
				alternative2->printTo(out);
				return out;
			}
		protected:
			virtual int match(MatchContext& context, const std::vector<TreePtr>& trees, int start) const {
				// create context copy for rollback
				MatchContext contextCopy(context);
				int res = alternative1->match(context, trees, start);
				if (res >= 0) {
					return res;
				}
				context = contextCopy;
				return alternative2->match(context, trees, start);
			}
		};

		// Realizes the star operator for node patterns
		class Repetition : public NodePattern {
			const NodePatternPtr pattern;
		public:
			Repetition(const NodePatternPtr& pattern) : pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				out << "[";
				pattern->printTo(out);
				out << "]*";
				return out;
			}

		protected:
			virtual int match(MatchContext& context, const std::vector<TreePtr>& trees, int start) const {
				int last = start;
				int cur = start;
				// match greedy
				while (cur != -1) {
					last = cur;
					// isolate context for each try
					MatchContext contextCopy(context);
					cur = pattern->match(contextCopy, trees, cur);
				}
				return last;
			}
		};

	}
	
	extern const TreePatternPtr any;
	extern const TreePatternPtr recurse;

	inline TreePatternPtr atom(const TreePtr& tree) {
		return std::make_shared<trees::Atom>(tree);
	}

	inline TreePatternPtr operator|(const TreePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<trees::Alternative>(a,b);
	}

	inline TreePatternPtr operator!(const TreePatternPtr& a) {
		return std::make_shared<trees::Negation>(a);
	}
	
	inline TreePatternPtr node(const NodePatternPtr& pattern) {
		return std::make_shared<trees::NodeTreePattern>(pattern);
	}
	inline TreePatternPtr node(const int id, const NodePatternPtr& pattern) {
		return std::make_shared<trees::NodeTreePattern>(id, pattern);
	}

	inline TreePatternPtr var(const std::string& name) {
		return std::make_shared<trees::Variable>(name);
	}

	template<typename ... Patterns>
	inline TreePatternPtr aT(Patterns ... patterns) {
		return std::make_shared<trees::Descendant>(patterns...);
	}

	inline TreePatternPtr rT(const TreePatternPtr& pattern) {
		return std::make_shared<trees::Recursion>(pattern);
	}
	inline TreePatternPtr rT(const NodePatternPtr& pattern) {
		return std::make_shared<trees::Recursion>(node(pattern));
	}
	
	inline NodePatternPtr single(const TreePatternPtr& pattern) {
		return std::make_shared<nodes::Single>(pattern);
	}
	inline NodePatternPtr single(const TreePtr& tree) {
		return single(atom(tree));
	}
	
	inline NodePatternPtr operator|(const NodePatternPtr& a, const NodePatternPtr& b) {
		return std::make_shared<nodes::Alternative>(a,b);
	}
	inline NodePatternPtr operator|(const TreePatternPtr& a, const NodePatternPtr& b) {
		return std::make_shared<nodes::Alternative>(single(a),b);
	}
	inline NodePatternPtr operator|(const NodePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<nodes::Alternative>(a,single(b));
	}

	inline NodePatternPtr operator*(const NodePatternPtr& pattern) {
		return std::make_shared<nodes::Repetition>(pattern);
	}
	inline NodePatternPtr operator*(const TreePatternPtr& pattern) {
		return std::make_shared<nodes::Repetition>(single(pattern));
	}

	inline NodePatternPtr operator<<(const NodePatternPtr& a, const NodePatternPtr& b) {
		return std::make_shared<nodes::Sequence>(a,b);
	}
	inline NodePatternPtr operator<<(const TreePatternPtr& a, const NodePatternPtr& b) {
		return std::make_shared<nodes::Sequence>(single(a),b);
	}
	inline NodePatternPtr operator<<(const NodePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<nodes::Sequence>(a,single(b));
	}
	inline NodePatternPtr operator<<(const TreePatternPtr& a, const TreePatternPtr& b) {
		return std::make_shared<nodes::Sequence>(single(a),single(b));
	}

	std::ostream& operator<<(std::ostream& out, const PatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const TreePatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const NodePatternPtr& pattern);


} // end namespace pattern
} // end namespace transform
} // end namespace insieme
