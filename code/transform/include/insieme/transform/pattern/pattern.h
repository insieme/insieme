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
#include "insieme/transform/pattern/match.h"

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

	class MatchContext : public utils::Printable {

		typedef std::unordered_map<string, TreeList> NodeVarMap;
		typedef std::unordered_map<string, TreePatternPtr> RecVarMap;

		MatchPath path;

		Match match;
		RecVarMap boundRecursiveVariables;

	public:

		MatchContext() { }

		const Match& getMatch() const {
			return match;
		}

		// -- The Match Path ---------------------------

		void push() {
			path.push(0);
		}

		void inc() {
			path.inc();
		}

		void pop() {
			path.pop();
		}


		// -- Tree Variables ---------------------------

		bool isTreeVarBound(const std::string& var) const {
			return match.isTreeVarBound(path, var);
		}

		void bindTreeVar(const std::string& var, const TreePtr value) {
			match.bindTreeVar(path, var, value);
		}

		TreePtr getTreeVarBinding(const std::string& var) const {
			return match.getTreeVarBinding(path, var);
		}

		// -- Node Variables --------------------------

		bool isNodeVarBound(const std::string& var) const {
			return match.isListVarBound(path, var);
		}

		void bindNodeVar(const std::string& var, const TreeListIterator& begin, const TreeListIterator& end) {
			match.bindListVar(path, var, begin, end);
		}

		TreeList getNodeVarBinding(const std::string& var) const {
			return match.getListVarBinding(path, var);
		}

		// -- Recursive Variables ---------------------------

		bool isRecVarBound(const std::string& var) const {
			return boundRecursiveVariables.find(var) != boundRecursiveVariables.end();
		}

		void bindRecVar(const std::string& var, const TreePatternPtr& pattern) {
			assert(!isRecVarBound(var) && "Variable bound twice");
			boundRecursiveVariables.insert(RecVarMap::value_type(var, pattern));
		}

		TreePatternPtr getRecVarBinding(const std::string& var) const {
			assert(isRecVarBound(var) && "Requesting bound value for unbound tree variable");
			return boundRecursiveVariables.find(var)->second;
		}

		void unbindRecVar(const std::string& var) {
			boundRecursiveVariables.erase(var);
		}


		virtual std::ostream& printTo(std::ostream& out) const;
	};


	class Pattern : public utils::Printable { };

	namespace nodes {
		class Single;
	}

	// The abstract base class for tree patterns
	class TreePattern : public Pattern {
		friend class nodes::Single;
//		const std::vector<FilterPtr> filter;
	public:

		MatchOpt match(const TreePtr& tree) const{
			MatchContext context;
			if (match(context, tree)) {
				return context.getMatch();
			}
			return 0;
		}

		virtual bool match(MatchContext& context, const TreePtr& tree) const =0;
	};

	namespace trees {
		class NodeTreePattern;
	}

	// An abstract base node for node patterns
	class NodePattern : public Pattern {
	public:

		bool match(const std::vector<TreePtr>& trees) const {
			MatchContext context;
			return match(context, trees);
		}

		bool match(MatchContext& context, const std::vector<TreePtr>& trees) const {
			return match(context, trees.begin(), trees.end());
		}
//	protected:
		virtual bool match(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const =0;
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


		// A simple variable
		class Variable : public TreePattern {
			const static TreePatternPtr any;
			const std::string name;
			const TreePatternPtr pattern;
		public:
			Variable(const std::string& name, const TreePatternPtr& pattern = any) : name(name), pattern(pattern) {}
			virtual std::ostream& printTo(std::ostream& out) const {
				out << "%" << name << "%";
				if(pattern && pattern != any) {
					out << ":" << *pattern;
				}
				return out;
			}
			const std::string& getName() {
				return name;
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {

				// check whether the variable is already bound
				if(context.isTreeVarBound(name)) {
					return *context.getTreeVarBinding(name) == *tree;
				}

				// check filter-pattern of this variable
				if (pattern->match(context, tree)) {
					context.bindTreeVar(name, tree);
					return true;
				}

				// tree is not a valid substitution for this variable
				return false;
			}
		};

		// Depth recursion (downward * operator)
		class Recursion : public TreePattern {
			const string name;
			bool terminal;
			const TreePatternPtr pattern;
		public:
			Recursion(const string& name) : name(name), terminal(true) {}
			Recursion(const string& name, const TreePatternPtr& pattern) : name(name), terminal(false), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(terminal) return out << "rec." << name;
				else {
					out << "rT." << name << "(";
					pattern->printTo(out);
					return out << ")";
				}
			}
		protected:
			virtual bool match(MatchContext& context, const TreePtr& tree) const {
				if(terminal) {
					assert(context.isRecVarBound(name) && "Recursive variable unbound!");
					return context.getRecVarBinding(name)->match(context, tree);
				} else {
					context.bindRecVar(name, pattern);
					bool res = pattern->match(context, tree);
					context.unbindRecVar(name);
					return res;
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
				return pattern->match(context, tree->getSubTrees());
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
				MatchContext copy(context);
				if(alternative1->match(context, tree)) return true;
				// restore context
				context = copy;
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
			virtual bool match(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const {
				// range has to be exactly one ...
				if (std::distance(begin, end) != 1) return false;
				// ... and the pattern has to match
				return element->match(context, *begin);
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
			virtual bool match(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const {
				// search for the split-point ...
				for(auto i = begin; i<=end; ++i) {
					MatchContext caseContext = context;
					if (left->match(caseContext, begin, i) && right->match(caseContext, i, end)) {
						context = caseContext; // make temporal context permanent
						return true;
					}
				}
				return false;
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
			virtual bool match(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const {
				// try both alternatives using a private context
				MatchContext copy(context);
				if (alternative1->match(copy, begin, end)) {
					// make temporal context permanent ..
					context = copy;
					return true;
				}
				return alternative2->match(context, begin, end);
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
			virtual bool match(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const {
				context.push();
				bool res = matchInternal(context, begin, end);
				context.pop();
				return res;
			}

		private:

			bool matchInternal(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const {
				// empty is accepted (terminal case)
				if (begin == end) {
					return true;
				}

				// test special case of a single iteration
				MatchContext copy = context;
				if (pattern->match(copy, begin, end)) {
					context = copy;
					return true;
				}

				// try one pattern + a recursive repedition
				for (auto i=begin; i<end; ++i) {

					// private copy for this try
					MatchContext copy = context;

					if (!pattern->match(copy, begin, i)) {
						// does not match ... try next!
						continue;
					}

					// increment repedition counter
					copy.inc();

					if (!matchInternal(copy, i, end)) {
						// does not fit any more ... try next!
						continue;
					}

					// found a match!
					context = copy;
					return true;
				}

				// the pattern does not match!
				return false;
			}

		};

		// A simple variable
		class Variable : public NodePattern {
			const static NodePatternPtr any;
			const std::string name;
			const NodePatternPtr pattern;
		public:
			Variable(const std::string& name, const NodePatternPtr& pattern = any)
				: name(name), pattern(pattern) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				out << "%" << name << "%";
				if(pattern && pattern != any) {
					out << ":" << *pattern;
				}
				return out;
			}
			const std::string& getName() {
				return name;
			}
		protected:
			virtual bool match(MatchContext& context, const TreeListIterator& begin, const TreeListIterator& end) const {

std::cout << "Matching: " << context << "\n";

				// check whether the variable is already bound
				if(context.isNodeVarBound(name)) {
					const TreeList& value = context.getNodeVarBinding(name);
					return std::distance(begin, end) == std::distance(value.begin(), value.end()) &&
						   std::equal(begin, end, value.begin(), equal_target<TreePtr>());
				}

				// check filter-pattern of this variable
				if (pattern->match(context, begin, end)) {
					context.bindNodeVar(name, begin, end);
					return true;
				}

				// tree is not a valid substitution for this variable
				return false;
			}
		};

	}
	
	extern const TreePatternPtr any;
	extern const TreePatternPtr recurse;

	extern const NodePatternPtr anyList;

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

	inline TreePatternPtr var(const std::string& name, const TreePatternPtr& pattern = any) {
		return std::make_shared<trees::Variable>(name, pattern);
	}

	inline TreePatternPtr treeVar(const std::string& name, const TreePatternPtr& pattern = any) {
		return std::make_shared<trees::Variable>(name, pattern);
	}

	inline NodePatternPtr nodeVar(const std::string& name, const NodePatternPtr& pattern = anyList) {
		return std::make_shared<nodes::Variable>(name, pattern);
	}

	template<typename ... Patterns>
	inline TreePatternPtr aT(Patterns ... patterns) {
		return std::make_shared<trees::Descendant>(patterns...);
	}

	inline TreePatternPtr rT(const TreePatternPtr& pattern, const string& varName = "x") {
		return std::make_shared<trees::Recursion>(varName, pattern);
	}
	inline TreePatternPtr rT(const NodePatternPtr& pattern, const string& varName = "x") {
		return std::make_shared<trees::Recursion>(varName, node(pattern));
	}
	inline TreePatternPtr rec(const string& varName) {
		return std::make_shared<trees::Recursion>(varName);
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

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::PatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::TreePatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::NodePatternPtr& pattern);

} // end namespace std
