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

		/**
		 * This function is implementing the actual matching algorithm.
		 */
		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const TreePattern& pattern, const T& node);

		bool isTypeOrValueOrParam(const core::NodeType type);
		inline bool isTypeOrValueOrParam(const int type) { return false; }

		inline bool isTypeOrValueOrParam(const core::NodeAddress node) {
			return isTypeOrValueOrParam(node->getNodeType());
		}

		inline bool isTypeOrValueOrParam(const TreePtr&) { return false; }

	}

	typedef boost::optional<Match<ptr_target>> MatchOpt;
	typedef boost::optional<Match<address_target>> AddressMatchOpt;


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

		// TODO: add filter support
//		const std::vector<FilterPtr> filter;


		TreePattern(const Type type, bool isVariableFree, bool mayBeType = true) : Pattern(isVariableFree), type(type), mayBeType(mayBeType) {};

		bool match(const core::NodePtr& node) const {
			return matchPointer(node);
		}

		MatchOpt matchPointer(const core::NodePtr& node) const {
			return details::match(*this, node);
		}

		AddressMatchOpt matchAddress(const core::NodeAddress& node) const {
			return details::match(*this, node);
		}
	};


	// An abstract base node for node patterns
	struct ListPattern : public Pattern {

		enum Type {
			Empty, Single, Variable, Alternative, Sequence, Repetition
		};

		const Type type;

		ListPattern(const Type type, bool isVariableFree) : Pattern(isVariableFree), type(type) {};

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
	inline TreePatternPtr node(const core::NodeType type, const ListPatternPtr& pattern = empty) {
		return std::make_shared<tree::Node>(type, pattern);
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
	
	inline ListPatternPtr single(const TreePatternPtr& pattern) {
		return std::make_shared<list::Single>(pattern);
	}
	inline ListPatternPtr single(const TreePtr& tree) {
		return single(atom(tree));
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

	inline TreePatternPtr outermost(const TreePatternPtr& a) {
		// should be:
		// 	return rT(a | !aT(a) | (!a & node(*recurse)));
		// but & operator is not implemented.

		// also works (since | is evaluated left-to-right)
		return rT(a | !aT(a) | node(*recurse));
	}

	inline TreePatternPtr step(const TreePatternPtr& a) {
		return node(anyList << a << anyList);
	}




	// -------------------------------------------------------------------------------------
	//   Pattern Matcher
	// -------------------------------------------------------------------------------------


	// -- Implementation detail -------------------------------------------

	namespace details {


		enum CachedMatchResult {
			Yes, No, Unknown
		};


		template<typename T>
		class MatchContext : public utils::Printable {

		public:
			typedef typename T::value_type value_type;
			typedef typename T::atom_type atom_type;
			typedef typename T::list_type list_type;
			typedef typename T::list_iterator iterator;

			// a cache for tree patterns not including variables
			typedef std::map<std::pair<const TreePattern*, typename T::atom_type>, bool> tree_pattern_cache;

		private:
			struct RecVarInfo {
				TreePatternPtr pattern;
				unsigned level;
				unsigned counter;

				RecVarInfo(TreePatternPtr pattern, unsigned level)
					: pattern(pattern), level(level), counter(0) {}
			};

			typedef std::unordered_map<string, RecVarInfo> RecVarMap;

			MatchPath path;

			Match<T> match;

			RecVarMap boundRecursiveVariables;

			std::shared_ptr<tree_pattern_cache> treePatternCache;

		public:

			MatchContext() : treePatternCache(std::make_shared<tree_pattern_cache>()) { }

			Match<T>& getMatch() {
				return match;
			}

			const Match<T>& getMatch() const {
				return match;
			}

			// -- The Match Path ---------------------------

			void push() {
				path.push(0);
			}

			void inc() {
				path.inc();
			}

			void dec() {
				path.dec();
			}

			void pop() {
				path.pop();
			}

			void set(std::size_t index) {
				path.set(index);
			}

			const MatchPath& getCurrentPath() const {
				return path;
			}

			void setCurrentPath(const MatchPath& newPath) {
				path = newPath;
			}

			// -- Tree Variables ---------------------------

			bool isTreeVarBound(const std::string& var) const {
				return match.isTreeVarBound(path, var);
			}

			void bindTreeVar(const std::string& var, const value_type& value) {
				match.bindTreeVar(path, var, value);
			}

			const value_type& getTreeVarBinding(const std::string& var) const {
				return match.getTreeVarBinding(path, var);
			}

			// -- Node Variables --------------------------

			bool isNodeVarBound(const std::string& var) const {
				return match.isListVarBound(path, var);
			}

			void bindNodeVar(const std::string& var, const iterator& begin, const iterator& end) {
				match.bindListVar(path, var, begin, end);
			}

			list_type getNodeVarBinding(const std::string& var) const {
				return match.getListVarBinding(path, var);
			}

			// -- Recursive Variables ---------------------------

			bool isRecVarBound(const std::string& var) const {
				return boundRecursiveVariables.find(var) != boundRecursiveVariables.end();
			}

			void bindRecVar(const std::string& var, const TreePatternPtr& pattern) {
				assert(!isRecVarBound(var) && "Variable bound twice");
				boundRecursiveVariables.insert(std::make_pair(var, RecVarInfo(pattern, path.getDepth())));
			}

			TreePatternPtr getRecVarBinding(const std::string& var) const {
				assert(isRecVarBound(var) && "Requesting bound value for unbound tree variable");
				return boundRecursiveVariables.find(var)->second.pattern;
			}

			unsigned getRecVarDepth(const std::string& var) const {
				assert(isRecVarBound(var) && "Requesting bound value for unbound tree variable");
				return boundRecursiveVariables.find(var)->second.level;
			}

			unsigned getRecVarCounter(const std::string& var) const {
				assert(isRecVarBound(var) && "Requesting bound value for unbound tree variable");
				return boundRecursiveVariables.find(var)->second.counter;
			}

			unsigned incRecVarCounter(const std::string& var) {
				assert(isRecVarBound(var) && "Requesting bound value for unbound tree variable");
				return ++(boundRecursiveVariables.find(var)->second.counter);
			}

			void unbindRecVar(const std::string& var) {
				boundRecursiveVariables.erase(var);
			}

			// -- Cached Match Results -----------------------------

			CachedMatchResult cachedMatch(const TreePattern& pattern, const atom_type& node) const {
				assert(pattern.isVariableFree && "Can only cache variable-free pattern fragments!");

				auto pos = treePatternCache->find(std::make_pair(&pattern, node));
				if (pos == treePatternCache->end()) {
					return Unknown;
				}
				return (pos->second)?Yes:No;
			}

			void addToCache(const TreePattern& pattern, const value_type& node, bool match) {
				(*treePatternCache)[std::make_pair(&pattern, node)] = match;
			}


			virtual std::ostream& printTo(std::ostream& out) const {
				out << "Match(";
				out << path << ", ";
				out << match << ", ";
				out << "{" << join(",", boundRecursiveVariables,
						[](std::ostream& out, const std::pair<string, RecVarInfo>& cur) {
							out << cur.first << "=" << cur.second.pattern;
				}) << "}";
				return out << ")";
			}
		};



		template<typename T>
		bool match(const TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree);

		template<typename T, typename iterator = typename T::value_iterator>
		bool match(const ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end);

		template<typename T>
		inline bool match(const TreePatternPtr& pattern, MatchContext<T>& context, const typename T::value_type& tree) {
			return match(*pattern.get(), context, tree);
		}

		template<typename T, typename iterator = typename T::value_iterator>
		inline bool match(const ListPatternPtr& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end) {
			return match(*pattern.get(), context, begin, end);
		}


		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const TreePattern& pattern, const T& tree) {
			MatchContext<target> context;
			if (match(pattern, context, tree)) {
				// complete match result
				context.getMatch().setRoot(tree);
				return context.getMatch();
			}
			return 0;
		}

		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const ListPattern& pattern, const std::vector<T>& trees) {
			MatchContext<target> context;
			if (match(pattern, context, trees.begin(), trees.end())) {
				// => it is a match (but leaf root empty)
				return context.getMatch();
			}
			return 0;
		}

		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const TreePatternPtr& pattern, const T& tree) {
			return match(*pattern.get(), tree);
		}

		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const ListPatternPtr& pattern, const std::vector<T>& trees) {
			return match(*pattern.get(), trees);
		}


		// -- Match Tree Patterns -------------------------------------------------------

		// Atom, Variable, Wildcard, Node, Negation, Alternative, Descendant, Recursion

		namespace tree {

		#define MATCH(NAME) \
			template<typename T> \
			bool match ## NAME (const pattern::tree::NAME& pattern, MatchContext<T>& context, const typename T::value_type& tree)

			MATCH(Value) {
				return tree->isValue() && tree->getNodeValue() == pattern.value;
			}

			MATCH(Constant) {
				assert(pattern.nodeAtom && "Wrong type of constant value stored within atom node!");
				return *pattern.nodeAtom == *tree;
			}

			MATCH(Wildcard) {
				return true;
			}

			MATCH(Variable) {

				// check whether the variable is already bound
				if(context.isTreeVarBound(pattern.name)) {
					return *context.getTreeVarBinding(pattern.name) == *tree;
				}

				// check filter-pattern of this variable
				if (match(pattern.pattern, context, tree)) {
					context.bindTreeVar(pattern.name, tree);
					return true;
				}

				// tree is not a valid substitution for this variable
				return false;
			}

			namespace {

				bool contains_variable_free(MatchContext<ptr_target>& context, const core::NodePtr& tree, const TreePatternPtr& pattern) {
					return core::visitDepthFirstOnceInterruptible(tree, [&](const core::NodePtr& cur)->bool {
						return match(pattern, context, cur);
					}, pattern->mayBeType);
				}

				bool contains_variable_free(MatchContext<address_target>& context, const core::NodeAddress& tree, const TreePatternPtr& pattern) {
					return core::visitDepthFirstOnceInterruptible(tree.as<core::NodePtr>(), [&](const core::NodePtr& cur)->bool {
						return match(pattern, context, core::NodeAddress(cur));
					}, pattern->mayBeType);
				}

				bool contains_variable_free(MatchContext<tree_target>& context, const TreePtr& tree, const TreePatternPtr& pattern) {
					bool res = false;
					res = res || match(pattern, context, tree);
					for_each(tree->getChildList(), [&](const TreePtr& cur) { // generalize this
						res = res || contains_variable_free(context, cur, pattern);
					});
					return res;
				}
			}

			template<typename T>
			bool contains(MatchContext<T>& context, const typename T::value_type& tree, const TreePatternPtr& pattern) {

				// prune types
				if (!pattern->mayBeType && isTypeOrValueOrParam(tree)) return false;

				// if variable free, only non-shared nodes need to be checked
				if (pattern->isVariableFree) {
					return contains_variable_free(context, tree, pattern);
				}

				// if there are variables, all nodes need to be checked
				bool res = false;
				// isolate context for each try
				res = res || match(pattern, context, tree);
				for_each(tree->getChildList(), [&](const typename T::value_type& cur) { // generalize this
					res = res || contains(context, cur, pattern);
				});
				return res;
			}

			MATCH(Descendant) {
				// search for all patterns occurring in the sub-trees
				return all(pattern.subPatterns, [&](const TreePatternPtr& cur) {
					return contains(context, tree, cur);
				});
			}

			MATCH(Recursion) {
				// handle terminal
				if(pattern.terminal) {
					// get pattern bound to recursive variable
					assert(context.isRecVarBound(pattern.name) && "Recursive variable unbound!");

					// save current context path
					MatchPath path = context.getCurrentPath();

					// restore recursion level
					unsigned recLevel = context.getRecVarDepth(pattern.name);
					while(context.getCurrentPath().getDepth() > recLevel) {
						context.pop();
					}

					// update number of recursion applications
					context.set(context.incRecVarCounter(pattern.name));

					// run match again
					bool res = match(context.getRecVarBinding(pattern.name), context, tree);

					// restore current context path
					context.setCurrentPath(path);
					return res;
				}

				// start of recursion => bind recursive variable and handle context
				context.push();

				// safe current value of the recursive variable
				TreePatternPtr oldValue;
				if (context.isRecVarBound(pattern.name)) {
					oldValue = context.getRecVarBinding(pattern.name);
					context.unbindRecVar(pattern.name);
				}

				// match using new rec-var binding
				context.bindRecVar(pattern.name, pattern.pattern);
				bool res = match(pattern.pattern, context, tree);
				context.unbindRecVar(pattern.name);

				// restore old recursive variable
				if (oldValue) {
					context.bindRecVar(pattern.name, oldValue);
				}

				context.pop();
				return res;
			}

			MATCH(Node) {
				if (pattern.type != -1 && pattern.type != tree->getNodeType()) return false;
				const auto& children = tree->getChildList();
				return match(pattern.pattern, context, children.begin(), children.end());
			}

			MATCH(Negation) {
				return !match(pattern.pattern, context, tree);
			}

			MATCH(Conjunction) {
				// create context copy for rollback
				MatchContext<T> copy(context);
				if (!match(pattern.pattern1, context, tree)) return false;
				// restore context
				context = copy;
				return match(pattern.pattern2, context, tree);
			}

			MATCH(Disjunction) {
				// create context copy for rollback
				MatchContext<T> copy(context);
				if (match(pattern.pattern1, context, tree)) return true;
				// restore context
				context = copy;
				return match(pattern.pattern2, context, tree);
			}

			// -- for test structure only --

			// a specialization for tree pointers
			inline bool matchValue(const pattern::tree::Value& pattern, MatchContext<tree_target>& context, const TreePtr& tree) {
				return false;
			}

			// a specialization for tree pointers
			inline bool matchConstant(const pattern::tree::Constant& pattern, MatchContext<tree_target>& context, const TreePtr& tree) {
				assert(pattern.treeAtom && "Wrong type of constant value stored within atom node!");
				return *pattern.treeAtom == *tree;
			}

			// a specialization for tree pointers
			inline bool matchNode(const pattern::tree::Node& pattern, MatchContext<tree_target>& context, const TreePtr& tree) {
				if (pattern.id != -1 && pattern.id != tree->getId()) return false;
				auto& children = tree->getSubTrees();
				return match(pattern.pattern, context, children.begin(), children.end());
			}


		#undef MATCH

		} // end namespace tree

		namespace list {

		// Empty, Single, Variable, Alternative, Sequence, Repedition

		#define MATCH(NAME) \
			template<typename T, typename iterator = typename T::value_iterator> \
			bool match ## NAME (const pattern::list::NAME& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end)

			MATCH(Empty) {
				// only accepts empty list
				return begin == end;
			}

			MATCH(Single) {
				// range has to be exactly one ...
				if (std::distance(begin, end) != 1) return false;
				// ... and the pattern has to match
				return match(pattern.element, context, *begin);
			}

			MATCH(Variable) {

				// check whether the variable is already bound
				if(context.isNodeVarBound(pattern.name)) {
					const auto& value = context.getNodeVarBinding(pattern.name);
					return std::distance(begin, end) == std::distance(value.begin(), value.end()) &&
						   std::equal(begin, end, value.begin(), equal_target<typename T::value_type>());
				}

				// check filter-pattern of this variable
				if (match(pattern.pattern, context, begin, end)) {
					context.bindNodeVar(pattern.name, begin, end);
					return true;
				}

				// tree is not a valid substitution for this variable
				return false;
			}

			MATCH(Sequence) {
				// search for the split-point ...
				for(auto i = begin; i<=end; ++i) {
					MatchContext<T> caseContext = context;
					if (match(pattern.left, caseContext, begin,i) && match(pattern.right, caseContext, i,end)) {
						context = caseContext; // make temporal context permanent
						return true;
					}
				}
				return false;
			}

			MATCH(Alternative) {
				// try both alternatives using a private context
				MatchContext<T> copy(context);
				if (match(pattern.alternative1, copy, begin, end)) {
					// make temporal context permanent ..
					context = copy;
					return true;
				}
				return match(pattern.alternative2, context, begin, end);
			}

			template<typename T, typename iterator = typename T::value_iterator>
			bool matchRepetitionInternal(
					const pattern::list::Repetition& rep, MatchContext<T>& context,
					const iterator& begin, const iterator& end, unsigned repetitions) {

				// empty is accepted (terminal case)
				if (begin == end) {
					return repetitions >= rep.minRep;
				}

				// test special case of a single iteration
				MatchContext<T> copy = context;
				if (rep.minRep <= 1 && match(rep.pattern, copy, begin, end)) {
					context = copy;
					return true;
				}

				// try one pattern + a recursive repetition
				for (auto i=begin; i<end; ++i) {

					// private copy for this try
					MatchContext<T> copy = context;

					if (!match(rep.pattern, copy, begin, i)) {
						// does not match ... try next!
						continue;
					}

					// increment repetition counter
					copy.inc();

					if (!matchRepetitionInternal(rep, copy, i, end, repetitions + 1)) {
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

			MATCH(Repetition) {
				context.push();
				bool res = matchRepetitionInternal(pattern, context, begin, end, 0);
				context.pop();
				return res;
			}

		#undef MATCH

		} // end namespace list

		namespace {

			template<typename T>
			bool match_internal(const TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree) {
				switch(pattern.type) {
					#define CASE(NAME) case TreePattern::NAME : return tree::match ## NAME (static_cast<const pattern::tree::NAME&>(pattern), context, tree)
						CASE(Value);
						CASE(Constant);
						CASE(Variable);
						CASE(Wildcard);
						CASE(Node);
						CASE(Negation);
						CASE(Conjunction);
						CASE(Disjunction);
						CASE(Descendant);
						CASE(Recursion);
					#undef CASE
				}
				assert(false && "Missed a pattern type!");
				return false;
			}
		}

		template<typename T>
		bool match(const TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree) {
			// skip searching within types if not searching for a type
			if (!pattern.mayBeType && isTypeOrValueOrParam(tree)) return false;

			// use cache if possible
			if (pattern.isVariableFree) {
				CachedMatchResult cachRes = context.cachedMatch(pattern, tree);
				if (cachRes != Unknown) {
					return cachRes == Yes;
				}

				// resolve and save result
				bool res = match_internal(pattern, context, tree);
				context.addToCache(pattern, tree, res);
				return res;
			}

			// for all the rest, use non-cached inner implementation
			return match_internal(pattern, context, tree);

		}


		template<typename T, typename iterator = typename T::value_iterator>
		bool match(const ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end) {
			switch(pattern.type) {
				#define CASE(NAME) case ListPattern::NAME : return list::match ## NAME (static_cast<const pattern::list::NAME&>(pattern), context, begin, end)
					CASE(Empty);
					CASE(Single);
					CASE(Variable);
					CASE(Alternative);
					CASE(Sequence);
					CASE(Repetition);
				#undef CASE
			}
			assert(false && "Missed a pattern type!");
			return false;
		}

	} // end namespace details


} // end namespace pattern
} // end namespace transform
} // end namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::PatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::TreePatternPtr& pattern);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::ListPatternPtr& pattern);

} // end namespace std
