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

#include "insieme/core/ir.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace transform {
namespace pattern {

	namespace details {

		bool isTypeOrValueOrParam(const core::NodeType type) {
			return core::isA<core::NC_Type>(type) ||
					core::isA<core::NC_IntTypeParam>(type) ||
					core::isA<core::NC_Value>(type);
		}

		/**
		 * This function is implementing the actual matching algorithm.
		 */
		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const TreePattern& pattern, const T& node);

		/**
		 * This function is implementing the actual matching algorithm for list patterns.
		 */
		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const ListPattern& pattern, const std::vector<T>& trees);

	}

	MatchOpt TreePattern::matchPointer(const core::NodePtr& node) const {
		return details::match(*this, node);
	}

	AddressMatchOpt TreePattern::matchAddress(const core::NodeAddress& node) const {
		return details::match(*this, node);
	}

	TreeMatchOpt TreePattern::matchTree(const TreePtr& tree) const {
		return details::match(*this, tree);
	}

	TreeMatchOpt ListPattern::match(const vector<TreePtr>& trees) const {
		return details::match(*this, trees);
	}


	const TreePatternPtr any = std::make_shared<tree::Wildcard>();
	const TreePatternPtr recurse = std::make_shared<tree::Recursion>("x");

	const ListPatternPtr anyList = *any;
	const ListPatternPtr empty = std::make_shared<list::Empty>();

	namespace tree {

		const TreePatternPtr Variable::any = pattern::any;

	}

	namespace list {

		const ListPatternPtr Variable::any = pattern::anyList;
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

			void clearCache() {
				treePatternCache->clear();
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
		bool match(const TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree, const std::function<bool(MatchContext<T>&)>& delayedCheck);

		template<typename T, typename iterator = typename T::value_iterator>
		bool match(const ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end, const std::function<bool(MatchContext<T>&)>& delayedCheck);

		template<typename T>
		inline bool match(const TreePatternPtr& pattern, MatchContext<T>& context, const typename T::value_type& tree, const std::function<bool(MatchContext<T>&)>& delayedCheck) {
			return match(*pattern.get(), context, tree, delayedCheck);
		}

		template<typename T, typename iterator = typename T::value_iterator>
		inline bool match(const ListPatternPtr& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end, const std::function<bool(MatchContext<T>&)>& delayedCheck) {
			return match(*pattern.get(), context, begin, end, delayedCheck);
		}


		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const TreePattern& pattern, const T& tree) {
			MatchContext<target> context;
			std::function<bool(MatchContext<target>&)> accept = [](MatchContext<target>& context)->bool { return true; };
			if (match(pattern, context, tree, accept)) {
				// complete match result
				context.getMatch().setRoot(tree);
				return context.getMatch();
			}
			return 0;
		}

		template<typename T, typename target = typename match_target_info<T>::target_type>
		boost::optional<Match<target>> match(const ListPattern& pattern, const std::vector<T>& trees) {
			MatchContext<target> context;
			std::function<bool(MatchContext<target>&)> accept = [](MatchContext<target>& context)->bool { return true; };
			if (match(pattern, context, trees.begin(), trees.end(), accept)) {
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
			bool match ## NAME (const pattern::tree::NAME& pattern, MatchContext<T>& context, const typename T::value_type& tree, const std::function<bool(MatchContext<T>&)>& delayedCheck)

			MATCH(Value) {
				return tree->isValue() && tree->getNodeValue() == pattern.value && delayedCheck(context);
			}

			MATCH(Constant) {
				assert(pattern.nodeAtom && "Wrong type of constant value stored within atom node!");
				return *pattern.nodeAtom == *tree && delayedCheck(context);
			}

			MATCH(Wildcard) {
				return delayedCheck(context);	// just finish delayed checks
			}

			MATCH(Variable) {

				// check whether the variable is already bound
				if(context.isTreeVarBound(pattern.name)) {
					return *context.getTreeVarBinding(pattern.name) == *tree && delayedCheck(context);
				}

				// check filter-pattern of this variable
				context.bindTreeVar(pattern.name, tree);		// speculate => bind variable
				if (match(pattern.pattern, context, tree, delayedCheck)) {
					return true;
				}

				// tree is not a valid substitution for this variable
				return false;
			}

			namespace {

				bool contains_variable_free(MatchContext<ptr_target>& context, const core::NodePtr& tree, const TreePatternPtr& pattern, const std::function<bool(MatchContext<ptr_target>&)>& delayedCheck) {
					return core::visitDepthFirstOnceInterruptible(tree, [&](const core::NodePtr& cur)->bool {
						return match(pattern, context, cur, delayedCheck);
					}, pattern->mayBeType);
				}

				bool contains_variable_free(MatchContext<address_target>& context, const core::NodeAddress& tree, const TreePatternPtr& pattern, const std::function<bool(MatchContext<address_target>&)>& delayedCheck) {
					return core::visitDepthFirstOnceInterruptible(tree.as<core::NodePtr>(), [&](const core::NodePtr& cur)->bool {
						return match(pattern, context, core::NodeAddress(cur), delayedCheck);
					}, pattern->mayBeType);
				}

				bool contains_variable_free(MatchContext<tree_target>& context, const TreePtr& tree, const TreePatternPtr& pattern, const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
					bool res = false;
					res = res || match(pattern, context, tree, delayedCheck);
					for_each(tree->getChildList(), [&](const TreePtr& cur) { // generalize this
						res = res || contains_variable_free(context, cur, pattern, delayedCheck);
					});
					return res;
				}
			}

			template<typename T>
			bool contains(MatchContext<T>& context, const typename T::value_type& tree, const TreePatternPtr& pattern, const std::function<bool(MatchContext<T>&)>& delayedCheck) {

				// prune types
				if (!pattern->mayBeType && isTypeOrValueOrParam(tree)) return false;

				// if variable free, only non-shared nodes need to be checked
				if (pattern->isVariableFree) {
					return contains_variable_free(context, tree, pattern, delayedCheck);
				}

				// if there are variables, all nodes need to be checked
				bool res = false;

				// isolate context for each try
				MatchContext<T> backup = context;
				res = res || match(pattern, context, tree, delayedCheck);
				for_each(tree->getChildList(), [&](const typename T::value_type& cur) { // generalize this
					if (!res) context = backup;		// restore context
					res = res || contains(context, cur, pattern, delayedCheck);
				});
				return res;
			}

			MATCH(Descendant) {
				// search for all patterns occurring in the sub-trees
				return all(pattern.subPatterns, [&](const TreePatternPtr& cur) {
					return contains(context, tree, cur, delayedCheck);
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
					bool res = match(context.getRecVarBinding(pattern.name), context, tree, delayedCheck);

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
				bool res = match(pattern.pattern, context, tree, delayedCheck);
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
				return match(pattern.pattern, context, children.begin(), children.end(), delayedCheck);
			}

			MATCH(Negation) {
				return !match(pattern.pattern, context, tree, delayedCheck);
			}

			MATCH(Conjunction) {
				// match first and delay matching of second half
				std::function<bool(MatchContext<T>&)> delayed = [&](MatchContext<T>& context) { return match(pattern.pattern2, context, tree, delayedCheck); };
				return match(pattern.pattern1, context, tree, delayed);
			}

			MATCH(Disjunction) {
				// create context copy for rollback
				MatchContext<T> copy(context);
				if (match(pattern.pattern1, context, tree, delayedCheck)) return true;
				// restore context
				context = copy;
				return match(pattern.pattern2, context, tree, delayedCheck);
			}

			// -- for test structure only --

			// a specialization for tree pointers
			inline bool matchValue(const pattern::tree::Value& pattern, MatchContext<tree_target>& context, const TreePtr& tree, const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
				return false;
			}

			// a specialization for tree pointers
			inline bool matchConstant(const pattern::tree::Constant& pattern, MatchContext<tree_target>& context, const TreePtr& tree, const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
				assert(pattern.treeAtom && "Wrong type of constant value stored within atom node!");
				return *pattern.treeAtom == *tree && delayedCheck(context);
			}

			// a specialization for tree pointers
			inline bool matchNode(const pattern::tree::Node& pattern, MatchContext<tree_target>& context, const TreePtr& tree, const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
				if (pattern.id != -1 && pattern.id != tree->getId()) return false;
				auto& children = tree->getSubTrees();
				return match(pattern.pattern, context, children.begin(), children.end(), delayedCheck) && delayedCheck(context);
			}


		#undef MATCH

		} // end namespace tree

		namespace list {

		// Empty, Single, Variable, Alternative, Sequence, Repedition

		#define MATCH(NAME) \
			template<typename T, typename iterator = typename T::value_iterator> \
			bool match ## NAME (const pattern::list::NAME& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end, const std::function<bool(MatchContext<T>&)>& delayedCheck)

			MATCH(Empty) {
				// only accepts empty list
				return begin == end && delayedCheck(context);
			}

			MATCH(Single) {
				// range has to be exactly one ...
				if (std::distance(begin, end) != 1) return false;
				// ... and the pattern has to match
				return match(pattern.element, context, *begin, delayedCheck) && delayedCheck(context);
			}

			MATCH(Variable) {

				// check whether the variable is already bound
				if(context.isNodeVarBound(pattern.name)) {
					const auto& value = context.getNodeVarBinding(pattern.name);
					return std::distance(begin, end) == std::distance(value.begin(), value.end()) &&
						   std::equal(begin, end, value.begin(), equal_target<typename T::value_type>()) &&
						   delayedCheck(context);
				}

				// check filter-pattern of this variable
				context.bindNodeVar(pattern.name, begin, end);	// speculate => bind variable
				if (match(pattern.pattern, context, begin, end, delayedCheck)) {
					return true;
				}

				// tree is not a valid substitution for this variable
				return false;
			}

			MATCH(Sequence) {
				// search for the split-point ...
				for(auto i = begin; i<=end; ++i) {
					MatchContext<T> caseContext = context;
					// check left side and delay right side
					std::function<bool(MatchContext<T>&)> delayed = [&](MatchContext<T>& context) { return match(pattern.right, caseContext, i, end, delayedCheck); };
					if (match(pattern.left, caseContext, begin, i, delayed)) {
						context = caseContext; // make temporal context permanent
						return true;
					}
				}
				return false;
			}

			MATCH(Alternative) {
				// try both alternatives using a private context
				MatchContext<T> copy(context);
				if (match(pattern.alternative1, copy, begin, end, delayedCheck)) {
					// make temporal context permanent ..
					context = copy;
					return true;
				}
				return match(pattern.alternative2, context, begin, end, delayedCheck);
			}

			template<typename T, typename iterator = typename T::value_iterator>
			bool matchRepetitionInternal(
					const pattern::list::Repetition& rep, MatchContext<T>& context,
					const iterator& begin, const iterator& end, unsigned repetitions,
					const std::function<bool(MatchContext<T>&)>& delayedCheck) {

				// empty is accepted (terminal case)
				if (begin == end) {
					return repetitions >= rep.minRep;
				}

				// test special case of a single iteration
				MatchContext<T> copy = context;
				if (rep.minRep <= 1 && match(rep.pattern, copy, begin, end, delayedCheck)) {
					context = copy;
					return true;
				}

				// try one pattern + a recursive repetition
				for (auto i=begin; i<end; ++i) {

					// private copy for this try
					MatchContext<T> copy = context;

					if (!match(rep.pattern, copy, begin, i, delayedCheck)) {
						// does not match ... try next!
						continue;
					}

					// increment repetition counter
					copy.inc();

					if (!matchRepetitionInternal(rep, copy, i, end, repetitions + 1, delayedCheck)) {
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
				bool res = matchRepetitionInternal(pattern, context, begin, end, 0, delayedCheck);
				context.pop();
				return res;
			}

		#undef MATCH

		} // end namespace list

		namespace {

			template<typename T>
			bool match_internal(const TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree, const std::function<bool(MatchContext<T>&)>& delayedCheck) {
				switch(pattern.type) {
					#define CASE(NAME) case TreePattern::NAME : return tree::match ## NAME (static_cast<const pattern::tree::NAME&>(pattern), context, tree, delayedCheck)
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
		bool match(const TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree, const std::function<bool(MatchContext<T>&)>& delayedCheck) {

			const bool DEBUG = false;

			if (DEBUG) std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... \n";

			// skip searching within types if not searching for a type
			if (!pattern.mayBeType && isTypeOrValueOrParam(tree)) return false;

			// use cache if possible
			if (pattern.isVariableFree) {
				CachedMatchResult cachRes = context.cachedMatch(pattern, tree);
				if (cachRes != Unknown) {
					bool res = cachRes == Yes && delayedCheck(context);
					if (DEBUG) std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... - from cache: " << res << "\n";
					return res;
				}

				// resolve without delayed checks and save result
				std::function<bool(MatchContext<T>&)> accept = [](MatchContext<T>& context) { return true; };
				bool res = match_internal(pattern, context, tree, accept);
				context.addToCache(pattern, tree, res);

				// return result + delayed checks
				res = res && delayedCheck(context);
				if (DEBUG) std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... - added to cache: " << res << "\n";
				return res;
			}

			// for all the rest, use non-cached inner implementation
			bool res = match_internal(pattern, context, tree, delayedCheck);
			if (DEBUG) std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... - matched: " << res << "\n";
			return res;
		}


		template<typename T, typename iterator = typename T::value_iterator>
		bool match(const ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end, const std::function<bool(MatchContext<T>&)>& delayedCheck) {
			switch(pattern.type) {
				#define CASE(NAME) case ListPattern::NAME : return list::match ## NAME (static_cast<const pattern::list::NAME&>(pattern), context, begin, end, delayedCheck)
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

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::PatternPtr& pattern) {
		return (pattern)?(pattern->printTo(out)):(out << "null");
	}

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::TreePatternPtr& pattern) {
		return (pattern)?(pattern->printTo(out)):(out << "null");
	}

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::ListPatternPtr& pattern) {
		return (pattern)?(pattern->printTo(out)):(out << "null");
	}

} // end namespace std
