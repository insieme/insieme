/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <algorithm>

#include "insieme/core/pattern/pattern.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/unused.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/math.h"

namespace insieme {

	// a translation-unit specific handling of the hashing of node pointers and addresses
	namespace core {

		size_t hash_value(const NodePtr& node) {
			return (size_t)(node.ptr);
		}

		size_t hash_value(const NodeAddress& address) {
			return address.hash();
		}

	} // end namespace core


	namespace core {
	namespace pattern {


		// -------------------------------------------------------------------------------------
		//   Pattern Node Implementation
		// -------------------------------------------------------------------------------------

		// TODO: clean up this
		namespace details {

			bool isTypeOrValueOrParam(const core::NodeType type);
			inline bool isTypeOrValueOrParam(const int type) {
				return false;
			}

			inline bool isTypeOrValueOrParam(const core::NodeAddress node) {
				return isTypeOrValueOrParam(node->getNodeType());
			}

			inline bool isTypeOrValueOrParam(const TreePtr&) {
				return false;
			}
		}

		namespace impl {

			/**
			 * A common base class for tree and list patterns.
			 */
			struct Pattern : public utils::VirtualPrintable {
				/**
				 * A flag indicating that this pattern does not contain any variables.
				 */
				bool isVariableFree;

				Pattern(bool isVariableFree) : isVariableFree(isVariableFree) {}

				virtual ~Pattern() {}
			};


			// The abstract base class for tree patterns
			struct TreePattern : public Pattern {
				// a list of all types of tree pattern constructs
				enum Type { Value, Constant, LazyConstant, Variable, Wildcard, Node, Negation, Conjunction, Disjunction, Descendant, Recursion, Lambda };

				/**
				 * The type of this tree pattern
				 */
				const Type type;

				/**
				 * A flag indicating whether the matched tree may be a type or a type parameter.
				 */
				bool mayBeType;

				TreePattern(const Type type, bool isVariableFree, bool mayBeType = true) : Pattern(isVariableFree), type(type), mayBeType(mayBeType){};

				bool match(const core::NodePtr& node) const {
					return matchPointer(node);
				}

				MatchOpt matchPointer(const core::NodePtr& node) const;

				AddressMatchOpt matchAddress(const core::NodeAddress& node) const;

				TreeMatchOpt matchTree(const TreePtr& tree) const;
			};


			// An abstract base node for node patterns
			struct ListPattern : public Pattern {
				enum Type { Empty, Single, Variable, Alternative, Sequence, Repetition };

				const Type type;

				unsigned minLength;
				unsigned maxLength;

				ListPattern(const Type type, bool isVariableFree, unsigned min, unsigned max = std::numeric_limits<unsigned>::max())
				    : Pattern(isVariableFree), type(type), minLength(min), maxLength(max){};

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
						if(nodeAtom) { return out << *nodeAtom; }
						return out << *treeAtom;
					}
				};

				// A constant value - that is lazily evaluated
				struct LazyConstant : public TreePattern {
					typedef std::function<core::NodePtr(core::NodeManager&)> factory_type;

					factory_type factory;

					LazyConstant(const factory_type& factory) : TreePattern(TreePattern::LazyConstant, true, isType(factory)), factory(factory) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						core::NodeManager mgr;
						return out << *factory(mgr);
					}

				  private:
					static bool isType(const factory_type& factory) {
						core::NodeManager mgr;
						return details::isTypeOrValueOrParam(factory(mgr));
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
						if(pattern && pattern != any) { out << ":" << *pattern; }
						return out;
					}
				};

				// Depth recursion (downward * operator)
				struct Recursion : public TreePattern {
					const string name;
					const bool terminal;
					const TreePatternPtr pattern;

					Recursion(const string& name) : TreePattern(TreePattern::Recursion, false), name(name), terminal(true) {}
					Recursion(const string& name, const TreePatternPtr& pattern)
					    : TreePattern(TreePattern::Recursion, pattern->isVariableFree, pattern->mayBeType), name(name), terminal(false), pattern(pattern) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						if(terminal) { return out << "rec." << name; }
						return out << "rT." << name << "(" << *pattern << ")";
					}
				};

				// bridge to Node Pattern
				struct Node : public TreePattern {
					const int id;
					const int type;
					const ListPatternPtr pattern;

					Node(const ListPatternPtr& pattern) : TreePattern(TreePattern::Node, pattern->isVariableFree), id(-1), type(-1), pattern(pattern) {}

					Node(char id, const ListPatternPtr& pattern)
					    : TreePattern(TreePattern::Node, pattern->isVariableFree), id(id), type(-1), pattern(pattern) {}

					Node(const core::NodeType type, const ListPatternPtr& pattern)
					    : TreePattern(TreePattern::Node, pattern->isVariableFree, details::isTypeOrValueOrParam(type)), id(-1), type(type), pattern(pattern) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						if(id != -1) {
							out << "(" << id << "|";
						} else if(type != -1) {
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

					Negation(const TreePatternPtr& pattern) : TreePattern(TreePattern::Negation, pattern->isVariableFree), pattern(pattern) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << "!(" << *(pattern.get()) << ")";
					}
				};

				// Conjunction
				struct Conjunction : public TreePattern {
					const TreePatternPtr pattern1;
					const TreePatternPtr pattern2;

					Conjunction(const TreePatternPtr& a, const TreePatternPtr& b)
					    : TreePattern(TreePattern::Conjunction, a->isVariableFree && b->isVariableFree, a->mayBeType && b->mayBeType), pattern1(a),
					      pattern2(b) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << *(pattern1.get()) << " & " << *(pattern2.get());
					}
				};

				// Disjunction
				struct Disjunction : public TreePattern {
					const TreePatternPtr pattern1;
					const TreePatternPtr pattern2;

					Disjunction(const TreePatternPtr& a, const TreePatternPtr& b)
					    : TreePattern(TreePattern::Disjunction, a->isVariableFree && b->isVariableFree, a->mayBeType || b->mayBeType), pattern1(a),
					      pattern2(b) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << *(pattern1.get()) << " | " << *(pattern2.get());
					}
				};

				// Matches any subtree containing one of the subpatterns
				struct Descendant : public TreePattern {
					const std::vector<TreePatternPtr> subPatterns;

					Descendant(const vector<TreePatternPtr>& patterns)
					    : TreePattern(TreePattern::Descendant, ::all(patterns, [](const TreePatternPtr& cur) { return cur->isVariableFree; })),
					      subPatterns(patterns){};

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << "aT(" << join(",", subPatterns, print<deref<TreePatternPtr>>()) << ")";
					}
				};

				// A generic lambda evaluating a match
				struct Lambda : public TreePattern {
					typedef std::function<bool(const core::NodePtr&)> ptr_condition_type;
					typedef std::function<bool(const core::NodeAddress&)> addr_condition_type;

					boost::variant<ptr_condition_type, addr_condition_type> condition;

					Lambda(const ptr_condition_type& condition, bool mayBeType = true)
					    : TreePattern(TreePattern::Lambda, true, mayBeType), condition(condition) {}
					Lambda(const addr_condition_type& condition, bool mayBeType = true)
					    : TreePattern(TreePattern::Lambda, true, mayBeType), condition(condition) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						core::NodeManager mgr;
						return out << "LambdaTreePattern(" << (isPtrCondition() ? "NodePtr" : "NodeAddress") << ")";
					}

					bool isPtrCondition() const {
						return condition.which() == 0;
					}
				};
			}

			namespace list {

				struct Empty : public ListPattern {
					Empty() : ListPattern(ListPattern::Empty, true, 0, 0) {}
					virtual std::ostream& printTo(std::ostream& out) const {
						return out << "[]";
					}
				};

				// The most simple node pattern covering a single tree
				struct Single : public ListPattern {
					const TreePatternPtr element;

					Single(const TreePatternPtr& element) : ListPattern(ListPattern::Single, element->isVariableFree, 1, 1), element(element) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << *element;
					}
				};

				// A sequence node pattern representing the composition of two node patterns
				struct Sequence : public ListPattern {
					const ListPatternPtr left;
					const ListPatternPtr right;

					Sequence(const ListPatternPtr& left, const ListPatternPtr& right)
					    : ListPattern(ListPattern::Sequence, left->isVariableFree && right->isVariableFree,
					                  left->minLength + right->minLength,                      // sum up lower boundaries
					                  utils::saturating_add(left->maxLength, right->maxLength) // sum up upper boundaries (saturation add)
					                  ),
					      left(left), right(right) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << *left << "," << *right;
					}
				};

				// A node pattern alternative
				struct Alternative : public ListPattern {
					const ListPatternPtr alternative1;
					const ListPatternPtr alternative2;

					Alternative(const ListPatternPtr& A, const ListPatternPtr& B)
					    : ListPattern(ListPattern::Alternative, A->isVariableFree && B->isVariableFree, std::min(A->minLength, B->minLength),
					                  std::max(A->maxLength, B->maxLength)),
					      alternative1(A), alternative2(B) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						return out << *alternative1 << "|" << *alternative2;
					}
				};

				// Realizes the star operator for node patterns
				struct Repetition : public ListPattern {
					const ListPatternPtr pattern;
					const unsigned minRep; // minimum number of repetitions

					Repetition(const ListPatternPtr& pattern, unsigned minRep = 0)
					    : ListPattern(ListPattern::Repetition, pattern->isVariableFree, pattern->minLength * minRep), pattern(pattern), minRep(minRep) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						if(minRep == 0) { return out << "[" << *pattern << "]*"; }
						if(minRep == 1) { return out << "[" << *pattern << "]+"; }
						return out << "[" << *pattern << "]*{" << minRep << "}";
					}
				};

				// A simple variable
				struct Variable : public ListPattern {
					const static ListPatternPtr any;
					const std::string name;
					const ListPatternPtr pattern;

					Variable(const std::string& name, const ListPatternPtr& pattern = any)
					    : ListPattern(ListPattern::Variable, false, pattern->minLength, pattern->maxLength), name(name), pattern(pattern) {}

					virtual std::ostream& printTo(std::ostream& out) const {
						out << "$" << name;
						if(pattern && pattern != any) { out << ":" << *pattern; }
						return out;
					}
				};

			} // end namespace list

			insieme::core::pattern::TreePattern lambda(const std::function<bool(const core::NodePtr&)>& condition) {
				return insieme::core::pattern::TreePattern(std::make_shared<impl::tree::Lambda>(condition));
			}
			insieme::core::pattern::TreePattern lambda(const std::function<bool(const core::NodeAddress&)>& condition) {
				return insieme::core::pattern::TreePattern(std::make_shared<impl::tree::Lambda>(condition));
			}

		} // end namespace impl

		TreePattern value(const core::NodeValue& value) {
			return TreePattern(std::make_shared<impl::tree::Value>(value));
		}

		TreePattern atom(const TreePtr& tree) {
			return TreePattern(std::make_shared<impl::tree::Constant>(tree));
		}
		TreePattern atom(const core::NodePtr& tree) {
			return TreePattern(std::make_shared<impl::tree::Constant>(tree));
		}

		TreePattern lazyAtom(const std::function<core::NodePtr(core::NodeManager&)>& factory) {
			return TreePattern(std::make_shared<impl::tree::LazyConstant>(factory));
		}

		ListPattern single(const TreePattern& pattern) {
			return ListPattern(std::make_shared<impl::list::Single>(pattern));
		}

		TreePattern operator!(const TreePattern& a) {
			return TreePattern(std::make_shared<impl::tree::Negation>(a));
		}

		TreePattern operator&(const TreePattern& a, const TreePattern& b) {
			return TreePattern(std::make_shared<impl::tree::Conjunction>(a, b));
		}

		TreePattern operator|(const TreePattern& a, const TreePattern& b) {
			return TreePattern(std::make_shared<impl::tree::Disjunction>(a, b));
		}

		TreePattern node(const ListPattern& pattern) {
			return TreePattern(std::make_shared<impl::tree::Node>(pattern));
		}
		TreePattern node(const char id, const ListPattern& pattern) {
			return TreePattern(std::make_shared<impl::tree::Node>(id, pattern));
		}
		TreePattern node(const core::NodeType type, const ListPattern& pattern) {
			return TreePattern(std::make_shared<impl::tree::Node>(type, pattern));
		}

		TreePattern var(const std::string& name, const TreePattern& pattern) {
			return TreePattern(std::make_shared<impl::tree::Variable>(name, pattern));
		}

		ListPattern listVar(const std::string& name, const ListPattern& pattern) {
			return ListPattern(std::make_shared<impl::list::Variable>(name, pattern));
		}

		TreePattern aT(const std::vector<TreePattern>& patterns) {
			std::vector<impl::TreePatternPtr> subPatterns(patterns.begin(), patterns.end());
			return TreePattern(std::make_shared<impl::tree::Descendant>(subPatterns));
		}

		TreePattern rT(const TreePattern& pattern, const string& varName) {
			return TreePattern(std::make_shared<impl::tree::Recursion>(varName, pattern));
		}
		TreePattern rT(const ListPattern& pattern, const string& varName) {
			return TreePattern(std::make_shared<impl::tree::Recursion>(varName, node(pattern)));
		}
		TreePattern rec(const string& varName) {
			return TreePattern(std::make_shared<impl::tree::Recursion>(varName));
		}

		ListPattern operator|(const ListPattern& a, const ListPattern& b) {
			return ListPattern(std::make_shared<impl::list::Alternative>(a, b));
		}
		ListPattern operator|(const TreePattern& a, const ListPattern& b) {
			return ListPattern(std::make_shared<impl::list::Alternative>(single(a), b));
		}
		ListPattern operator|(const ListPattern& a, const TreePattern& b) {
			return ListPattern(std::make_shared<impl::list::Alternative>(a, single(b)));
		}


		ListPattern operator*(const ListPattern& pattern) {
			return ListPattern(std::make_shared<impl::list::Repetition>(pattern));
		}
		ListPattern operator*(const TreePattern& pattern) {
			return ListPattern(std::make_shared<impl::list::Repetition>(single(pattern)));
		}

		ListPattern operator+(const ListPattern& pattern) {
			return ListPattern(std::make_shared<impl::list::Repetition>(pattern, 1));
		}
		ListPattern operator+(const TreePattern& pattern) {
			return ListPattern(std::make_shared<impl::list::Repetition>(single(pattern), 1));
		}

		ListPattern operator<<(const ListPattern& a, const ListPattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(a, b));
		}
		ListPattern operator<<(const TreePattern& a, const ListPattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(single(a), b));
		}
		ListPattern operator<<(const ListPattern& a, const TreePattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(a, single(b)));
		}
		ListPattern operator<<(const TreePattern& a, const TreePattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(single(a), single(b)));
		}

		ListPattern operator>>(const ListPattern& a, const ListPattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(a, b));
		}
		ListPattern operator>>(const TreePattern& a, const ListPattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(single(a), b));
		}
		ListPattern operator>>(const ListPattern& a, const TreePattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(a, single(b)));
		}
		ListPattern operator>>(const TreePattern& a, const TreePattern& b) {
			return ListPattern(std::make_shared<impl::list::Sequence>(single(a), single(b)));
		}


		namespace details {

			bool isTypeOrValueOrParam(const core::NodeType type) {
				return core::isA<core::NC_Type>(type) || core::isA<core::NC_Value>(type);
			}

			/**
			 * This function is implementing the actual matching algorithm.
			 */
			template <typename T, typename target = typename match_target_info<T>::target_type>
			boost::optional<Match<target>> match(const impl::TreePattern& pattern, const T& node);

			/**
			 * This function is implementing the actual matching algorithm for list patterns.
			 */
			template <typename T, typename target = typename match_target_info<T>::target_type>
			boost::optional<Match<target>> match(const impl::ListPattern& pattern, const std::vector<T>& trees);
		}

		TreePattern::TreePattern() : pattern(any.pattern) {}

		MatchOpt TreePattern::matchPointer(const core::NodePtr& node) const {
			return pattern->matchPointer(node);
		}

		AddressMatchOpt TreePattern::matchAddress(const core::NodeAddress& node) const {
			return pattern->matchAddress(node);
		}

		TreeMatchOpt TreePattern::matchTree(const TreePtr& tree) const {
			return pattern->matchTree(tree);
		}

		std::ostream& TreePattern::printTo(std::ostream& out) const {
			return out << *pattern;
		}

		ListPattern::ListPattern() : pattern(anyList.pattern) {}

		TreeMatchOpt ListPattern::match(const vector<TreePtr>& trees) const {
			return pattern->match(trees);
		}

		std::ostream& ListPattern::printTo(std::ostream& out) const {
			return out << *pattern;
		}

		namespace impl {

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
		}

		const TreePattern any = TreePattern(std::make_shared<impl::tree::Wildcard>());
		const TreePattern recurse = TreePattern(std::make_shared<impl::tree::Recursion>("x"));

		const ListPattern anyList = *TreePattern(std::make_shared<impl::tree::Wildcard>());
		const ListPattern empty = ListPattern(std::make_shared<impl::list::Empty>());

		namespace impl {
		namespace tree {

			const TreePatternPtr Variable::any = pattern::any;
		}

		namespace list {

			const ListPatternPtr Variable::any = pattern::anyList;
		}
		}


		// -------------------------------------------------------------------------------------
		//   Pattern Matcher
		// -------------------------------------------------------------------------------------


		size_t hash_value(const TreePtr& tree) {
			return (size_t)(tree.get());
		}

		// -- Implementation detail -------------------------------------------

		namespace details {


			enum CachedMatchResult { Yes, No, Unknown };


			template <typename T>
			class MatchContext : public utils::Printable, private boost::noncopyable {
			  public:
				typedef typename T::value_type value_type;
				typedef typename T::atom_type atom_type;
				typedef typename T::list_type list_type;
				typedef typename T::list_iterator iterator;

				// a cache for tree patterns not including variables
				typedef std::map<std::pair<const impl::TreePattern*, typename T::atom_type>, bool> tree_pattern_cache;

				// a cache for tree patterns not including variables
				typedef std::map<std::tuple<const impl::ListPattern*, iterator, iterator>, bool> list_pattern_cache;

				struct RecVarInfo {
					impl::TreePatternPtr pattern;
					unsigned level;
					unsigned counter;

					RecVarInfo(impl::TreePatternPtr pattern = impl::TreePatternPtr(), unsigned level = 0) : pattern(pattern), level(level), counter(0) {}
				};

			  private:
				typedef std::unordered_map<string, RecVarInfo> RecVarMap;

				MatchPath path;

				Match<T> match;

				RecVarMap boundRecursiveVariables;

				tree_pattern_cache treePatternCache;

				list_pattern_cache listPatternCache;

			  public:
				MatchContext(const value_type& root = value_type()) : match(root), treePatternCache() {}


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

				std::size_t get() const {
					return path.get();
				}

				void set(std::size_t index) {
					path.set(index);
				}

				std::size_t getDepth() const {
					return path.getDepth();
				}

				const MatchPath& getCurrentPath() const {
					return path;
				}

				MatchPath& getCurrentPath() {
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

				void unbindTreeVar(const std::string& var) {
					match.unbindTreeVar(path, var);
				}

				const value_type& getTreeVarBinding(const std::string& var) const {
					return match.getTreeVarBinding(path, var);
				}

				// -- List Variables --------------------------

				bool isListVarBound(const std::string& var) const {
					return match.isListVarBound(path, var);
				}

				void bindListVar(const std::string& var, const iterator& begin, const iterator& end) {
					match.bindListVar(path, var, begin, end);
				}

				void unbindListVar(const std::string& var) {
					match.unbindListVar(path, var);
				}

				list_type getListVarBinding(const std::string& var) const {
					return match.getListVarBinding(path, var);
				}

				// -- Recursive Variables ---------------------------

				bool isRecVarBound(const std::string& var) const {
					return boundRecursiveVariables.find(var) != boundRecursiveVariables.end();
				}

				void bindRecVar(const std::string& var, const impl::TreePatternPtr& pattern) {
					assert_false(isRecVarBound(var)) << "Variable bound twice";
					boundRecursiveVariables.insert(std::make_pair(var, RecVarInfo(pattern, path.getDepth())));
				}

				void bindRecVar(const std::string& var, const RecVarInfo& info) {
					assert_false(isRecVarBound(var)) << "Variable bound twice";
					boundRecursiveVariables.insert(std::make_pair(var, info));
				}

				impl::TreePatternPtr getRecVarBinding(const std::string& var) const {
					assert_true(isRecVarBound(var)) << "Requesting bound value for unbound tree variable";
					return boundRecursiveVariables.find(var)->second.pattern;
				}

				const RecVarInfo& getRecVarInfo(const std::string& var) const {
					assert_true(isRecVarBound(var)) << "Requesting bound value for unbound tree variable";
					return boundRecursiveVariables.find(var)->second;
				}

				unsigned getRecVarDepth(const std::string& var) const {
					assert_true(isRecVarBound(var)) << "Requesting bound value for unbound tree variable";
					return boundRecursiveVariables.find(var)->second.level;
				}

				unsigned getRecVarCounter(const std::string& var) const {
					assert_true(isRecVarBound(var)) << "Requesting bound value for unbound tree variable";
					return boundRecursiveVariables.find(var)->second.counter;
				}

				unsigned incRecVarCounter(const std::string& var) {
					assert_true(isRecVarBound(var)) << "Requesting bound value for unbound tree variable";
					return ++(boundRecursiveVariables.find(var)->second.counter);
				}

				void unbindRecVar(const std::string& var) {
					boundRecursiveVariables.erase(var);
				}

				// -- Cached Match Results -----------------------------

				CachedMatchResult cachedMatch(const impl::TreePattern& pattern, const atom_type& node) const {
					assert_true(pattern.isVariableFree) << "Can only cache variable-free pattern fragments!";

					auto pos = treePatternCache.find(std::make_pair(&pattern, node));
					if(pos == treePatternCache.end()) { return Unknown; }
					return (pos->second) ? Yes : No;
				}

				void addToCache(const impl::TreePattern& pattern, const value_type& node, bool match) {
					treePatternCache[std::make_pair(&pattern, node)] = match;
				}

				CachedMatchResult cachedMatch(const impl::ListPattern& pattern, const iterator& begin, const iterator& end) const {
					assert_true(pattern.isVariableFree) << "Can only cache variable-free pattern fragments!";

					auto pos = listPatternCache.find(std::make_tuple(&pattern, begin, end));
					if(pos == listPatternCache.end()) { return Unknown; }
					return (pos->second) ? Yes : No;
				}

				void addToCache(const impl::ListPattern& pattern, const iterator& begin, const iterator& end, bool match) {
					listPatternCache[std::make_tuple(&pattern, begin, end)] = match;
				}

				// -- debug print --

				virtual std::ostream& printTo(std::ostream& out) const {
					out << "Match(";
					out << path << ", ";
					out << match << ", ";
					out << "{" << join(",", boundRecursiveVariables, [](std::ostream& out, const std::pair<string, RecVarInfo>& cur) {
						out << cur.first << "=" << cur.second.pattern;
					}) << "}";
					return out << ")";
				}

				// -- Backup and Restore --------------------------------------

				struct MatchContextBackup {
					/**
					 * A reference to the context this backup is based on.
					 * Backups may only be restored for the same context.
					 */
					const MatchContext& context;

					const IncrementID backup;

					explicit MatchContextBackup(const MatchContext& data) : context(data), backup(data.match.backup()) {}

					void restore(MatchContext& target) {
						assert(&target == &context && "Unable to restore different context!");

						// restore match context content
						//	- path can be ignored since it is continuously maintained
						//  - recursive variables are also not required to be checked (handled automatically)

						// the variable bindings need to be re-set
						target.match.restore(backup);
					}
				};

				MatchContextBackup backup() const {
					return MatchContextBackup(*this);
				}

				void restore(const MatchContextBackup& backup) {
					backup.restore(*this);
				}
			};


			template <typename T>
			bool match(const impl::TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree,
			           const std::function<bool(MatchContext<T>&)>& delayedCheck);

			template <typename T, typename iterator = typename T::value_iterator>
			bool match(const impl::ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end,
			           const std::function<bool(MatchContext<T>&)>& delayedCheck);

			template <typename T>
			inline bool match(const impl::TreePatternPtr& pattern, MatchContext<T>& context, const typename T::value_type& tree,
			                  const std::function<bool(MatchContext<T>&)>& delayedCheck) {
				return match(*pattern.get(), context, tree, delayedCheck);
			}

			template <typename T, typename iterator = typename T::value_iterator>
			inline bool match(const impl::ListPatternPtr& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end,
			                  const std::function<bool(MatchContext<T>&)>& delayedCheck) {
				return match(*pattern.get(), context, begin, end, delayedCheck);
			}


			template <typename T, typename target>
			boost::optional<Match<target>> match(const impl::TreePattern& pattern, const T& tree) {
				MatchContext<target> context(tree);
				std::function<bool(MatchContext<target>&)> accept = [](MatchContext<target>& context) -> bool { return true; };
				if(match(pattern, context, tree, accept)) {
					// it worked => return match result
					return context.getMatch();
				}
				return boost::none;
			}

			template <typename T, typename target>
			boost::optional<Match<target>> match(const impl::ListPattern& pattern, const std::vector<T>& trees) {
				MatchContext<target> context;
				std::function<bool(MatchContext<target>&)> accept = [](MatchContext<target>& context) -> bool { return true; };
				if(match(pattern, context, trees.begin(), trees.end(), accept)) {
					// => it is a match (but leaf root empty)
					return context.getMatch();
				}
				return boost::none;
			}

			template <typename T, typename target>
			boost::optional<Match<target>> match(const impl::TreePatternPtr& pattern, const T& tree) {
				return match(*pattern.get(), tree);
			}

			template <typename T, typename target>
			boost::optional<Match<target>> match(const impl::ListPatternPtr& pattern, const std::vector<T>& trees) {
				return match(*pattern.get(), trees);
			}


			// -- Match Tree Patterns -------------------------------------------------------

			// Atom, Variable, Wildcard, Node, Negation, Alternative, Descendant, Recursion, Lambda

			namespace tree {

			#define MATCH(NAME)                                                                                                                                \
				template <typename T>                                                                                                                          \
				bool match##NAME(const pattern::impl::tree::NAME& pattern, MatchContext<T>& context, const typename T::value_type& tree,                       \
				                 const std::function<bool(MatchContext<T>&)>& delayedCheck)

				MATCH(Value) {
					return tree->isValue() && tree->getNodeValue() == pattern.value && delayedCheck(context);
				}

				MATCH(Constant) {
					assert_true(pattern.nodeAtom) << "Wrong type of constant value stored within atom node!";
					return *pattern.nodeAtom == *tree && delayedCheck(context);
				}

				MATCH(LazyConstant) {
					auto value = pattern.factory(tree->getNodeManager());
					return *value == *tree && delayedCheck(context);
				}

				MATCH(Wildcard) {
					return delayedCheck(context); // just finish delayed checks
				}

				MATCH(Variable) {
					// check whether the variable is already bound
					if(context.isTreeVarBound(pattern.name)) { return *context.getTreeVarBinding(pattern.name) == *tree && delayedCheck(context); }

					// check filter-pattern of this variable
					context.bindTreeVar(pattern.name, tree); // speculate => bind variable
					return match(pattern.pattern, context, tree, delayedCheck);
				}

				namespace {

					bool contains_variable_free(MatchContext<ptr_target>& context, const core::NodePtr& tree, const impl::TreePatternPtr& pattern,
					                            const std::function<bool(MatchContext<ptr_target>&)>& delayedCheck) {
						return core::visitDepthFirstOnceInterruptible(
						    tree, [&](const core::NodePtr& cur) -> bool { return match(pattern, context, cur, delayedCheck); }, pattern->mayBeType);
					}

					bool contains_variable_free(MatchContext<address_target>& context, const core::NodeAddress& tree, const impl::TreePatternPtr& pattern,
					                            const std::function<bool(MatchContext<address_target>&)>& delayedCheck) {
						return core::visitDepthFirstOnceInterruptible(tree.as<core::NodePtr>(), [&](const core::NodePtr& cur) -> bool {
							return match(pattern, context, core::NodeAddress(cur), delayedCheck);
						}, pattern->mayBeType);
					}

					bool contains_variable_free(MatchContext<tree_target>& context, const TreePtr& tree, const impl::TreePatternPtr& pattern,
					                            const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
						bool res = false;
						res = res || match(pattern, context, tree, delayedCheck);
						for_each(tree->getChildList(), [&](const TreePtr& cur) { // generalize this
							res = res || contains_variable_free(context, cur, pattern, delayedCheck);
						});
						return res;
					}

					bool contains_with_variables(MatchContext<ptr_target>& context, const core::NodePtr& tree, const impl::TreePatternPtr& pattern,
					                             const std::function<bool(MatchContext<ptr_target>&)>& delayedCheck) {
						// if there are variables, the context needs to be reset
						auto backup = context.backup();
						return core::visitDepthFirstOnceInterruptible(tree, [&](const core::NodePtr& cur) -> bool {
							backup.restore(context); // restore context
							return match(pattern, context, cur, delayedCheck);
						}, true, pattern->mayBeType);
					}

					bool contains_with_variables(MatchContext<address_target>& context, const core::NodeAddress& tree, const impl::TreePatternPtr& pattern,
					                             const std::function<bool(MatchContext<address_target>&)>& delayedCheck) {
						// if there are variables, the context needs to be reset
						auto backup = context.backup();
						// for addresses everything has to be visited (no visit once)
						return core::visitDepthFirstInterruptible(tree, [&](const core::NodeAddress& cur) -> bool {
							backup.restore(context); // restore context
							return match(pattern, context, cur, delayedCheck);
						}, true, pattern->mayBeType);
					}

					bool contains_with_variables(MatchContext<tree_target>& context, const TreePtr& tree, const impl::TreePatternPtr& pattern,
					                             const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
						// if there are variables, all nodes need to be checked
						bool res = false;

						// isolate context for each try
						auto backup = context.backup();
						res = res || match(pattern, context, tree, delayedCheck);
						for_each(tree->getChildList(), [&](const TreePtr& cur) { // generalize this
							if(!res) {
								backup.restore(context); // restore context
							}
							res = res || contains_with_variables(context, cur, pattern, delayedCheck);
						});
						return res;
					}
				}

				template <typename T>
				bool contains(MatchContext<T>& context, const typename T::value_type& tree, const impl::TreePatternPtr& pattern,
				              const std::function<bool(MatchContext<T>&)>& delayedCheck) {
					// prune types
					if(!pattern->mayBeType && isTypeOrValueOrParam(tree)) { return false; }

					// if variable free, only non-shared nodes need to be checked
					if(pattern->isVariableFree) { return contains_variable_free(context, tree, pattern, delayedCheck); }

					// use version considering variables
					return contains_with_variables(context, tree, pattern, delayedCheck);
				}

				MATCH(Descendant) {
					// search for all patterns occurring in the sub-trees
					return ::all(pattern.subPatterns, [&](const impl::TreePatternPtr& cur) { return contains(context, tree, cur, delayedCheck); });
				}

				MATCH(Recursion) {
					// handle terminal
					if(pattern.terminal) {
						// get pattern bound to recursive variable
						assert_true(context.isRecVarBound(pattern.name)) << "Recursive variable unbound!";

						// save current context path
						MatchPath path = context.getCurrentPath();

						// restore recursion level of outer recursive scope
						unsigned recLevel = context.getRecVarDepth(pattern.name);
						context.getCurrentPath().prune(recLevel);

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
					bool preBound = context.isRecVarBound(pattern.name);
					typename MatchContext<T>::RecVarInfo oldInfo;
					if(preBound) {
						assert(context.getDepth() > context.getRecVarDepth(pattern.name) && "Nested recursive variables must not be on same level!");
						oldInfo = context.getRecVarInfo(pattern.name);
						context.unbindRecVar(pattern.name);
					}

					// start by ignoring delayed checks
					std::function<bool(MatchContext<T>&)> accept = [](MatchContext<T>& context) { return true; };

					// match using new rec-var binding
					context.bindRecVar(pattern.name, pattern.pattern);
					bool res = match(pattern.pattern, context, tree, accept);

					// remove binding
					context.unbindRecVar(pattern.name);
					context.pop();

					// restore old recursive variable if necessary
					if(preBound) { context.bindRecVar(pattern.name, oldInfo); }

					// run remaining delayed checks
					return res && delayedCheck(context);
				}

				MATCH(Node) {
					if(pattern.type != -1 && pattern.type != tree->getNodeType()) { return false; }
					const auto& children = tree->getChildList();
					return match(pattern.pattern, context, children.begin(), children.end(), delayedCheck);
				}

				MATCH(Negation) {
					// backup current state - negation operates on isolated context (what shouldn't be shouldn't leaf traces)
					auto backup = context.backup();

					// ignore delayed checks while matching inner block and conduct those checks if inner one fails
					std::function<bool(MatchContext<T>&)> accept = [](MatchContext<T>& context) { return true; };
					bool fits = !match(pattern.pattern, context, tree, accept);

					// save us the effort of restoring the old context
					if(!fits) { return false; }

					// restore context
					backup.restore(context);

					// finish by processing delayed checks on the original context
					return delayedCheck(context);
				}

				MATCH(Conjunction) {
					// match first and delay matching of second half
					std::function<bool(MatchContext<T>&)> delayed = [&](MatchContext<T>& context) {
						return match(pattern.pattern2, context, tree, delayedCheck);
					};
					return match(pattern.pattern1, context, tree, delayed);
				}

				MATCH(Disjunction) {
					// create context backup for rollback
					auto backup = context.backup();
					if(match(pattern.pattern1, context, tree, delayedCheck)) { return true; }
					// restore context
					backup.restore(context);
					return match(pattern.pattern2, context, tree, delayedCheck);
				}

				// lambda implementation is specialized for IR nodes and addresses
				inline bool matchLambda(const pattern::impl::tree::Lambda& pattern, MatchContext<ptr_target>& context, const NodePtr& tree,
				                        const std::function<bool(MatchContext<ptr_target>&)>& delayedCheck) {
					assert_true(pattern.isPtrCondition())
					    << "Pattern " << pattern
					    << " features an address condition but is applied during pointer visiting. This will very likely not do what you want.";
					return boost::get<pattern::impl::tree::Lambda::ptr_condition_type>(pattern.condition)(tree) && delayedCheck(context);
				}
				inline bool matchLambda(const pattern::impl::tree::Lambda& pattern, MatchContext<address_target>& context, const NodeAddress& tree,
				                        const std::function<bool(MatchContext<address_target>&)>& delayedCheck) {
					bool match = false;
					if(pattern.isPtrCondition()) {
						match = boost::get<pattern::impl::tree::Lambda::ptr_condition_type>(pattern.condition)(tree.getAddressedNode());
					} else {
						match = boost::get<pattern::impl::tree::Lambda::addr_condition_type>(pattern.condition)(tree);
					}
					return match && delayedCheck(context);
				}
				inline bool matchLambda(const pattern::impl::tree::Lambda& pattern, MatchContext<tree_target>& context, const TreePtr& tree,
				                        const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
					return false;
				}

				// -- for test structure only --

				// a specialization for tree pointers
				inline bool matchValue(const pattern::impl::tree::Value& pattern, MatchContext<tree_target>& context, const TreePtr& tree,
				                       const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
					return false;
				}

				// a specialization for tree pointers
				inline bool matchConstant(const pattern::impl::tree::Constant& pattern, MatchContext<tree_target>& context, const TreePtr& tree,
				                          const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
					assert_true(pattern.treeAtom) << "Wrong type of constant value stored within atom node!";
					return *pattern.treeAtom == *tree && delayedCheck(context);
				}

				// a specialization for tree pointers
				inline bool matchLazyConstant(const pattern::impl::tree::LazyConstant& pattern, MatchContext<tree_target>& context, const TreePtr& tree,
				                              const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
					assert_fail() << "Not applicable to test-tree structure!";
					return false;
				}


				// a specialization for tree pointers
				inline bool matchNode(const pattern::impl::tree::Node& pattern, MatchContext<tree_target>& context, const TreePtr& tree,
				                      const std::function<bool(MatchContext<tree_target>&)>& delayedCheck) {
					if(pattern.id != -1 && pattern.id != tree->getId()) { return false; }
					auto& children = tree->getSubTrees();
					return match(pattern.pattern, context, children.begin(), children.end(), delayedCheck) && delayedCheck(context);
				}

				#undef MATCH

			} // end namespace tree

			namespace list {

			// Empty, Single, Variable, Alternative, Sequence, Repetition

			#define MATCH(NAME)                                                                                                                                \
				template <typename T, typename iterator = typename T::value_iterator>                                                                          \
				bool match##NAME(const pattern::impl::list::NAME& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end,               \
				                 const std::function<bool(MatchContext<T>&)>& delayedCheck)

				MATCH(Empty) {
					// only accepts empty list
					return begin == end && delayedCheck(context);
				}

				MATCH(Single) {
					// range has to be exactly one ...
					if(std::distance(begin, end) != 1) { return false; }
					// ... and the pattern has to match
					return match(pattern.element, context, *begin, delayedCheck);
				}

				MATCH(Variable) {
					// check whether the variable is already bound
					if(context.isListVarBound(pattern.name)) {
						const auto& value = context.getListVarBinding(pattern.name);
						return std::distance(begin, end) == std::distance(value.begin(), value.end())
						       && std::equal(begin, end, value.begin(), equal_target<typename T::value_type>()) && delayedCheck(context);
					}

					// check filter-pattern of this variable
					context.bindListVar(pattern.name, begin, end); // speculate => bind variable

					// check whether the tree is a valid substitution for this variable
					return match(pattern.pattern, context, begin, end, delayedCheck);
				}

				MATCH(Sequence) {
					unsigned length = std::distance(begin, end);
					assert(length >= pattern.minLength && length <= pattern.maxLength);

					// compute sub-range to be searched
					auto min = pattern.left->minLength;
					auto max = length - pattern.right->minLength;

					// constrain min based on max length of right side
					if(length > pattern.right->maxLength) { min = std::max(min, length - pattern.right->maxLength); }

					// constrain max based on max length of left side
					if(length > pattern.left->maxLength) { max = std::min(max, pattern.left->maxLength); }

					// special case: only one split point => safe a backup
					if(min == max) {
						std::function<bool(MatchContext<T>&)> delayed = [&](MatchContext<T>& context) {
							return match(pattern.right, context, begin + min, end, delayedCheck);
						};
						return match(pattern.left, context, begin, begin + min, delayed);
					}

					// search for the split-point ...
					auto backup = context.backup();
					for(auto i = begin + min; i <= begin + max; ++i) {
						backup.restore(context);
						// check left side and delay right side
						std::function<bool(MatchContext<T>&)> delayed = [&](MatchContext<T>& context) {
							return match(pattern.right, context, i, end, delayedCheck);
						};
						if(match(pattern.left, context, begin, i, delayed)) { return true; }
					}
					return false;
				}

				MATCH(Alternative) {
					// try both alternatives using a private context
					auto backup = context.backup();
					if(match(pattern.alternative1, context, begin, end, delayedCheck)) { return true; }

					// try alternative after reseting context
					backup.restore(context);
					return match(pattern.alternative2, context, begin, end, delayedCheck);
				}

				template <typename T, typename iterator = typename T::value_iterator>
				bool matchRepetitionInternal(const pattern::impl::list::Repetition& rep, MatchContext<T>& context, const iterator& begin, const iterator& end,
				                             unsigned repetitions, const std::function<bool(MatchContext<T>&)>& delayedCheck) {
					// empty is accepted (terminal case)
					if(begin == end) { return repetitions >= rep.minRep && delayedCheck(context); }

					// test special case of a single iteration
					auto backup = context.backup();
					if(rep.minRep <= 1) {
						// try whether a single repetition is sufficient
						if(match(rep.pattern, context, begin, end, delayedCheck)) { return true; }
						// undo changes
						backup.restore(context);
					}

					// compute sub-range to be searched
					unsigned length = std::distance(begin, end);
					assert(length >= rep.pattern->minLength);

					auto min = rep.pattern->minLength;
					auto max = std::min(rep.pattern->maxLength,
					                    length - min * (rep.minRep - repetitions)); // max length or length - min space required for remaining repetitions

					// try one pattern + a recursive repetition
					for(auto i = begin + min; i <= begin + max; ++i) {
						// restore context for this attempt
						backup.restore(context);

						if(!match(rep.pattern, context, begin, i, delayedCheck)) {
							// does not match ... try next!
							continue;
						}

						// increment repetition counter
						context.inc();

						if(!matchRepetitionInternal(rep, context, i, end, repetitions + 1, delayedCheck)) {
							// does not fit any more ... try next!
							continue;
						}

						return true;
					}

					// the pattern does not match!
					return false;
				}

				MATCH(Repetition) {
					// special case: repetition of a wildcard
					if(pattern.pattern->type == impl::ListPattern::Single
					   && static_pointer_cast<insieme::core::pattern::impl::list::Single>(pattern.pattern)->element->type == impl::TreePattern::Wildcard) {
						assert(std::distance(begin, end) >= pattern.minRep);
						return delayedCheck(context);
					}

					// increase nesting level of variables by one
					context.push();

					// accept everything until repetition is complete
					std::function<bool(MatchContext<T>&)> accept = [](MatchContext<T>& context) { return true; };
					bool res = matchRepetitionInternal(pattern, context, begin, end, 0, accept);

					// drop extra level
					context.pop();

					// conduct delayed checks if necessary
					return res & delayedCheck(context);
				}

				#undef MATCH

			} // end namespace list

			namespace {

				template <typename T>
				bool match_internal(const impl::TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree,
				                    const std::function<bool(MatchContext<T>&)>& delayedCheck) {
					switch(pattern.type) {
					#define CASE(NAME)                                                                                                                         \
						case impl::TreePattern::NAME:                                                                                                          \
							return tree::match##NAME(static_cast<const pattern::impl::tree::NAME&>(pattern), context, tree, delayedCheck)
						CASE(Value);
						CASE(Constant);
						CASE(LazyConstant);
						CASE(Variable);
						CASE(Wildcard);
						CASE(Node);
						CASE(Negation);
						CASE(Conjunction);
						CASE(Disjunction);
						CASE(Descendant);
						CASE(Recursion);
						CASE(Lambda);
						#undef CASE
					}
					assert_fail() << "Missed a pattern type!";
					return false;
				}
			}

			template <typename T>
			bool match(const impl::TreePattern& pattern, MatchContext<T>& context, const typename T::value_type& tree,
			           const std::function<bool(MatchContext<T>&)>& delayedCheck) {
				const bool DEBUG = false;

				if(DEBUG) { std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... \n"; }
				assert_decl(auto path = context.getCurrentPath());

				// quick check for wildcards (should not even be cached)
				if(pattern.type == impl::TreePattern::Wildcard) { return delayedCheck(context); }

				// skip searching within types if not searching for a type
				if(!pattern.mayBeType && isTypeOrValueOrParam(tree)) { return false; }

				// use cache if possible
				// TODO: re-enable, find a better solution of disabling/enabling pattern matcher cache
				//if(pattern.isVariableFree) {
				//	CachedMatchResult cachRes = context.cachedMatch(pattern, tree);
				//	if(cachRes != Unknown) {
				//		bool res = (cachRes == Yes) && delayedCheck(context);
				//		if(DEBUG) {
				//			std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... - from cache: " << res << "\n";
				//		}
				//		return res;
				//	}

				//	// resolve without delayed checks and save result
				//	std::function<bool(MatchContext<T>&)> accept = [](MatchContext<T>& context) { return true; };
				//	bool res = match_internal(pattern, context, tree, accept);
				//	context.addToCache(pattern, tree, res);

				//	// return result + delayed checks
				//	res = res && delayedCheck(context);
				//	if(DEBUG) {
				//		std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... - added to cache: " << res << "\n";
				//	}
				//	return res;
				//}

				// for all the rest, use non-cached inner implementation
				bool res = match_internal(pattern, context, tree, delayedCheck);
				if(DEBUG) { std::cout << "Matching " << pattern << " against " << tree << " with context " << context << " ... - matched: " << res << "\n"; }

				// check correct handling of paths
				assert_eq(path, context.getCurrentPath());

				return res;
			}

			namespace {

				template <typename T, typename iterator = typename T::value_iterator>
				bool match_internal(const impl::ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end,
				                    const std::function<bool(MatchContext<T>&)>& delayedCheck) {
					switch(pattern.type) {
					#define CASE(NAME)                                                                                                                         \
						case impl::ListPattern::NAME:                                                                                                          \
							return list::match##NAME(static_cast<const pattern::impl::list::NAME&>(pattern), context, begin, end, delayedCheck);               \
							break
						CASE(Empty);
						CASE(Single);
						CASE(Variable);
						CASE(Alternative);
						CASE(Sequence);
						CASE(Repetition);
						#undef CASE
					}
					assert_fail() << "Missed a pattern type!";
					return false;
				}
			}

			template <typename T, typename iterator>
			bool match(const impl::ListPattern& pattern, MatchContext<T>& context, const iterator& begin, const iterator& end,
			           const std::function<bool(MatchContext<T>&)>& delayedCheck) {
				const bool DEBUG = false;

				if(DEBUG) {
					std::cout << "Matching " << pattern << " against " << join(", ", begin, end, print<deref<typename T::value_type>>()) << " with context "
					          << context << " ... \n";
				}
				assert_decl(auto path = context.getCurrentPath());

				// quick check length
				auto length = std::distance(begin, end);
				if(pattern.minLength > length || length > pattern.maxLength) {
					if(DEBUG) {
						std::cout << "Matching " << pattern << " against " << join(", ", begin, end, print<deref<typename T::value_type>>()) << " with context "
						          << context << " ... skipped due to length limit. \n";
					}
					return false; // will not match
				}

				//			// use cache if possible
				//			if (pattern.isVariableFree) {
				//				CachedMatchResult cachRes = context.cachedMatch(pattern, begin, end);
				//				if (cachRes != Unknown) {
				//					bool res = (cachRes == Yes) && delayedCheck(context);
				//					if (DEBUG) std::cout << "Matching " << pattern << " against " << join(", ", begin, end, print<deref<typename T::value_type>>()) << "
				//with context " << context << " ... - from cache: " << res << "\n";
				//					return res;
				//				}
				//
				//				// resolve without delayed checks and save result
				//				std::function<bool(MatchContext<T>&)> accept = [](MatchContext<T>& context) { return true; };
				//				bool res = match_internal(pattern, context, begin, end, accept);
				//				context.addToCache(pattern, begin, end, res);
				//
				//				// return result + delayed checks
				//				res = res && delayedCheck(context);
				//				if (DEBUG) std::cout << "Matching " << pattern << " against " << join(", ", begin, end, print<deref<typename T::value_type>>()) << "
				//with context " << context << " ... - added to cache: " << res << "\n";
				//				return res;
				//			}

				bool res = match_internal(pattern, context, begin, end, delayedCheck);
				if(DEBUG) {
					std::cout << "Matching " << pattern << " against " << join(", ", begin, end, print<deref<typename T::value_type>>()) << " with context "
					          << context << " ...  match: " << res << "\n";
				}

				// check correct handling of paths
				assert_eq(path, context.getCurrentPath());

				return res;
			}

		} // end namespace details


	} // end namespace pattern
	} // end namespace core
} // end namespace insieme
