/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include "insieme/core/pattern/match.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/assert.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/pattern/ir_pattern.h"

namespace insieme {
namespace core {
namespace pattern {

namespace generator {
namespace impl {

	// --- forward declarations (pimpl) ---

	struct TreeGenerator;
	typedef std::shared_ptr<TreeGenerator> TreeGeneratorPtr;

	struct ListGenerator;
	typedef std::shared_ptr<ListGenerator> ListGeneratorPtr;

} // end namespace impl
} // end namespace generator

/**
 * The class to be utilized for handling generator terms producing tree instances.
 */
class TreeGenerator : public utils::Printable {
	/**
	 * The internal data structure implementing the generator.
	 */
	generator::impl::TreeGeneratorPtr generator;

  public:
	/**
	 * A default constructed pattern matching everything -- equals the any pattern.
	 */
	TreeGenerator();

	/**
	 * A copy constructor for generators.
	 */
	TreeGenerator(const TreeGenerator& other) : generator(other.generator) {
		assert_true(generator) << "Generator pointer must not be null!";
	}

	/**
	 * A r-value move constructor for generators.
	 */
	TreeGenerator(TreeGenerator&& other) : generator(other.generator) {
		assert_true(generator) << "Generator pointer must not be null!";
	}

	/**
	 * A constructor to be utilized by factories to create a generator based on the implementation.
	 */
	TreeGenerator(const generator::impl::TreeGeneratorPtr& generator) : generator(generator) {
		assert_true(generator) << "Generator pointer must not be null!";
	}


	/**
	 * An implicit conversion to the internal implementation structure.
	 */
	operator const generator::impl::TreeGeneratorPtr&() const {
		return generator;
	}

	/**
	 * This function triggers the generation of a tree based on the given match.
	 */
	template <typename T>
	typename T::value_type generate(const Match<T>& match) const;

	/**
	 * Enable default handling of assignments.
	 */
	TreeGenerator& operator=(const TreeGenerator&) = default;

	/**
	 * Prints this generator to the given output stream -- implements the Printable interface.
	 */
	std::ostream& printTo(std::ostream& out) const;
};

/**
 * The class to be utilized for handling generator terms producing forest instances.
 */
class ListGenerator : public utils::Printable {
	/**
	 * The internal data structure implementing the generator.
	 */
	generator::impl::ListGeneratorPtr generator;

  public:
	/**
	 * A copy constructor for generators.
	 */
	ListGenerator(const ListGenerator& other) : generator(other.generator) {
		assert_true(generator) << "Generator pointer must not be null!";
	}

	/**
	 * A r-value move constructor for generators.
	 */
	ListGenerator(ListGenerator&& other) : generator(other.generator) {
		assert_true(generator) << "Generator pointer must not be null!";
	}

	/**
	 * A constructor to be utilized by factories to create a generator based on the implementation.
	 */
	ListGenerator(const generator::impl::ListGeneratorPtr& generator) : generator(generator) {
		assert_true(generator) << "Generator pointer must not be null!";
	}

	/**
	 * An implicit conversion to the internal implementation structure.
	 */
	operator const generator::impl::ListGeneratorPtr&() const {
		return generator;
	}

	/**
	 * Utilize default assignemnt operator.
	 */
	ListGenerator& operator=(const ListGenerator&) = default;

	/**
	 * This function triggers the generation of a list of trees (forest) based on the given match.
	 */
	template <typename T>
	typename T::list_type generate(const Match<T>& match) const;

	/**
	 * Prints this generator to the given output stream -- implements the Printable interface.
	 */
	std::ostream& printTo(std::ostream& out) const;
};

namespace generator {
namespace impl {


	class Generator : public utils::VirtualPrintable {
	  public:
		virtual ~Generator() {}
	};


	template <typename T>
	typename T::value_type generate(const TreeGenerator& generator, const Match<T>& match);

	template <typename T>
	typename T::list_type generate(const ListGenerator& generator, const Match<T>& match);


	struct TreeGenerator : public Generator {
		enum Type { Atom, Node, Root, Child, Element, Expression, Substitute, Let };

		const Type type;

		TreeGenerator(Type type) : type(type) {}

		template <typename T>
		typename T::value_type generate(const Match<T>& match) const {
			return generator::impl::generate(*this, match);
		}
	};

	struct ListGenerator : public Generator {
		enum Type { Empty, Single, Expression, Sequence, ForEach };

		const Type type;

		ListGenerator(Type type) : type(type) {}

		template <typename T>
		typename T::list_type generate(const Match<T>& value) const {
			return generate(*this, value);
		}
	};

	template <typename T>
	class MatchExpression : public utils::VirtualPrintable {
	  public:
		virtual MatchValue<T> eval(const Match<T>& match) const = 0;
		unsigned getNestingLevel(const Match<T>& match) const {
			return eval(match).getDepth();
		}
	};


	typedef std::shared_ptr<MatchExpression<tree_target>> TreeMatchExpressionPtr;
	typedef std::shared_ptr<MatchExpression<ptr_target>> NodeMatchExpressionPtr;


	namespace expression {

		template <typename T>
		class Tree : public MatchExpression<T> {
			MatchValue<T> value;

		  public:
			Tree(const typename T::value_type& tree) : value(tree) {}
			MatchValue<T> eval(const Match<T>& match) const {
				return value;
			}
			std::ostream& printTo(std::ostream& out) const {
				return out << value;
			}
		};

		template <typename T>
		class List : public MatchExpression<T> {
			MatchValue<T> value;

		  public:
			List(const TreeList& list) : value(list) {}
			MatchValue<T> eval(const Match<T>& match) const {
				return value;
			}
			std::ostream& printTo(std::ostream& out) const {
				return out << value;
			}
		};

		template <typename T>
		class Variable : public MatchExpression<T> {
			string name;

		  public:
			Variable(const string& name) : name(name) {}
			MatchValue<T> eval(const Match<T>& match) const {
				assert_true(match.isVarBound(name)) << "Unknown variable encountered!";
				return match.getVarBinding(name);
			}
			std::ostream& printTo(std::ostream& out) const {
				return out << "$" << name;
			}
		};

		template <typename T>
		class Element : public MatchExpression<T> {
			typedef std::shared_ptr<MatchExpression<T>> ExpressionPtr;

			ExpressionPtr subExpr;
			unsigned index;

		  public:
			Element(const ExpressionPtr& expression, unsigned index) : subExpr(expression), index(index) {}
			MatchValue<T> eval(const Match<T>& match) const {
				MatchValue<T>&& value = subExpr->eval(match);
				assert_gt(value.getDepth(), 0) << "Cannot access element of a tree.";
				return value.getValue(index);
			}
			std::ostream& printTo(std::ostream& out) const {
				return out << *subExpr << "[" << index << "]";
			}
		};

		template <typename T>
		class Transform : public MatchExpression<T> {
		  public:
			typedef std::function<MatchValue<T>(const MatchValue<T>&)> ValueOperation;

		  private:
			typedef std::shared_ptr<MatchExpression<T>> ExpressionPtr;
			ExpressionPtr subExpr;
			ValueOperation transformation;
			string name;

		  public:
			Transform(const ExpressionPtr& expression, const ValueOperation& transform, const string& name = "f")
			    : subExpr(expression), transformation(transform), name(name) {}
			MatchValue<T> eval(const Match<T>& match) const {
				return transformation(subExpr->eval(match));
			}
			std::ostream& printTo(std::ostream& out) const {
				return out << name << "(" << *subExpr << ")";
			}
		};

		template <typename T>
		class Constructor : public MatchExpression<T> {
		  public:
			typedef std::function<MatchValue<T>(const Match<T>&)> Producer;

		  private:
			Producer producer;
			string name;

		  public:
			Constructor(const Producer& producer, const string& name) : producer(producer), name(name) {}
			MatchValue<T> eval(const Match<T>& match) const {
				return producer(match);
			}
			std::ostream& printTo(std::ostream& out) const {
				return out << name;
			}
		};

		template <typename T>
		class Combine : public MatchExpression<T> {
		  public:
			typedef std::function<typename T::value_type(const vector<typename T::value_type>&)> Combinator;

		  private:
			vector<TreeGeneratorPtr> subGenerators;
			Combinator combinator;
			string name;

		  public:
			Combine(const vector<pattern::TreeGenerator>& subGenerator, const Combinator& combinator, string name)
			    : subGenerators(subGenerator.begin(), subGenerator.end()), combinator(combinator), name(name) {}

			Combine(const vector<TreeGeneratorPtr>& subGenerator, const Combinator& combinator, string name)
			    : subGenerators(subGenerator), combinator(combinator), name(name) {}

			MatchValue<T> eval(const Match<T>& match) const {
				// eval sub-generator expressions
				vector<typename T::value_type> ops;
				for_each(subGenerators, [&](const TreeGeneratorPtr& cur) { ops.push_back(generate(*cur, match)); });
				return combinator(ops);
			}

			std::ostream& printTo(std::ostream& out) const {
				return out << name << "(" << join(",", subGenerators, print<deref<TreeGeneratorPtr>>()) << ")";
			}
		};
	}

	namespace tree {

		struct Atom : public TreeGenerator {
			const core::NodePtr node;
			const TreePtr tree;

			Atom(const core::NodePtr& node) : TreeGenerator(TreeGenerator::Atom), node(node) {}
			Atom(const TreePtr& tree) : TreeGenerator(TreeGenerator::Atom), tree(tree) {}

			std::ostream& printTo(std::ostream& out) const {
				if(tree) { return out << tree; }
				return out << node;
			}
		};

		struct Root : public TreeGenerator {
			Root() : TreeGenerator(TreeGenerator::Root) {}
			std::ostream& printTo(std::ostream& out) const {
				return out << "root";
			}
		};

		struct Expression : public TreeGenerator {
			const NodeMatchExpressionPtr node_expr;
			const TreeMatchExpressionPtr tree_expr;

			Expression(const NodeMatchExpressionPtr& expr) : TreeGenerator(TreeGenerator::Expression), node_expr(expr) {}

			Expression(const TreeMatchExpressionPtr& expr) : TreeGenerator(TreeGenerator::Expression), tree_expr(expr) {}

			std::ostream& printTo(std::ostream& out) const {
				return (node_expr) ? (out << *node_expr) : (out << *tree_expr);
			}
		};

		struct Child : public TreeGenerator {
			TreeGeneratorPtr treeGen;
			unsigned childIndex;

			Child(const TreeGeneratorPtr& tree, unsigned childIndex) : TreeGenerator(TreeGenerator::Child), treeGen(tree), childIndex(childIndex) {}

			std::ostream& printTo(std::ostream& out) const {
				return out << *treeGen << "." << childIndex;
			}
		};

		struct Element : public TreeGenerator {
			ListGeneratorPtr listGen;
			unsigned index;

			Element(const ListGeneratorPtr& list, unsigned index) : TreeGenerator(TreeGenerator::Element), listGen(list), index(index) {}

			std::ostream& printTo(std::ostream& out) const {
				return out << *listGen << "[" << index << "]";
			}
		};

		struct Node : public TreeGenerator {
			int id;
			int type;
			ListGeneratorPtr childGen;

			Node(core::NodeType type, const ListGeneratorPtr& childGen) : TreeGenerator(TreeGenerator::Node), id(-1), type(type), childGen(childGen) {}
			Node(char id, const ListGeneratorPtr& childGen) : TreeGenerator(TreeGenerator::Node), id(id), type(-1), childGen(childGen) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if(id != -1) { return out << "(" << id << "|" << *childGen << ")"; }
				return out << "(" << ((core::NodeType)type) << "|" << *childGen << ")";
			}

			core::NodeType getNodeType() const {
				assert_ne(type, -1) << "Node is not generating an IR Node: " << *this;
				return core::NodeType(type);
			}

			char getId() const {
				assert_ne(id, -1) << "Node is not generating a Tree Node: " << *this;
				return char(id);
			}
		};

		struct Substitute : public TreeGenerator {
			TreeGeneratorPtr tree;
			TreeGeneratorPtr replacement;
			TreeGeneratorPtr var;

			Substitute(const TreeGeneratorPtr& tree, const TreeGeneratorPtr& replacement, const TreeGeneratorPtr& var)
			    : TreeGenerator(TreeGenerator::Substitute), tree(tree), replacement(replacement), var(var) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *tree << "{" << *replacement << "/" << *var << "}";
			}
		};

		struct Let : public TreeGenerator {
			string name;
			TreeGeneratorPtr value;
			TreeGeneratorPtr tree;
			Let(const string& name, const TreeGeneratorPtr& value, const TreeGeneratorPtr& tree)
			    : TreeGenerator(TreeGenerator::Let), name(name), value(value), tree(tree) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "let " << name << "=" << *value << " in " << *tree;
			}
		};
	}


	namespace list {

		struct Empty : public ListGenerator {
			Empty() : ListGenerator(ListGenerator::Empty) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "[]";
			}
		};

		struct Single : public ListGenerator {
			TreeGeneratorPtr treeGen;

			Single(const TreeGeneratorPtr& tree) : ListGenerator(ListGenerator::Single), treeGen(tree) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *treeGen;
			}
		};

		struct Expression : public ListGenerator {
			typedef std::shared_ptr<MatchExpression<tree_target>> TreeMatchExpressionPtr;
			typedef std::shared_ptr<MatchExpression<ptr_target>> NodeMatchExpressionPtr;

			const NodeMatchExpressionPtr node_expr;
			const TreeMatchExpressionPtr tree_expr;

			Expression(const NodeMatchExpressionPtr& expr) : ListGenerator(ListGenerator::Expression), node_expr(expr) {}

			Expression(const TreeMatchExpressionPtr& expr) : ListGenerator(ListGenerator::Expression), tree_expr(expr) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return (node_expr) ? (out << *node_expr) : (out << *tree_expr);
			}
		};

		struct Sequence : public ListGenerator {
			ListGeneratorPtr listA;
			ListGeneratorPtr listB;

			Sequence(const ListGeneratorPtr& listA, const ListGeneratorPtr& listB) : ListGenerator(ListGenerator::Sequence), listA(listA), listB(listB) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << *listA << "," << *listB;
			}
		};

		struct ForEach : public ListGenerator {
			string varName;
			TreeGeneratorPtr tree;

			typedef std::shared_ptr<MatchExpression<tree_target>> TreeMatchExpressionPtr;
			typedef std::shared_ptr<MatchExpression<ptr_target>> NodeMatchExpressionPtr;

			const NodeMatchExpressionPtr node_list;
			const TreeMatchExpressionPtr tree_list;

			ForEach(const string& name, const NodeMatchExpressionPtr& list, const TreeGeneratorPtr& tree)
			    : ListGenerator(ListGenerator::ForEach), varName(name), tree(tree), node_list(list) {}

			ForEach(const string& name, const TreeMatchExpressionPtr& list, const TreeGeneratorPtr& tree)
			    : ListGenerator(ListGenerator::ForEach), varName(name), tree(tree), tree_list(list) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				out << "for " << varName << " in ";
				if(node_list) {
					out << *node_list;
				} else {
					out << *tree_list;
				}
				return out << " get " << *tree;
			}
		};
	}

} // end namespace impl

namespace detail {

	// -- Utilities ----------------------------------------------------

	// -- some match value operations ----------------------------------


	template <typename T>
	MatchValue<T> reverse(const MatchValue<T>& value) {
		assert_eq(value.getDepth(), 1) << "Data is not a list!";
		auto list = value.getList();
		std::reverse(list.begin(), list.end());
		return MatchValue<T>(list);
	}

	template <typename T>
	MatchValue<T> listChildren(const MatchValue<T>& value) {
		assert_eq(value.getDepth(), 0) << "Data is not a tree!";
		return MatchValue<T>(value.getValue().getChildList());
	}

} // end namespace details


// -------------------------------------------------------------------------------------
//   Generator Factories
// -------------------------------------------------------------------------------------


// -- expressions --

template <typename T>
inline std::shared_ptr<impl::MatchExpression<T>> reverse(const std::shared_ptr<impl::MatchExpression<T>>& subexpr) {
	typename impl::expression::Transform<T>::ValueOperation op = &(detail::reverse<T>);
	return std::make_shared<impl::expression::Transform<T>>(subexpr, op, "reverse");
}

template <typename T>
inline std::shared_ptr<impl::MatchExpression<T>> childrenOf(const std::shared_ptr<impl::MatchExpression<T>>& subexpr) {
	typename impl::expression::Transform<T>::ValueOperation op = &(detail::listChildren<T>);
	return std::make_shared<impl::expression::Transform<T>>(subexpr, op, "childrenOf");
}

inline impl::NodeMatchExpressionPtr construct(const std::function<MatchValue<ptr_target>(const Match<ptr_target>&)>& producer, const string& name) {
	return std::make_shared<impl::expression::Constructor<ptr_target>>(producer, name);
}


// -- trees and lists --

extern const TreeGenerator root;
extern const ListGenerator empty;

inline TreeGenerator atom(const core::NodePtr& node) {
	return TreeGenerator(std::make_shared<impl::tree::Atom>(node));
}

inline TreeGenerator atom(const TreePtr& tree) {
	return TreeGenerator(std::make_shared<impl::tree::Atom>(tree));
}

inline ListGenerator single(const TreeGenerator& tree) {
	return ListGenerator(std::make_shared<impl::list::Single>(tree));
}

template <typename T>
inline ListGenerator single(const std::shared_ptr<impl::MatchExpression<T>>& expr) {
	return single(std::make_shared<impl::tree::Expression>(expr));
}

inline TreeGenerator node(const ListGenerator& generator) {
	return TreeGenerator(std::make_shared<impl::tree::Node>(0, generator));
}

inline TreeGenerator node(const int id, const ListGenerator& generator) {
	return TreeGenerator(std::make_shared<impl::tree::Node>(id, generator));
}

inline TreeGenerator node(const core::NodeType type, const ListGenerator& generator) {
	return TreeGenerator(std::make_shared<impl::tree::Node>(type, generator));
}

inline TreeGenerator substitute(const TreeGenerator& tree, const TreeGenerator& target, const TreeGenerator& replacement) {
	return TreeGenerator(std::make_shared<impl::tree::Substitute>(tree, replacement, target));
}

inline TreeGenerator let(const string& var, const TreeGenerator& value, const TreeGenerator& tree) {
	return TreeGenerator(std::make_shared<impl::tree::Let>(var, value, tree));
}

template <typename target = ptr_target>
inline std::shared_ptr<impl::MatchExpression<target>> varExpr(const std::string& name) {
	return std::make_shared<impl::expression::Variable<target>>(name);
}

template <typename target = ptr_target>
inline TreeGenerator var(const std::string& name) {
	return TreeGenerator(std::make_shared<impl::tree::Expression>(varExpr<target>(name)));
}


template <typename target = ptr_target>
inline TreeGenerator treeVar(const std::string& name) {
	return TreeGenerator(std::make_shared<impl::tree::Expression>(varExpr<target>(name)));
}

template <typename target = ptr_target>
inline ListGenerator listVar(const std::string& name) {
	return ListGenerator(std::make_shared<impl::list::Expression>(varExpr<target>(name)));
}

template <typename target>
inline TreeGenerator treeExpr(const std::shared_ptr<impl::MatchExpression<target>>& expr) {
	return TreeGenerator(std::make_shared<impl::tree::Expression>(expr));
}

template <typename target>
inline ListGenerator listExpr(const std::shared_ptr<impl::MatchExpression<target>>& expr) {
	return ListGenerator(std::make_shared<impl::list::Expression>(expr));
}

template <typename target>
inline ListGenerator forEach(const std::string& name, const std::shared_ptr<impl::MatchExpression<target>>& list, const TreeGenerator& tree) {
	return ListGenerator(std::make_shared<impl::list::ForEach>(name, list, tree));
}

template <typename target>
inline ListGenerator forEach(const std::string& name, const std::shared_ptr<impl::MatchExpression<target>>& list,
                             const std::shared_ptr<impl::MatchExpression<target>>& tree) {
	return forEach(name, list, treeExpr(tree));
}


namespace impl {

	// -------------------------------------------------------------------------------------
	//   Generator Implementation
	// -------------------------------------------------------------------------------------

	template <typename T>
	inline typename T::value_type generate(const TreeGeneratorPtr& generator, const Match<T>& value) {
		return generate(*generator.get(), value);
	}

	template <typename T>
	inline typename T::list_type generate(const ListGeneratorPtr& generator, const Match<T>& value) {
		return generate(*generator.get(), value);
	}

	inline MatchValue<ptr_target> eval(const std::shared_ptr<MatchExpression<ptr_target>>& node_expr,
	                                   const std::shared_ptr<MatchExpression<tree_target>>& tree_expr, const Match<ptr_target>& match) {
		assert_true(node_expr) << "Missing node expression!";
		return node_expr->eval(match);
	}

	inline MatchValue<tree_target> eval(const std::shared_ptr<MatchExpression<ptr_target>>& node_expr,
	                                    const std::shared_ptr<MatchExpression<tree_target>>& tree_expr, const Match<tree_target>& match) {
		assert_true(tree_expr) << "Missing tree expression!";
		return tree_expr->eval(match);
	}


	#define GENERATE(NAME)                                                                                                                                     \
		template <typename T>                                                                                                                                  \
		typename T::value_type generate##NAME##Tree(const tree::NAME& generator, const Match<T>& match)

	GENERATE(Atom) {
		return generator.node;
	}

	inline TreePtr generateAtomTree(const tree::Atom& generator, const Match<tree_target>& match) {
		return generator.tree;
	}

	GENERATE(Node) {
		core::IRBuilder builder(match.getRoot()->getNodeManager());
		return builder.get(generator.getNodeType(), generate(generator.childGen, match));
	}

	inline TreePtr generateNodeTree(const tree::Node& generator, const Match<tree_target>& match) {
		return makeTree(generator.getId(), generate(generator.childGen, match));
	}

	GENERATE(Root) {
		return match.getRoot();
	}

	GENERATE(Expression) {
		MatchValue<T>&& value = eval(generator.node_expr, generator.tree_expr, match);
		assert_eq(value.getDepth(), 0);
		return value.getValue();
	}

	GENERATE(Child) {
		return generate(generator.treeGen, match)->getChildList()[generator.childIndex];
	}

	GENERATE(Element) {
		return generate(generator.listGen, match)[generator.index];
	}

	GENERATE(Substitute) {
		// eval sub-terms
		core::NodePtr target = generate(generator.tree, match);
		core::NodePtr var = generate(generator.var, match);
		core::NodePtr replacement = generate(generator.replacement, match);

		// apply substitution
		return core::transform::replaceAll(target->getNodeManager(), target, var, replacement);
	}

	namespace {


		TreePtr substitute(const TreePtr& target, const TreePtr& replacement, const TreePtr& var) {
			// test current node
			if(target == var) { return replacement; }

			// replace nodes recursively
			TreeList list = ::transform(target->getSubTrees(), [&](const TreePtr& tree) { return substitute(tree, replacement, var); });

			return makeTree(target->getId(), list);
		}
	}

	inline TreePtr generateSubstituteTree(const tree::Substitute& sub, const Match<tree_target>& match) {
		// eval sub-terms
		TreePtr a = generate(sub.tree, match);
		TreePtr b = generate(sub.replacement, match);
		TreePtr c = generate(sub.var, match);

		// apply substitution
		return substitute(a, b, c);
	}

	GENERATE(Let) {
		// bind result of value generator to name within private copy of match
		Match<T> tmp = match;
		tmp.bindVar(generator.name, generate(generator.value, match));
		return generate(generator.tree, tmp);
	}

	#undef GENERATE


	#define GENERATE(NAME)                                                                                                                                     \
		template <typename T>                                                                                                                                  \
		typename T::list_type generate##NAME##List(const list::NAME& generator, const Match<T>& match)

	GENERATE(Empty) {
		return typename T::list_type();
	}

	GENERATE(Single) {
		return toVector(generate(generator.treeGen, match));
	}

	GENERATE(Expression) {
		MatchValue<T>&& value = eval(generator.node_expr, generator.tree_expr, match);
		assert_eq(value.getDepth(), 1);
		return value.getList();
	}

	GENERATE(Sequence) {
		typename T::list_type resA = generate(generator.listA, match);
		typename T::list_type resB = generate(generator.listB, match);
		resA.insert(resA.end(), resB.begin(), resB.end());
		return resA;
	}

	GENERATE(ForEach) {
		// obtain list of values
		MatchValue<T> all = eval(generator.node_list, generator.tree_list, match);
		assert_gt(all.getDepth(), 0) << "Cannot apply for-each on scalar!";

		// create one entry per element within the list
		typename T::list_type res;
		::transform(all.getValues(), std::back_inserter(res), [&](const MatchValue<T>& cur) {
			Match<T> extended = match;
			extended.bindVar(generator.varName, cur);
			return generate(generator.tree, extended);
		});
		return res;
	}


	#undef GENERATE

	template <typename T>
	typename T::value_type generate(const TreeGenerator& generator, const Match<T>& match) {
		switch(generator.type) {
		#define CASE(NAME)                                                                                                                                     \
			case TreeGenerator::NAME: return generate##NAME##Tree(static_cast<const tree::NAME&>(generator), match)
			CASE(Atom);
			CASE(Node);
			CASE(Root);
			CASE(Child);
			CASE(Element);
			CASE(Expression);
			CASE(Substitute);
			CASE(Let);
			#undef CASE
		}
		assert_fail() << "Unsupported tree-generator construct encountered!";
		return typename T::value_type();
	}

	template <typename T>
	typename T::list_type generate(const ListGenerator& generator, const Match<T>& match) {
		switch(generator.type) {
		#define CASE(NAME)                                                                                                                                     \
			case ListGenerator::NAME: return generate##NAME##List(static_cast<const list::NAME&>(generator), match)
			CASE(Empty);
			CASE(Single);
			CASE(Expression);
			CASE(Sequence);
			CASE(ForEach);
			#undef CASE
		}
		assert_fail() << "Unsupported list-generator construct encountered!";
		return typename T::list_type();
	}

} // end namespace impl

} // end namespace generator

// implements the generic member function of the tree generator
template <typename T>
typename T::value_type TreeGenerator::generate(const Match<T>& match) const {
	return generator->template generate<T>(match);
}

// implements the generic member function of the list generator
template <typename T>
typename T::list_type ListGenerator::generate(const Match<T>& match) const {
	return generator->template generate<T>(match);
}

// -- some operators for the list and tree generators --

inline ListGenerator operator<<(const ListGenerator& a, const ListGenerator& b) {
	return ListGenerator(std::make_shared<generator::impl::list::Sequence>(a, b));
}
inline ListGenerator operator<<(const TreeGenerator& a, const ListGenerator& b) {
	return ListGenerator(std::make_shared<generator::impl::list::Sequence>(generator::single(a), b));
}
inline ListGenerator operator<<(const ListGenerator& a, const TreeGenerator& b) {
	return ListGenerator(std::make_shared<generator::impl::list::Sequence>(a, generator::single(b)));
}
inline ListGenerator operator<<(const TreeGenerator& a, const TreeGenerator& b) {
	return ListGenerator(std::make_shared<generator::impl::list::Sequence>(generator::single(a), generator::single(b)));
}


} // end namespace pattern
} // end namespace core
} // end namespace insieme
