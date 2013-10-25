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

#include "insieme/transform/pattern/match.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace transform {
namespace pattern {

	class Generator;

	class TreeGenerator;
	typedef std::shared_ptr<TreeGenerator> TreeGeneratorPtr;

	class ListGenerator;
	typedef std::shared_ptr<ListGenerator> ListGeneratorPtr;

	class Generator : public utils::VirtualPrintable {
	public:
		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};

	namespace generator {

		template<typename T>
		typename T::value_type generate(const TreeGenerator& generator, const Match<T>& match);

		template<typename T>
		typename T::list_type generate(const ListGenerator& generator, const Match<T>& match);

	} // end namespace generator

	struct TreeGenerator : public Generator {
		enum Type {
			Atom, Node, Root, Child, Element, Expression, Substitute, Let
		};

		const Type type;

		TreeGenerator(Type type) : type(type) {}

		template<typename T>
		typename T::value_type generate(const Match<T>& match) const {
			return generator::generate(*this, match);
		}

	};

	struct ListGenerator : public Generator {
		enum Type {
			Empty, Single, Expression, Sequence, ForEach
		};

		const Type type;

		ListGenerator(Type type) : type(type) {}

		template<typename T>
		typename T::list_type generate(const Match<T>& value) {
			return generate(*this, value);
		}
	};

	template<typename T>
	class MatchExpression : public utils::VirtualPrintable {
	public:
		virtual MatchValue<T> eval(const Match<T>& match) const = 0;
		unsigned getNestingLevel(const Match<T>& match) const { return eval(match).getDepth(); }
	};


	typedef std::shared_ptr<MatchExpression<tree_target>> TreeMatchExpressionPtr;
	typedef std::shared_ptr<MatchExpression<ptr_target>> NodeMatchExpressionPtr;


namespace generator {

	namespace expression {

		template<typename T>
		class Tree : public MatchExpression<T> {
			MatchValue<T> value;
		public:
			Tree(const typename T::value_type& tree) : value(tree) {}
			MatchValue<T> eval(const Match<T>& match) const { return value; }
			std::ostream& printTo(std::ostream& out) const { return out << value; }
		};

		template<typename T>
		class List : public MatchExpression<T> {
			MatchValue<T> value;
		public:
			List(const TreeList& list) : value(list) {}
			MatchValue<T> eval(const Match<T>& match) const { return value; }
			std::ostream& printTo(std::ostream& out) const { return out << value; }
		};

		template<typename T>
		class Variable : public MatchExpression<T> {
			string name;
		public:
			Variable(const string& name) : name(name) {}
			MatchValue<T> eval(const Match<T>& match) const {
				assert(match.isVarBound(name) && "Unknown variable encountered!");
				return match.getVarBinding(name);
			}
			std::ostream& printTo(std::ostream& out) const { return out << "$" << name; }
		};

		template<typename T>
		class Element : public MatchExpression<T> {
			typedef std::shared_ptr<MatchExpression<T>> ExpressionPtr;

			ExpressionPtr subExpr;
			unsigned index;
		public:
			Element(const ExpressionPtr& expression, unsigned index)
				: subExpr(expression), index(index) {}
			MatchValue<T> eval(const Match<T>& match) const {
				MatchValue<T>&& value = subExpr->eval(match);
				assert(value.getDepth() > 0 && "Cannot access element of a tree.");
				return value.getValue(index);
			}
			std::ostream& printTo(std::ostream& out) const { return out << *subExpr << "[" << index << "]"; }
		};

		template<typename T>
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
			std::ostream& printTo(std::ostream& out) const { return out << name << "(" << *subExpr << ")"; }
		};

		template<typename T>
		class Constructor : public MatchExpression<T> {
		public:
			typedef std::function<MatchValue<T>(const Match<T>&)> Producer;
		private:
			Producer producer;
			string name;
		public:
			Constructor(const Producer& producer, const string& name)
				: producer(producer), name(name) {}
			MatchValue<T> eval(const Match<T>& match) const {
				return producer(match);
			}
			std::ostream& printTo(std::ostream& out) const { return out << name; }
		};

		template<typename T>
		class Combine : public MatchExpression<T> {
		public:
			typedef std::function<typename T::value_type(const vector<typename T::value_type>&)> Combinator;
		private:
			vector<TreeGeneratorPtr> subGenerators;
			Combinator combinator;
			string name;
		public:
			Combine(const vector<TreeGeneratorPtr>& subGenerator, const Combinator& combinator, string name)
				: subGenerators(subGenerator), combinator(combinator), name(name) {}

			MatchValue<T> eval(const Match<T>& match) const {
				// eval sub-generator expressions
				vector<typename T::value_type> ops;
				for_each(subGenerators, [&](const TreeGeneratorPtr& cur) {
					ops.push_back(generate(*cur, match));
				});
				return combinator(ops);
			}

			std::ostream& printTo(std::ostream& out) const { return out << name << "(" << join(",",subGenerators, print<deref<TreeGeneratorPtr>>()) << ")"; }
		};
	}

	namespace tree {

		struct Atom : public TreeGenerator {
			const core::NodePtr node;
			const TreePtr tree;

			Atom(const core::NodePtr& node) : TreeGenerator(TreeGenerator::Atom), node(node) {}
			Atom(const TreePtr& tree) : TreeGenerator(TreeGenerator::Atom), tree(tree) {}

			std::ostream& printTo(std::ostream& out) const { return out << tree; }
		};

		struct Root : public TreeGenerator {
			Root() : TreeGenerator(TreeGenerator::Root) {}
			std::ostream& printTo(std::ostream& out) const { return out << "root"; }
		};

		struct Expression : public TreeGenerator {

			const NodeMatchExpressionPtr node_expr;
			const TreeMatchExpressionPtr tree_expr;

			Expression(const NodeMatchExpressionPtr& expr)
				: TreeGenerator(TreeGenerator::Expression), node_expr(expr) {}

			Expression(const TreeMatchExpressionPtr& expr)
				: TreeGenerator(TreeGenerator::Expression), tree_expr(expr) {}

			std::ostream& printTo(std::ostream& out) const {
				return (node_expr)? (out << *node_expr) : (out << *tree_expr); }
		};

		struct Child : public TreeGenerator {
			TreeGeneratorPtr treeGen;
			unsigned childIndex;

			Child(const TreeGeneratorPtr& tree, unsigned childIndex)
				: TreeGenerator(TreeGenerator::Child), treeGen(tree), childIndex(childIndex) {}

			std::ostream& printTo(std::ostream& out) const { return out << *treeGen << "." << childIndex; }
		};

		struct Element : public TreeGenerator {
			ListGeneratorPtr listGen;
			unsigned index;

			Element(const ListGeneratorPtr& list, unsigned index)
				: TreeGenerator(TreeGenerator::Element), listGen(list), index(index) {}

			std::ostream& printTo(std::ostream& out) const { return out << *listGen << "[" << index << "]"; }
		};

		struct Node : public TreeGenerator {
			int id;
			int type;
			ListGeneratorPtr childGen;

			Node(core::NodeType type, const ListGeneratorPtr& childGen)
				: TreeGenerator(TreeGenerator::Node), id(-1), type(type), childGen(childGen) {}
			Node(char id, const ListGeneratorPtr& childGen)
				: TreeGenerator(TreeGenerator::Node), id(id), type(-1), childGen(childGen) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if (id != -1) {
					return out << "(" << id << "|" << *childGen << ")";
				}
				return out << "(" << ((core::NodeType)type) << "|" << *childGen << ")";
			}

			core::NodeType getNodeType() const {
				assert(type != -1 && "Node is not generating an IR Node!");
				return core::NodeType(type);
			}

			char getId() const {
				assert(id != -1 && "Node is not generating a Tree Node!");
				return char(id);
			}
		};

		struct Substitute : public TreeGenerator {
			TreeGeneratorPtr tree;
			TreeGeneratorPtr replacement;
			TreeGeneratorPtr var;

			Substitute(const TreeGeneratorPtr& tree, const TreeGeneratorPtr& replacement, const TreeGeneratorPtr& var)
				: TreeGenerator(TreeGenerator::Substitute), tree(tree), replacement(replacement), var(var) {}

			virtual std::ostream& printTo(std::ostream& out) const { return out << *tree << "{" << *replacement << "/" << *var << "}"; }
		};

		struct Let : public TreeGenerator {
			string name;
			TreeGeneratorPtr value;
			TreeGeneratorPtr tree;
			Let(const string& name, const TreeGeneratorPtr& value, const TreeGeneratorPtr& tree)
				: TreeGenerator(TreeGenerator::Let), name(name), value(value), tree(tree) {}

			virtual std::ostream& printTo(std::ostream& out) const { return out << "let " << name << "=" << *value << " in " << *tree; }
		};

	}


	namespace list {

		struct Empty : public ListGenerator {

			Empty() : ListGenerator(ListGenerator::Empty) {}

			virtual std::ostream& printTo(std::ostream& out) const { return out << "[]"; }
		};

		struct Single : public ListGenerator {
			TreeGeneratorPtr treeGen;

			Single(const TreeGeneratorPtr& tree)
				: ListGenerator(ListGenerator::Single), treeGen(tree) {}

			virtual std::ostream& printTo(std::ostream& out) const { return out << *treeGen; }
		};

		struct Expression : public ListGenerator {

			typedef std::shared_ptr<MatchExpression<tree_target>> TreeMatchExpressionPtr;
			typedef std::shared_ptr<MatchExpression<ptr_target>> NodeMatchExpressionPtr;

			const NodeMatchExpressionPtr node_expr;
			const TreeMatchExpressionPtr tree_expr;

			Expression(const NodeMatchExpressionPtr& expr)
				: ListGenerator(ListGenerator::Expression), node_expr(expr) {}

			Expression(const TreeMatchExpressionPtr& expr)
				: ListGenerator(ListGenerator::Expression), tree_expr(expr) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				return (node_expr)? (out << *node_expr) : (out << *tree_expr);
			}
		};

		struct Sequence : public ListGenerator {
			ListGeneratorPtr listA;
			ListGeneratorPtr listB;

			Sequence(const ListGeneratorPtr& listA, const ListGeneratorPtr& listB)
				: ListGenerator(ListGenerator::Sequence), listA(listA), listB(listB) {}

			virtual std::ostream& printTo(std::ostream& out) const { return out << *listA << "," << *listB; }
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
				if (node_list) {
					out << *node_list;
				} else {
					out << *tree_list;
				}
				return out << " get " << *tree;
			}
		};
	}


	// -- Utilities ----------------------------------------------------

	// -- some match value operations ----------------------------------

	namespace impl {

		template<typename T>
		MatchValue<T> reverse(const MatchValue<T>& value) {
			assert(value.getDepth() == 1 && "Data is not a list!");
			auto list = value.getList();
			std::reverse(list.begin(), list.end());
			return MatchValue<T>(list);
		}

		template<typename T>
		MatchValue<T> listChildren(const MatchValue<T>& value) {
			assert(value.getDepth() == 0 && "Data is not a tree!");
			return MatchValue<T>(value.getValue().getChildList());
		}

	}

	template<typename T>
	inline std::shared_ptr<MatchExpression<T>> reverse(const std::shared_ptr<MatchExpression<T>>& subexpr) {
		typename expression::Transform<T>::ValueOperation op = &(impl::reverse<T>);
		return std::make_shared<expression::Transform<T>>(subexpr, op , "reverse");
	}

	template<typename T>
	inline std::shared_ptr<MatchExpression<T>> childrenOf(const std::shared_ptr<MatchExpression<T>>& subexpr) {
		typename expression::Transform<T>::ValueOperation op = &(impl::listChildren<T>);
		return std::make_shared<expression::Transform<T>>(subexpr, op , "childrenOf");
	}

	inline NodeMatchExpressionPtr construct(const std::function<MatchValue<ptr_target>(const Match<ptr_target>&)>& producer, const string& name) {
		return std::make_shared<expression::Constructor<ptr_target>>(producer, name);
	}

	// -- generator factories ------------------------------------------

	extern const TreeGeneratorPtr root;
	extern const ListGeneratorPtr empty;

	inline TreeGeneratorPtr atom(const core::NodePtr& node) {
		return std::make_shared<tree::Atom>(node);
	}

	inline TreeGeneratorPtr atom(const TreePtr& tree) {
		return std::make_shared<tree::Atom>(tree);
	}

	inline ListGeneratorPtr single(const TreeGeneratorPtr& tree) {
		return std::make_shared<list::Single>(tree);
	}

	template<typename T>
	inline ListGeneratorPtr single(const std::shared_ptr<MatchExpression<T>>& expr) {
		return single(std::make_shared<tree::Expression>(expr));
	}

	inline TreeGeneratorPtr node(const ListGeneratorPtr& generator) {
		return std::make_shared<tree::Node>(0, generator);
	}

	inline TreeGeneratorPtr node(const int id, const ListGeneratorPtr& generator) {
		return std::make_shared<tree::Node>(id, generator);
	}

	inline TreeGeneratorPtr node(const core::NodeType type, const ListGeneratorPtr& generator) {
		return std::make_shared<tree::Node>(type, generator);
	}

	inline TreeGeneratorPtr substitute(const TreeGeneratorPtr& tree, const TreeGeneratorPtr& target, const TreeGeneratorPtr& replacement) {
		return std::make_shared<tree::Substitute>(tree, replacement, target);
	}

	inline TreeGeneratorPtr let(const string& var, const TreeGeneratorPtr& value, const TreeGeneratorPtr& tree) {
		return std::make_shared<tree::Let>(var, value, tree);
	}

	template<typename target = ptr_target>
	inline std::shared_ptr<MatchExpression<target>> varExpr(const std::string& name) {
		return std::make_shared<expression::Variable<target>>(name);
	}

	template<typename target = ptr_target>
	inline TreeGeneratorPtr var(const std::string& name) {
		return std::make_shared<tree::Expression>(varExpr<target>(name));
	}

	template<typename target = ptr_target>
	inline TreeGeneratorPtr treeVar(const std::string& name) {
		return std::make_shared<tree::Expression>(varExpr<target>(name));
	}

	template<typename target = ptr_target>
	inline ListGeneratorPtr listVar(const std::string& name) {
		return std::make_shared<list::Expression>(varExpr<target>(name));
	}

	template<typename target>
	inline TreeGeneratorPtr treeExpr(const std::shared_ptr<MatchExpression<target>>& expr) {
		return std::make_shared<tree::Expression>(expr);
	}

	template<typename target>
	inline ListGeneratorPtr listExpr(const std::shared_ptr<MatchExpression<target>>& expr) {
		return std::make_shared<list::Expression>(expr);
	}

	template<typename target>
	inline ListGeneratorPtr forEach(const std::string& name, const std::shared_ptr<MatchExpression<target>>& list, const TreeGeneratorPtr& tree) {
		return std::make_shared<list::ForEach>(name, list, tree);
	}

	template<typename target>
	inline ListGeneratorPtr forEach(const std::string& name, const std::shared_ptr<MatchExpression<target>>& list, const std::shared_ptr<MatchExpression<target>>& tree) {
		return forEach(name, list, treeExpr(tree));
	}


	// -------------------------------------------------------------------------------------
	//   Generator Implementation
	// -------------------------------------------------------------------------------------

	template<typename T>
	inline typename T::value_type generate(const TreeGeneratorPtr& generator, const Match<T>& value) {
		return generate(*generator.get(), value);
	}

	template<typename T>
	inline typename T::list_type generate(const ListGeneratorPtr& generator, const Match<T>& value) {
		return generate(*generator.get(), value);
	}

	inline MatchValue<ptr_target> eval(
				const std::shared_ptr<MatchExpression<ptr_target>>& node_expr,
				const std::shared_ptr<MatchExpression<tree_target>>& tree_expr,
				const Match<ptr_target>& match
			) {
		assert(node_expr && "Missing node expression!");
		return node_expr->eval(match);
	}

	inline MatchValue<tree_target> eval(
				const std::shared_ptr<MatchExpression<ptr_target>>& node_expr,
				const std::shared_ptr<MatchExpression<tree_target>>& tree_expr,
				const Match<tree_target>& match
			) {
		assert(tree_expr && "Missing tree expression!");
		return tree_expr->eval(match);
	}



	#define GENERATE(NAME) \
		template<typename T> \
		typename T::value_type generate ## NAME ## Tree(const tree::NAME& generator, const Match<T>& match)

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
			assert(value.getDepth() == 0);
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
				if (target == var) {
					return replacement;
				}

				// replace nodes recursively
				TreeList list = ::transform(target->getSubTrees(), [&](const TreePtr& tree) {
					return substitute(tree, replacement, var);
				});

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


	#define GENERATE(NAME) \
		template<typename T> \
		typename T::list_type generate ## NAME ## List(const list::NAME& generator, const Match<T>& match)

		GENERATE(Empty) {
			return typename T::list_type();
		}

		GENERATE(Single) {
			return toVector(generate(generator.treeGen, match));
		}

		GENERATE(Expression) {
			MatchValue<T>&& value = eval(generator.node_expr, generator.tree_expr, match);
			assert(value.getDepth() == 1);
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
			assert(all.getDepth() > 0 && "Cannot apply for-each on scalar!");

			// create one entry per element within the list
			typename T::list_type res;
			::transform(all.getValues(), std::back_inserter(res), [&](const MatchValue<T>& cur){
				Match<T> extended = match;
				extended.bindVar(generator.varName, cur);
				return generate(generator.tree, extended);
			});
			return res;
		}


	#undef GENERATE

	template<typename T>
	typename T::value_type generate(const TreeGenerator& generator, const Match<T>& match) {
		switch(generator.type) {
			#define CASE(NAME) case TreeGenerator::NAME : return generate ## NAME ## Tree(static_cast<const tree::NAME&>(generator), match)
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
		assert(false && "Unsupported tree-generator construct encountered!");
		return typename T::value_type();
	}

	template<typename T>
	typename T::list_type generate(const ListGenerator& generator, const Match<T>& match) {
		switch(generator.type) {
			#define CASE(NAME) case ListGenerator::NAME : return generate ## NAME ## List(static_cast<const list::NAME&>(generator), match)
				CASE(Empty);
				CASE(Single);
				CASE(Expression);
				CASE(Sequence);
				CASE(ForEach);
			#undef CASE
		}
		assert(false && "Unsupported list-generator construct encountered!");
		return typename T::list_type();
	}


} // end namespace generator


	inline ListGeneratorPtr operator<<(const ListGeneratorPtr& a, const ListGeneratorPtr& b) {
		return std::make_shared<generator::list::Sequence>(a,b);
	}
	inline ListGeneratorPtr operator<<(const TreeGeneratorPtr& a, const ListGeneratorPtr& b) {
		return std::make_shared<generator::list::Sequence>(generator::single(a),b);
	}
	inline ListGeneratorPtr operator<<(const ListGeneratorPtr& a, const TreeGeneratorPtr& b) {
		return std::make_shared<generator::list::Sequence>(a,generator::single(b));
	}
	inline ListGeneratorPtr operator<<(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b) {
		return std::make_shared<generator::list::Sequence>(generator::single(a),generator::single(b));
	}


} // end namespace pattern
} // end namespace transform
} // end namespace insieme

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::TreeGeneratorPtr& generator);
	std::ostream& operator<<(std::ostream& out, const insieme::transform::pattern::ListGeneratorPtr& generator);

} // end namespace std
