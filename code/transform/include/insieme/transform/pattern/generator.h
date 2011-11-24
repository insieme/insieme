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

namespace insieme {
namespace transform {
namespace pattern {

	class Generator;

	class TreeGenerator;
	typedef std::shared_ptr<TreeGenerator> TreeGeneratorPtr;

	class ListGenerator;
	typedef std::shared_ptr<ListGenerator> ListGeneratorPtr;

	class Generator : public utils::Printable {
	public:
		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};

	struct TreeGenerator : public Generator {
		enum Type {
			Atom, Node, Root, Child, Element, Expression, Substitute
		};

		const Type type;

		TreeGenerator(Type type) : type(type) {}
	};

	struct ListGenerator : public Generator {
		enum Type {
			Single, Expression, Sequence, ForEach
		};

		const Type type;

		ListGenerator(Type type) : type(type) {}
	};

	template<typename T>
	class MatchExpression : public utils::Printable {
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
			Node(unsigned id, const ListGeneratorPtr& childGen)
				: TreeGenerator(TreeGenerator::Node), id(id), type(-1), childGen(childGen) {}

			virtual std::ostream& printTo(std::ostream& out) const {
				if (id != -1) {
					return out << "(" << id << "|" << *childGen << ")";
				}
				return out << "(" << ((core::NodeType)type) << "|" << *childGen << ")";
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
	}


	namespace list {

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
				return out << "for " << varName << " in ";
				if (node_list) {
					out << *node_list;
				} else {
					out << *tree_list;
				}
				return out << "." << *tree;
			}
		};
	}


	// -- Utilities ----------------------------------------------------

	extern const TreeGeneratorPtr root;

	inline TreeGeneratorPtr atom(const TreePtr& tree) {
		return std::make_shared<tree::Atom>(tree);
	}

	inline ListGeneratorPtr single(const TreeGeneratorPtr& tree) {
		return std::make_shared<list::Single>(tree);
	}

	inline ListGeneratorPtr single(const TreeMatchExpressionPtr& expr) {
		return std::make_shared<list::Single>(std::make_shared<tree::Expression>(expr));
	}

	inline TreeGeneratorPtr node(const ListGeneratorPtr& generator) {
		return std::make_shared<tree::Node>(0, generator);
	}

	inline TreeGeneratorPtr node(const int id, const ListGeneratorPtr& generator) {
		return std::make_shared<tree::Node>(id, generator);
	}

	inline MatchExpressionPtr var(const std::string& name) {
		return std::make_shared<expression::Variable>(name);
	}

	inline TreeGeneratorPtr treeVar(const std::string& name) {
		return std::make_shared<tree::Expression>(var(name));
	}

	inline ListGeneratorPtr listVar(const std::string& name) {
		return std::make_shared<list::Expression>(var(name));
	}

	inline TreeGeneratorPtr treeExpr(const MatchExpressionPtr& expr) {
		return std::make_shared<tree::Expression>(expr);
	}

	inline ListGeneratorPtr listExpr(const MatchExpressionPtr& expr) {
		return std::make_shared<list::Expression>(expr);
	}


	inline ListGeneratorPtr forEach(const std::string& name, const MatchExpressionPtr& list, const TreeGeneratorPtr& tree) {
		return std::make_shared<list::ForEach>(name, list, tree);
	}

	inline ListGeneratorPtr forEach(const std::string& name, const MatchExpressionPtr& list, const MatchExpressionPtr& tree) {
		return forEach(name, list, treeExpr(tree));
	}



	// -- some match value operations ----------------------------

	namespace impl {

		template<typename T>
		MatchValue<T> reverse(const MatchValue<T>& value) {
			assert(value.getDepth() == 1 && "Data is not a list!");
			auto list = value.getTreeList();
			std::reverse(list.begin(), list.end());
			return MatchValue<T>(list);
		}

	}

	template<typename T>
	inline std::shared_ptr<MatchExpression<T>> reverse(const std::shared_ptr<MatchExpression<T>>& subexpr) {
		return std::make_shared<expression::Transform<T>>(subexpr, &(impl::reverse<T>) , "reverse");
	}





	// -------------------------------------------------------------------------------------
	//   Generator Implementation
	// -------------------------------------------------------------------------------------

	template<typename T>
	typename T::value_type generate(const TreeGeneratorPtr& generator, const Match<T>& value);

	template<typename T>
	typename T::list_type generate(const ListGeneratorPtr& generator, const Match<T>& value);

	MatchValue<ptr_target> eval(
				const std::shared_ptr<MatchExpression<ptr_target>>& node_expr,
				const std::shared_ptr<MatchExpression<tree_target>>& tree_expr,
				const Match<ptr_target>& match
			) {
		return node_expr->eval(match);
	}

	MatchValue<tree_target> eval(
				const std::shared_ptr<MatchExpression<ptr_target>>& node_expr,
				const std::shared_ptr<MatchExpression<tree_target>>& tree_expr,
				const Match<tree_target>& match
			) {
		return tree_expr->eval(match);
	}



	#define GENERATE(NAME) \
		template<typename T> \
		typename T::value_type generate ## NAME ## Tree(const tree::NAME& generator, const Match<T>& match)

		GENERATE(Atom) {
			return generator.tree;
		}

		GENERATE(Root) {
			return match.getRoot();
		}

		GENERATE(Expression) {
			MatchValue<T>&& value = eval(generator.node_expr, generator.tree_expr, match);
			assert(value.getDepth() == 0);
			return value.getTree();
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

		TreePtr generateSubstituteTree(const tree::Substitute& sub, const Match<tree_target>& match) {

			// eval sub-terms
			TreePtr a = generate(sub.tree, match);
			TreePtr b = generate(sub.replacement, match);
			TreePtr c = generate(sub.var, match);

			// apply substitution
			return substitute(a, b, c);
		}

	#undef GENERATE


	#define GENERATE(NAME) \
		template<typename T> \
		typename T::list_type generate ## NAME ## List(const list::NAME& generator, const Match<T>& match)

		GENERATE(Single) {
			return toVector(generate(generator.treeGen, match));
		}

		GENERATE(Expression) {
			MatchValue<ptr_target>&& value = eval(generator.node_expr, generator.tree_expr, match);
			assert(value.getDepth() == 1);
			return value.getTreeList();
		}

		GENERATE(Sequence) {
			TreeList resA = generate(generator.listA, match);
			TreeList resB = generate(generator.listA, match);
			resA.insert(resA.end(), resB.begin(), resB.end());
			return resA;
		}

		GENERATE(ForEach) {

			// obtain list of values
			MatchValue<T> all = eval(generator.node_list, generator.tree_list, match);
			assert(all.getDepth() > 0 && "Cannot apply for-each on scalar!");

			// create one entry per element within the list
			TreeList res;
			Match<T> extended = match;
			::transform(all.getValues(), std::back_inserter(res), [&](const MatchValue<T>& cur){
				extended.bindVar(generator.varName, cur);
				return generate(generator.tree, extended);
			});
			return res;
		}


	#undef GENERATE

	template<typename T>
	typename T::value_type generate(const TreeGeneratorPtr& generator, const Match<T>& value) {
		switch(generator->type) {

		}
		assert(false && "Unsupported tree-generator construct encountered!");
		return typename T::value_type();
	}

	template<typename T>
	typename T::list_type generate(const ListGeneratorPtr& generator, const Match<T>& value) {
		switch(generator->type) {

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
