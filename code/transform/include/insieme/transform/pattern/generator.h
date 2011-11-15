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

namespace insieme {
namespace transform {
namespace pattern {

	class Generator;

	class TreeGenerator;
	typedef std::shared_ptr<TreeGenerator> TreeGeneratorPtr;

	class ListGenerator;
	typedef std::shared_ptr<ListGenerator> ListGeneratorPtr;

	class MatchExpression;
	typedef std::shared_ptr<MatchExpression> MatchExpressionPtr;

	class Generator : public utils::Printable {
	public:
		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};

	class TreeGenerator : public Generator {
	public:
		virtual TreePtr generate(const Match& match) const =0;
	};

	class ListGenerator : public Generator {
	public:
		virtual TreeList generate(const Match& match) const =0;
	};

	class MatchExpression : public utils::Printable {
	public:
		virtual MatchValue eval(const Match& match) const = 0;
		unsigned getNestingLevel(const Match& match) const { return eval(match).getDepth(); }
	};

namespace generator {

	namespace expression {

		class Tree : public MatchExpression {
			MatchValue value;
		public:
			Tree(const TreePtr& tree) : value(tree) {}
			virtual MatchValue eval(const Match& match) const { return value; }
			virtual std::ostream& printTo(std::ostream& out) const { return out << value; }
		};

		class List : public MatchExpression {
			MatchValue value;
		public:
			List(const TreeList& list) : value(list) {}
			virtual MatchValue eval(const Match& match) const { return value; }
			virtual std::ostream& printTo(std::ostream& out) const { return out << value; }
		};

		class Variable : public MatchExpression {
			string name;
		public:
			Variable(const string& name) : name(name) {}
			virtual MatchValue eval(const Match& match) const {
				assert(match.isVarBound(name) && "Unknown variable encountered!");
				return match.getVarBinding(name);
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << "$" << name; }
		};

		class Element : public MatchExpression {
			MatchExpressionPtr subExpr;
			unsigned index;
		public:
			Element(const MatchExpressionPtr& expression, unsigned index)
				: subExpr(expression), index(index) {}
			virtual MatchValue eval(const Match& match) const {
				MatchValue&& value = subExpr->eval(match);
				assert(value.getDepth() > 0 && "Cannot access element of a tree.");
				return value.getValue(index);
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << *subExpr << "[" << index << "]"; }
		};

		class Transform : public MatchExpression {
			MatchExpressionPtr subExpr;
			ValueOp transformation;
			string name;
		public:
			Transform(const MatchExpressionPtr& expression, const ValueOp& transform, const string& name = "f")
				: subExpr(expression), transformation(transform), name(name) {}
			virtual MatchValue eval(const Match& match) const {
				return transformation(subExpr->eval(match));
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << name << "(" << *subExpr << ")"; }
		};

	}

	namespace tree {

		class Atom : public TreeGenerator {
			TreePtr tree;
		public:
			Atom(const TreePtr& tree) : tree(tree) {}
			virtual TreePtr generate(const Match& match) const {
				return tree;
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << tree; }
		};

		class Root : public TreeGenerator {
		public:
			virtual TreePtr generate(const Match& match) const {
				return match.getRoot();
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << "root"; }
		};

		class Expression : public TreeGenerator {
			MatchExpressionPtr expr;
		public:
			Expression(const MatchExpressionPtr& expr) : expr(expr) {}

			virtual TreePtr generate(const Match& match) const {
				MatchValue&& value = expr->eval(match);
				assert(value.getDepth() == 0);
				return value.getTree();
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << *expr; }
		};

		class Child : public TreeGenerator {
			TreeGeneratorPtr treeGen;
			unsigned childIndex;
		public:
			Child(const TreeGeneratorPtr& tree, unsigned childIndex) : treeGen(tree), childIndex(childIndex) {}

			virtual TreePtr generate(const Match& match) const {
				return treeGen->generate(match)->getSubTrees()[childIndex];
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << *treeGen << "." << childIndex; }
		};

		class Element : public TreeGenerator {
			ListGeneratorPtr listGen;
			unsigned index;
		public:
			Element(const ListGeneratorPtr& list, unsigned index) : listGen(list), index(index) {}
			virtual TreePtr generate(const Match& match) const {
				return listGen->generate(match)[index];
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << *listGen << "[" << index << "]"; }
		};

		class Node : public TreeGenerator {
			int id;
			ListGeneratorPtr childGen;
		public:
			Node(int id, const ListGeneratorPtr& childGen) : id(id), childGen(childGen) {}

			virtual TreePtr generate(const Match& match) const {
				return makeTree(id, childGen->generate(match));
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << "(id:" << id << "|" << *childGen << ")"; }
		};

		class Substitute : public TreeGenerator {
			TreeGeneratorPtr tree;
			TreeGeneratorPtr replacement;
			TreeGeneratorPtr var;
		public:
			Substitute(const TreeGeneratorPtr& tree, const TreeGeneratorPtr& replacement, const TreeGeneratorPtr& var)
				: tree(tree), replacement(replacement), var(var) {}

			virtual TreePtr generate(const Match& match) const;

			virtual std::ostream& printTo(std::ostream& out) const { return out << *tree << "{" << *replacement << "/" << *var << "}"; }
		};
	}


	namespace list {

		class Single : public ListGenerator {
			TreeGeneratorPtr treeGen;
		public:
			Single(const TreeGeneratorPtr& tree) : treeGen(tree) {}
			virtual TreeList generate(const Match& match) const {
				return toVector(treeGen->generate(match));
			}

			virtual std::ostream& printTo(std::ostream& out) const { return out << *treeGen; }
		};

		class Expression : public ListGenerator {
			MatchExpressionPtr expr;
		public:
			Expression(const MatchExpressionPtr& expr) : expr(expr) {}

			virtual TreeList generate(const Match& match) const {
				MatchValue&& value = expr->eval(match);
				assert(value.getDepth() == 1);
				return value.getTreeList();
			}

			virtual std::ostream& printTo(std::ostream& out) const { return out << *expr; }
		};

		class Sequence : public ListGenerator {
			ListGeneratorPtr listA;
			ListGeneratorPtr listB;
		public:
			Sequence(const ListGeneratorPtr& listA, const ListGeneratorPtr& listB) : listA(listA), listB(listB) {}

			virtual TreeList generate(const Match& match) const {
				TreeList resA = listA->generate(match);
				TreeList resB = listB->generate(match);
				resA.insert(resA.end(), resB.begin(), resB.end());
				return resA;
			}
			virtual std::ostream& printTo(std::ostream& out) const { return out << *listA << "," << *listB; }
		};

		class ForEach : public ListGenerator {
			string varName;
			MatchExpressionPtr list;
			TreeGeneratorPtr tree;
		public:
			ForEach(const string& name, const MatchExpressionPtr& list, const TreeGeneratorPtr& tree)
				: varName(name), list(list), tree(tree) {}

			virtual TreeList generate(const Match& match) const {

				// obtain list of values
				MatchValue all = list->eval(match);
				assert(all.getDepth() > 0 && "Cannot apply for-each on scalar!");

				// create one entry per element within the list
				TreeList res;
				Match extended = match;
				::transform(all.getValues(), std::back_inserter(res), [&](const MatchValue& cur){
					extended.bindVar(varName, cur);
					return tree->generate(extended);
				});
				return res;
			}

			virtual std::ostream& printTo(std::ostream& out) const { return out << "for " << varName << " in " << *list << "." << *tree; }
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

	inline ListGeneratorPtr single(const MatchExpressionPtr& expr) {
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

		MatchValue reverse(const MatchValue& value) {
			assert(value.getDepth() == 1 && "Data is not a list!");
			TreeList list = value.getTreeList();
			std::reverse(list.begin(), list.end());
			return MatchValue(list);
		}

	}

	inline MatchExpressionPtr reverse(const MatchExpressionPtr& subexpr) {
		return std::make_shared<expression::Transform>(subexpr, &impl::reverse , "reverse");
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
