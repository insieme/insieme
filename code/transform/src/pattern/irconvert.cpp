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

#include "insieme/transform/pattern/irconvert.h"

#include "insieme/core/ast_builder.h"
#include "insieme/utils/container_utils.h"

using std::make_shared;

namespace insieme {
namespace transform {
namespace pattern {

namespace {

	using namespace core;

	/**
	 * Represents a complex IR node (with children).
	 * Children are converted lazily on demand.
	 */
	class IRTree : public Tree {
	public:
		typedef std::function<TreeList(const NodePtr&)> EvalFunctor;

	private:
		const NodePtr node;
		mutable bool evaluated;
		const EvalFunctor evalFunctor;

	public:

		IRTree(const core::NodePtr& node, const EvalFunctor& evalFunctor = &convertChildren)
			: Tree(node->getNodeType()), node(node), evaluated(false), evalFunctor(evalFunctor) {}

		const core::NodePtr& getNode() const {
			return node;
		}

		std::ostream& printTo(std::ostream& out) const {
			if(!evaluated) {
				return out << "irtree[lazy](" << getId() << "," << *node << ")";
			}
			return out << "irtree[evaled](" << getId() << "," << join(",", subTrees, print<deref<TreePtr>>()) << ")";
		}

		virtual const TreeList& getSubTrees() const {
			if(!evaluated) {
				evaluated = true;
				const_cast<IRTree*>(this)->subTrees = evalFunctor(node);
			}
			return subTrees;
		}

		static TreeList convertChildren(const NodePtr& node) {
			auto children = node->getChildList();
			TreeList res;
			::transform(children, std::back_inserter(res), &toTree);
			return res;
		}
	};

	typedef std::shared_ptr<IRTree> IRTreePtr;

	/**
	 * Visitor that converts any IR address to the tree structure used by the pattern matcher.
	 */
	class TreeConverter : public core::ASTVisitor<TreePtr> {

	public:

		TreeConverter() : core::ASTVisitor<TreePtr>(true) { }

		// NODES REQUIERING SPECIAL TREATMENT

		TreePtr visitGenericType(const GenericTypePtr& node){
			static IRTree::EvalFunctor eval = [](const NodePtr& node) {
				TreeList children;
				children.push_back(makeValue(static_pointer_cast<const GenericType>(node)->getFamilyName()));
				copy(IRTree::convertChildren(node), back_inserter(children));
				return children;
			};
			return std::make_shared<IRTree>(node, eval);
		}

		TreePtr visitLiteral(const LiteralPtr& node){
			static IRTree::EvalFunctor eval = [](const NodePtr& node) {
				TreeList res;
				res.push_back(makeValue(static_pointer_cast<const Literal>(node)->getValue()));
				copy(IRTree::convertChildren(node), back_inserter(res));
				return res;
			};
			return make_shared<IRTree>(node, eval);
		}
		TreePtr visitCallExpr(const CallExprPtr& node){
			static IRTree::EvalFunctor eval = [](const NodePtr& node) {
				auto children = node->getChildList();
				TreeList res;
				std::transform(children.begin()+1, children.end(), std::back_inserter(res), &toTree);
				return res;
			};
			return make_shared<IRTree>(node, eval);
		}

		TreePtr visitVariableIntTypeParam(const VariableIntTypeParamPtr& node){
			return makeTree((int)node->getNodeType(), makeValue(node->getSymbol()));
		}

		TreePtr visitConcreteIntTypeParam(const ConcreteIntTypeParamPtr& node){
			return makeTree((int)node->getNodeType(), makeValue(node->getValue()));
		}

		// ALL REMAINING NODES

		TreePtr visitNode(const NodePtr& node) {
			// apply standard treatment
			return make_shared<IRTree>(node);
		}
	};

}

TreePtr toTree(const core::NodePtr& node) {
	return TreeConverter().visit(node);
}



namespace {


}

core::NodePtr toIR(core::NodeManager& manager, const TreePtr& tree) {

	// test node id
	auto id = tree->getId();
	assert( 0 <= id && id < core::NUM_CONCRETE_NODE_TYPES && "Invalid node type encountered!");

	core::NodeType type = (core::NodeType)id;

	core::ASTBuilder builder(manager);


//	switch(type) {
//	#define CONCRETE(KIND) \
//		case NT_ ## KIND : return convert ## KIND (tree);
//	#include "insieme/core/ast_nodes.def"
//	#undef CONCRETE
//	}

	assert(false && "Some node type is missing ...");
	return NodePtr();
}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
