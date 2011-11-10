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

#include "insieme/core/ir_builder.h"
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
	class TreeConverter : public core::IRVisitor<TreePtr> {

	public:

		TreeConverter() : core::IRVisitor<TreePtr>(true) { }

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
				res.push_back(makeValue(static_pointer_cast<const Literal>(node)->getStringValue()));
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
			return makeTree((int)node->getNodeType(), makeValue(node->getSymbol()->getValue()));
		}

		TreePtr visitConcreteIntTypeParam(const ConcreteIntTypeParamPtr& node){
			return makeTree((int)node->getNodeType(), makeValue((int)node->getValue()));
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

	NodePtr convertTypeVariable(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertFunctionType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertTupleType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertRecType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertGenericType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertArrayType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertVectorType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertRefType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertChannelType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertStructType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertUnionType(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertBreakStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertContinueStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertReturnStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertDeclarationStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertCompoundStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertWhileStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertForStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertIfStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertSwitchStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertVariable(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertLambdaExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertBindExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertLiteral(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertCallExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertCastExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertJobExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertTupleExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertVectorExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertStructExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertUnionExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertMemberAccessExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertTupleProjectionExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertProgram(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertIdentifier(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertVariableIntTypeParam(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertConcreteIntTypeParam(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertInfiniteIntTypeParam(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertLambda(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertLambdaDefinition(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertRecTypeDefinition(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertMarkerExpr(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

	NodePtr convertMarkerStmt(core::NodeManager& manager, const TreePtr& tree) {
		return NodePtr();
	}

}

core::NodePtr toIR(core::NodeManager& manager, const TreePtr& tree) {

	// test for an IR tree;
	if (IRTreePtr irTree = dynamic_pointer_cast<IRTree>(tree)) {
		return irTree->getNode();
	}

	/*
	// test node id
	auto id = tree->getId();
	assert( 0 <= id && id < core::NUM_CONCRETE_NODE_TYPES && "Invalid node type encountered!");

	core::NodeType type = (core::NodeType)id;

	switch(type) {
	#define CONCRETE(KIND) \
		case NT_ ## KIND : return convert ## KIND (manager, tree);
	// #include "insieme/core/ir_nodes.def"
	#undef CONCRETE
	}
	*/

	assert(false && "Some node type is missing ...");
	return NodePtr();
}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme
