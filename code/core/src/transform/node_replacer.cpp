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

#include "transform/node_replacer.h"
#include "ast_builder.h"

namespace {

using namespace insieme::core;
using namespace insieme::core::transform;

#define VISIT_NODE(NodeTy) \
	NodeWrapper visit##NodeTy (const NodeTy##Ptr& currNode) { \
		if(*(currNode) == *(toReplace)) return NodeWrapper(replacement); \
		return NodeCloner::visit##NodeTy (currNode); \
	}

/**
 * Visitor which replace a specific node of the IR starting from a root node.
 */
class NodeReplacer : public NodeCloner {
	const NodePtr& toReplace;
	const NodePtr& replacement;

public:

	NodeReplacer(const ASTBuilder& builder, const NodePtr& toReplace, const NodePtr& replacement):
		NodeCloner(builder), toReplace(toReplace), replacement(replacement) { }

private:

	VISIT_NODE(BreakStmt)
	VISIT_NODE(ContinueStmt)
	VISIT_NODE(Literal)

	VISIT_NODE(ReturnStmt)
	VISIT_NODE(DeclarationStmt)
	VISIT_NODE(CompoundStmt)
	VISIT_NODE(WhileStmt)
	VISIT_NODE(ForStmt)
	VISIT_NODE(IfStmt)
	VISIT_NODE(SwitchStmt)
	VISIT_NODE(VarExpr)

	VISIT_NODE(ParamExpr)
	VISIT_NODE(LambdaExpr)
	VISIT_NODE(CallExpr)
	VISIT_NODE(CastExpr)
};

}

namespace insieme {
namespace core {
namespace transform {

NodePtr replaceNode(const SharedNodeManager& mgr, const NodePtr& root, const NodePtr& toReplace, const NodePtr& replacement) {
	if(!root) return NodePtr(NULL);

	::NodeReplacer replacer(ASTBuilder(mgr), toReplace, replacement);
	return replacer.visit(root).ref;
}

NodePtr replaceNode(const ASTBuilder& builder, const NodePtr& root, const NodePtr& toReplace, const NodePtr& replacement) {
	if(!root) return NodePtr(NULL);

	::NodeReplacer replacer(builder, toReplace, replacement);
	return replacer.visit(root).ref;
}

} // End transform namespace
} // End core namespace
} // End insieme namespace
