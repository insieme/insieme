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

#include "insieme/transform/ir_cleanup.h"

#include "insieme/core/ast_visitor.h"
#include "insieme/utils/set_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {

using namespace insieme::core;

namespace {

// TODO: split elimination of refs and arrays ... one after another

bool isArrayType(const TypePtr& cur) {

	// check for null pointer
	if (!cur) {
		return false;
	}

	// accept pure array type
	if (cur->getNodeType() == NT_ArrayType) {
		return true;
	}

	// also accept ref/array combination
	if ((cur->getNodeType() == NT_RefType && static_pointer_cast<const RefType>(cur)->getElementType()->getNodeType() == NT_ArrayType)) {
		return true;
	}
	return false;
}


core::NodePtr removePseudoArraysInStructs(const core::NodePtr& node) {

	// Step 1) search list of structs containing arrays as elements
	utils::set::PointerSet<StructTypePtr> structs;

	// search for the property
	visitDepthFirstOnce(node, makeLambdaVisitor([&](const NodePtr& cur){
		if (cur->getNodeType() != NT_StructType) {
			return;
		}

		StructTypePtr type = static_pointer_cast<const StructType>(cur);
		if (any(type->getEntries(), [](const StructType::Entry& cur)->bool { return isArrayType(cur.second); })) {
			structs.insert(type);
			return;
		}

	}, true));

	// Step 2) collect addresses of nodes accessing array types
	utils::set::PointerSet<MemberAccessExprAddress> accesses;
	{
		visitDepthFirst(NodeAddress(node), [&](const NodeAddress& cur){
			if (cur->getNodeType() != NT_MemberAccessExpr) {
				return;
			}

			MemberAccessExprAddress access = static_address_cast<const MemberAccessExpr>(cur);

			TypePtr type = access->getSubExpression()->getType();
			if (type->getNodeType() != NT_StructType) {
				return;
			}

			StructTypePtr structType = static_pointer_cast<const StructType>(type);
			if (isArrayType(structType->getTypeOfMember(access->getMemberName()))) {
				accesses.insert(access);
			}

		}, false);
	}

	//

	// print list of structs
	for_each(structs, [](const StructTypePtr& cur) {
		std::cout << "Current struct: " << *cur << std::endl;
	});

	for_each(accesses, [](const MemberAccessExprAddress& cur) {
		std::cout << "Access: " << *cur << " @ " << cur << std::endl;
	});

	return node;
}

struct LoopCollector : public core::ASTVisitor<void> {
	typedef std::vector<ForStmtPtr> LoopList;

	LoopCollector() : core::ASTVisitor<void>(false) { }

	void visitForStmt( const ForStmtPtr& forStmt ) {
		visit(forStmt->getBody());

		loops.push_back(forStmt);
	}

	LoopList operator()(const NodePtr& root) {
		visit(root);
		return loops;
	}

	void visitNode(const core::NodePtr& node) {
		std::for_each(node->getChildList().begin(), node->getChildList().end(),
			[ this ] (core::NodePtr curr){
				this->visit(curr);
			});
	}

private:
	LoopList loops;
};

core::NodePtr normalizeLoops(const core::NodePtr& node) {

	LoopCollector lc;
	LoopCollector::LoopList&& loops = lc(node);

	LOG(DEBUG) << loops.size();
	std::for_each(loops.begin(), loops.end(), [](const ForStmtPtr& cur){
		LOG(DEBUG) << *cur;
	} );

	return node;
}


} // end anonymous namespace


core::NodePtr cleanup(const core::NodePtr& node) {

	// start by doing nothing ..
	core::NodePtr res = node;

	// remove unnecessary array indirections
//	res = removePseudoArraysInStructs(res);
	res = normalizeLoops(res);

	// done
	return res;
}

class PseudoArrayEliminationMapping {
public:
	PseudoArrayEliminationMapping() {

	}

	virtual const NodePtr resolveElement(const NodePtr& ptr) {

	}
};

core::NodePtr eliminatePseudoArrays(const core::NodePtr& node) {
	// search for array variable declarations
	visitAllNodes(NodeAddress(node), [&](const NodeAddress& curdecl) {
		if(curdecl->getNodeType() != NT_DeclarationStmt) {
			return;
		}

		DeclarationStmtAddress decl = static_address_cast<const DeclarationStmt>(curdecl);
		auto var = decl->getVariable();
		auto type = var->getType();

		if(type == NT_ArrayType) {
			// array variable, check indexing
			visitAllNodes(decl, [&](const NodeAddress& curcall) {
				if(curcall->getNodeType() != NT_CallExpr) {
					return;
				}

				CallExprAddress call = static_address_cast<const CallExpr>(curcall);

			} );
		}
	}, true);
}

} // end of namespace transform
} // end of namespace insieme
