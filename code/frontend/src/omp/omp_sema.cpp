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

#include "insieme/frontend/omp/omp_sema.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace frontend {
namespace omp {

using namespace core;
namespace cl = lang;
namespace us = utils::set;
namespace um = utils::map;

/** Will certainly determine the declaration status of variables inside a block.
 */
struct LambdaDeltaVisitor : public ASTVisitor<bool, Address> {
	us::PointerSet<VariablePtr> declared;
	us::PointerSet<VariablePtr> undeclared;

	bool visitNode(const NodeAddress& node) { return true; } // default behaviour: continue visiting

	bool visitDeclarationStmt(const DeclarationStmtAddress &decl) {
		declared.insert(decl->getVariable());
		return true;
	}

	bool visitVariable(const VariableAddress& var) {
		auto vp = var.getAddressedNode();
		if(declared.find(vp) == declared.end()) undeclared.insert(vp);
		return true;
	}
};


bool SemaVisitor::visitNode(const core::NodeAddress& node) {
	return true; // default behaviour: continue visiting
}

bool SemaVisitor::visitStatement(const StatementAddress& stmt) {
	if(BaseAnnotationPtr anno = stmt.getAddressedNode().getAnnotation(BaseAnnotation::KEY)) {
		LOG(INFO) << "omp annotation(s) on: \n" << *stmt;
		std::for_each(anno->getAnnotationListBegin(), anno->getAnnotationListEnd(), [&](AnnotationPtr subAnn){
			LOG(INFO) << "annotation: " << *subAnn;
			if(auto par = std::dynamic_pointer_cast<Parallel>(subAnn)) {
				handleParallel(stmt, *par);
			}
		});
		return false;
	}
	return true;
}

void SemaVisitor::handleParallel(const StatementAddress& stmt, const Parallel& par) {
	auto stmtNode = stmt.getAddressedNode();

	LambdaDeltaVisitor ldv;
	core::visitAllInterruptable(StatementAddress(stmtNode), ldv);
	
	Lambda::CaptureList captures;
	um::PointerMap<NodePtr, NodePtr> replacements;
	for_each(ldv.undeclared, [&](VariablePtr p){
		//auto declStmt = build.declarationStmt(p->getType(), p);
		auto var = build.variable(p->getType());
		captures.push_back(var);
		replacements[p] = var;
	});
	StatementPtr newStmt = dynamic_pointer_cast<const Statement>(transform::replaceAll(nodeMan, stmtNode, replacements));

	auto parLambda = build.lambdaExpr(newStmt, captures, Lambda::ParamList());
	auto jobExp = build.jobExpr(parLambda, JobExpr::GuardedStmts(), JobExpr::LocalDecls());
	auto parallelCall = build.callExpr(cl::OP_PARALLEL, build.uintVal(8), build.uintVal(8), jobExp);
	auto mergeCall = build.callExpr(cl::OP_MERGE, parallelCall);
	//LOG(INFO) << "mergeCall:\n" << mergeCall;
	ProgramPtr retProgPtr = dynamic_pointer_cast<const Program>(transform::replaceNode(nodeMan, stmt, mergeCall, true));
	replacement = retProgPtr;
}

} // namespace omp
} // namespace frontend
} // namespace insieme