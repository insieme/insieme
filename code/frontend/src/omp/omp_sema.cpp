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
#include "insieme/core/transform/manipulation.h"
#include "insieme/utils/set_utils.h"
#include "insieme/core/lang/basic.h"

namespace insieme {
namespace frontend {
namespace omp {

using namespace core;
namespace cl = lang;
namespace us = utils::set;
namespace um = utils::map;

const core::ProgramPtr applySema(const core::ProgramPtr& prog, core::NodeManager& resultStorage) {
	ProgramPtr result = prog;
	for(;;) {
		SemaVisitor v(resultStorage);
		core::visitAllInterruptable(core::ProgramAddress(result), v);
		if(v.getReplacement()) result = v.getReplacement();
		else break;	
	}
	return result;
}

bool SemaVisitor::visitNode(const NodeAddress& node) {
	return true; // default behaviour: continue visiting
}

bool SemaVisitor::visitMarkerStmt(const MarkerStmtAddress& mark) {
	const StatementAddress stmt = static_address_cast<const Statement>(mark.getAddressOfChild(0));
	//LOG(INFO) << "marker on: \n" << *stmt;
	if(BaseAnnotationPtr anno = mark->getAnnotation(BaseAnnotation::KEY)) {
		LOG(INFO) << "omp annotation(s) on: \n" << *stmt;
		std::for_each(anno->getAnnotationListBegin(), anno->getAnnotationListEnd(), [&](AnnotationPtr subAnn){
			LOG(INFO) << "annotation: " << *subAnn;
			NodePtr newNode;
			if(auto parAnn = std::dynamic_pointer_cast<Parallel>(subAnn)) {
				newNode = handleParallel(stmt, parAnn);
			} else if(auto forAnn = std::dynamic_pointer_cast<For>(subAnn)) {
				newNode = handleFor(stmt, forAnn);
			}
			else assert(0 && "Unhandled OMP annotation.");
			//LOG(INFO) << "Pre replace: " << *mark.getRootNode();
			//LOG(INFO) << "Replace: " << *mark;
			//LOG(INFO) << "   with: " << *newNode;
			replacement = dynamic_pointer_cast<const Program>(transform::replaceNode(nodeMan, mark, newNode, true));
			//LOG(INFO) << "Post replace: " << replacement;
		});
		return false;
	}
	return true;
}

NodePtr SemaVisitor::handleParallel(const StatementAddress& stmt, const ParallelPtr& par) {
	auto stmtNode = stmt.getAddressedNode();

	auto parLambda = transform::extractLambda(nodeMan, stmtNode, true);

	auto& basic = nodeMan.basic;
	auto jobExp = build.jobExpr(parLambda, JobExpr::GuardedStmts(), JobExpr::LocalDecls());
	auto parallelCall = build.callExpr(basic.getParallel(), build.literal("8", basic.getUInt4()), build.literal("8", basic.getUInt4()), jobExp);
	auto mergeCall = build.callExpr(basic.getMerge(), parallelCall);
	//LOG(INFO) << "mergeCall:\n" << mergeCall;
	return mergeCall;
}

NodePtr SemaVisitor::handleFor(const core::StatementAddress& stmt, const ForPtr& forP) {
	auto stmtNode = stmt.getAddressedNode();
	ForStmtPtr forStmt = dynamic_pointer_cast<const ForStmt>(stmtNode);
	assert(forStmt && "OpenMP for attached to non-for statement");

	auto& basic = nodeMan.basic;
	auto pfor = build.pfor(forStmt);

	//LOG(INFO) << "for stmtNode:\n" << stmtNode;
	return pfor;
}

} // namespace omp
} // namespace frontend
} // namespace insieme
