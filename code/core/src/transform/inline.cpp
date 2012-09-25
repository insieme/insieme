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

#include "insieme/core/transform/inline.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/sequentialize.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace core {
namespace transform {

namespace {

using namespace std;
using namespace insieme::utils::log;
using namespace insieme::core;

/** Inlines a lambda body with return statements
 *  if the lambda does not return unit, a return location needs to be passed
 */
class Redeemer : public NodeMapping {
	NodeManager& nodeMan;
	IRBuilder build;
	const lang::BasicGenerator& basic;

	CompoundStmtPtr outerBody;
	ExpressionPtr retLoc;
	VariablePtr returned;
	
	bool compRetActive;
	bool encounteredReturn;

	NodePtr getReturnedAssignment() {
		return build.assign(returned, build.boolLit(true));
	}

	IfStmtPtr wrapNotReturned(const CompoundStmtPtr& comp) {
		return build.ifStmt(build.logicNeg(build.deref(returned)), comp);
	}
	
	CompoundStmtPtr mapCompoundStmt(const CompoundStmtPtr& compStmt) {
		StatementList retStmts, stmtList = compStmt->getStatements();
		for(size_t i=0; i<stmtList.size(); ++i) {
			StatementPtr replacement = mapElement(0, stmtList[i]).as<StatementPtr>();
			retStmts.push_back(replacement);
			if(compRetActive && i < stmtList.size()-1) {
				StatementList remainder(stmtList.begin()+i+1, stmtList.end());
				bool prevCompRetActive = compRetActive;
				compRetActive = false;
				retStmts.push_back(wrapNotReturned(mapCompoundStmt(build.compoundStmt(remainder))));
				compRetActive = prevCompRetActive;
				break;
			}
		}
		return build.compoundStmt(retStmts);
	}

	NodePtr mapWhileStmt(const WhileStmtPtr& whileStmt) {
		WhileStmtPtr replacement = whileStmt->substitute(nodeMan, *this).as<WhileStmtPtr>();
		if(compRetActive) {
			ExpressionPtr cond = replacement->getCondition();
			cond = build.logicAnd(cond, build.logicNeg(build.deref(returned)));
			replacement = build.whileStmt(cond, replacement->getBody());
		}
		return replacement;
	}

	NodePtr mapReturnStmt(const ReturnStmtPtr& retStmt) {
		compRetActive = true;
		encounteredReturn = true;
		StatementList retStmts;
		if(retLoc) retStmts.push_back(build.assign(retLoc, retStmt->getReturnExpr()));
		retStmts.push_back(getReturnedAssignment().as<StatementPtr>());
		return build.compoundStmt(retStmts);
	}

public:
	Redeemer(NodeManager& nodeMan) : nodeMan(nodeMan), build(nodeMan), basic(nodeMan.getLangBasic()),
		retLoc() { }

	CompoundStmtPtr apply(const CompoundStmtPtr& body, const ExpressionPtr returnLocation = ExpressionPtr()) {
		retLoc = returnLocation;
		outerBody = body;
		StatementList retStmts;
		compRetActive = false;
		encounteredReturn = false;

		// build returned boolean
		returned = build.variable(build.refType(build.getLangBasic().getBool()));
		retStmts.push_back(build.declarationStmt(returned, build.refVar(build.boolLit(false))));

		// adjust body
		retStmts.push_back(map(body));
		return encounteredReturn ? build.compoundStmt(retStmts) : body;
	}


protected:
	virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
		if(ptr->getNodeCategory() == NC_Expression || ptr->getNodeCategory() == NC_Type) return ptr;
		switch(ptr->getNodeType()) {
		case NT_CompoundStmt: return mapCompoundStmt(ptr.as<CompoundStmtPtr>());
		case NT_ReturnStmt: return mapReturnStmt(ptr.as<ReturnStmtPtr>());
		case NT_WhileStmt: return mapWhileStmt(ptr.as<WhileStmtPtr>());
		default: return ptr->substitute(nodeMan, *this);
		}
	}
};


CompoundStmtPtr inlineMultiReturnInternal(StatementList& retStmts, NodeManager& nodeMan, const CallExprPtr& call, const ExpressionPtr& retLocation) {
	IRBuilder build(nodeMan);

	// get called lambda and its parameters
	assert(call->getFunctionExpr()->getNodeType() == NT_LambdaExpr);
	LambdaExprPtr lambdaExpr = call->getFunctionExpr().as<LambdaExprPtr>();

	// build parameter map
	VarExprMap parReplacements;
	VariableList parameters = lambdaExpr->getParameterList()->getParameters();
	ExpressionList arguments = call->getArguments();
	auto parArgRange = make_paired_range(parameters, arguments);
	for_range(make_paired_range(parameters, arguments), [&](const std::pair<VariablePtr, ExpressionPtr>& cur) {
		if(LiteralPtr lit = dynamic_pointer_cast<LiteralPtr>(cur.second)) {
			// this literal can be mapped directly
			parReplacements.insert(make_pair(cur.first, cur.second));
		} else if(VariablePtr argVar = dynamic_pointer_cast<VariablePtr>(cur.second)) {
			// this variable can be mapped directly
			parReplacements.insert(make_pair(cur.first, cur.second));
		} else {
			// a more complex expression - we need to build a variable and map it to that
			VariablePtr newVar = build.variable(cur.first->getType());
			retStmts.push_back(build.declarationStmt(newVar, cur.second));
			parReplacements.insert(make_pair(cur.first, newVar));
		}
	});

	// adjust function body
	CompoundStmtPtr body = lambdaExpr->getBody();
	body = core::transform::replaceVarsGen(nodeMan, body, parReplacements);

	// replace returns with assignments, and adjust control flow
	Redeemer redeemer(nodeMan);
	body = redeemer.apply(body, retLocation);

	retStmts.push_back(body);

	return build.compoundStmt(retStmts);
}

} // anonymous namespace

CompoundStmtPtr inlineMultiReturnAssignment(NodeManager& nodeMan, const CallExprPtr& assignment) {
	StatementList retStmts;

	const lang::BasicGenerator& basic = nodeMan.getLangBasic();
	assert(assignment->getFunctionExpr() == basic.getRefAssign());

	// split into left and right side of assignment
	ExpressionPtr rhsExpr = assignment->getArgument(1);
	assert(rhsExpr->getNodeType() == NT_CallExpr);
	CallExprPtr rhsCall = rhsExpr.as<CallExprPtr>();
	ExpressionPtr retLocation = assignment->getArgument(0);

	return inlineMultiReturnInternal(retStmts, nodeMan, rhsCall, retLocation);
}

CompoundStmtPtr inlineMultiReturnPlainCall(NodeManager& nodeMan, const CallExprPtr& call) {
	StatementList retStmts;
	ExpressionPtr retLoc;
	return inlineMultiReturnInternal(retStmts, nodeMan, call, retLoc);
}


} // namespace transform
} // namespace core
} // namespace insieme
