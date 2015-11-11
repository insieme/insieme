/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <iomanip>
#include <ostream>
#include <string>

#include "insieme/frontend/extensions/while_to_for_extension.h"

#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

namespace pattern = insieme::core::pattern;
namespace irp = insieme::core::pattern::irp;

// TODO (in order of precedence):
// - Support more operations for boundary extraction (LT, LE, variables on the RHS)
// - Also support more ways to write the increment expression
// - Handle non-int loops (e.g. for C++) by constructing the required int range, looping over it and setting the iterator variable internally
// - replace stupid "use after loop" check by more sophisticated def use analysis.

namespace insieme {
namespace frontend {
namespace extensions {

	namespace {
		// tries to map a given operation (assignment or increment/decrement) to a step for an IR for loop
		// returns (read/write, step expression)
		std::pair<bool, core::ExpressionPtr> mapToStep(core::NodePtr operation) {
			auto invalid = std::make_pair(false, core::ExpressionPtr());

			auto& mgr = operation->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& basic = mgr.getLangBasic();
			auto& fMod = mgr.getLangExtension<utils::FrontendInspireModule>();
			auto& rMod = mgr.getLangExtension<core::lang::ReferenceExtension>();

			auto call = operation.as<core::CallExprPtr>();
			if(!call) return invalid;
			auto callee = call->getFunctionExpr();

			if(callee == fMod.getCStyleAssignment()) {
				// we could be smarter here, for now only handle the simplest case
				auto lhs = call->getArgument(0);
				auto rhsCall = call->getArgument(1).isa<core::CallExprPtr>();
				if(!rhsCall) return invalid;
				if(rhsCall->getArgument(0) != builder.deref(lhs)) return invalid;
				auto rhsCallee = rhsCall->getFunctionExpr();
				if(basic.isSignedIntAdd(rhsCallee) || basic.isUnsignedIntAdd(rhsCallee) || basic.isCharAdd(rhsCallee)) {
					return std::make_pair(false, rhsCall->getArgument(1));
				} else if(basic.isSignedIntSub(rhsCallee) || basic.isUnsignedIntSub(rhsCallee) || basic.isCharSub(rhsCallee)) {
					return std::make_pair(false, builder.minus(rhsCall->getArgument(1)));
				}
			} else if(rMod.isGenPreInc(callee) || rMod.isGenPostInc(callee)) {
				return std::make_pair(false, builder.literal("1", call->getType()));
			} else if(rMod.isGenPreDec(callee) || rMod.isGenPostDec(callee)) {
				return std::make_pair(false, builder.literal("-1", call->getType()));
			} else if(callee == rMod.getRefDeref()) {
				// just a read, we can ignore this
				return std::make_pair(true, core::ExpressionPtr());
			}
			return invalid;
		}
		
		// tries to map a given operation (declaration or assignment) to a start expr for an IR for loop
		// returns (valid, start expression)
		std::pair<bool, core::ExpressionPtr> mapToStart(core::VariablePtr var, core::NodePtr operation) {
			auto invalid = std::make_pair(false, core::ExpressionPtr());

			auto& mgr = operation->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& fMod = mgr.getLangExtension<utils::FrontendInspireModule>();
			auto& rMod = mgr.getLangExtension<core::lang::ReferenceExtension>();

			// first check if declaration
			if(auto decl = operation.isa<core::DeclarationStmtPtr>()) {
				auto initCall = decl->getInitialization().isa<core::CallExprPtr>();
				if(!initCall || !core::analysis::isCallOf(initCall, rMod.getRefVarInit())) return invalid;
				return std::make_pair(decl->getVariable() == var, initCall->getArgument(0));
			}

			// otherwise assignment call
			auto call = operation.as<core::CallExprPtr>();
			if(!call) return invalid;
			auto callee = call->getFunctionExpr();
			if(callee == fMod.getCStyleAssignment()) {
				auto lhs = call->getArgument(0);
				if(lhs != var) return invalid;
				return std::make_pair(true, call->getArgument(1));
			}
			return invalid;
		}
		
		// tries to map a given operation (conditional) to an end expr for an IR for loop
		// returns (valid, end expression)
		std::pair<bool, core::ExpressionPtr> mapToEnd(core::VariablePtr var, core::ExpressionPtr operation) {
			auto invalid = std::make_pair(false, core::ExpressionPtr());

			auto& mgr = operation->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& basic = mgr.getLangBasic();
			auto& rMod = mgr.getLangExtension<core::lang::ReferenceExtension>();

			// operation needs to be a call and have 2 arguments
			auto callExpr = operation.isa<core::CallExprPtr>();
			auto callee = callExpr->getFunctionExpr();
			if(!callExpr || callExpr->getArguments().size() != 2) return invalid;

			// first check if variable is on LHS
			// (should also work for rhs -- not yet)
			auto lhs = callExpr->getArgument(0).isa<core::CallExprPtr>();
			if(!rMod.isRefDeref(lhs->getFunctionExpr())) return invalid;
			if(lhs->getArgument(0) != var) return invalid;

			auto rhs = callExpr->getArgument(1);
			// now check type of comparison
			if(basic.isSignedIntNe(callee) || basic.isUnsignedIntNe(callee) || basic.isCharNe(callee) || basic.isSignedIntLt(callee)
			   || basic.isUnsignedIntLt(callee) || basic.isCharLt(callee)) {
				return std::make_pair(true, rhs);
			} else if(basic.isSignedIntGt(callee) || basic.isUnsignedIntGt(callee) || basic.isCharGt(callee)) {
				// don't handle these for now, complex to say for certain what to do in all cases
				return invalid;
			} else if(basic.isSignedIntLe(callee) || basic.isUnsignedIntLe(callee) || basic.isCharLe(callee)) {
				return std::make_pair(true, builder.add(rhs, builder.literal("1", rhs->getType())));
			} else if(basic.isSignedIntGe(callee) || basic.isUnsignedIntGe(callee) || basic.isCharGe(callee)) {
				//return std::make_pair(true, builder.sub(rhs, builder.literal("1", rhs->getType())));
				// don't handle these for now, complex to say for certain what to do in all cases
				return invalid;
			}

			return invalid;
		}
	}

	bool isUsedAfterLoop(const core::StatementList& stmts,
			const core::WhileStmtPtr& whileStmt, const core::NodePtr& predecessor) {
		if(((stmts.size()>=1) && (stmts[stmts.size()-1] != whileStmt))
			|| !predecessor.isa<core::DeclarationStmtPtr>()) {
			return true;
		}
		return false;
	}

	/// while statements can be for statements iff only one variable used in the condition is
	/// altered within the statement, and this alteration satisfies certain conditions
	core::ProgramPtr WhileToForExtension::IRVisit(insieme::core::ProgramPtr& prog) {

		auto& mgr = prog->getNodeManager();		
		core::IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		auto whilePat = irp::compoundStmt(pattern::anyList << pattern::var("predecessor", (irp::declarationStmt() | irp::callExpr(pattern::any)))
			                                               << pattern::var("while", irp::whileStmt()) 
														   << pattern::anyList);

		// match (and potentially replace) all generated while statements
		prog = irp::replaceAll(whilePat, core::NodeAddress(prog), [&](pattern::AddressMatch match) -> core::StatementPtr {
			auto original = match.getRoot().getAddressedNode().as<core::CompoundStmtPtr>();
			auto whileAddr = match["while"].getFlattened().front().as<core::WhileStmtAddress>();
			auto condition = whileAddr.getAddressedNode()->getCondition();
			auto body = whileAddr.getAddressedNode()->getBody();
			auto pred = match["predecessor"].getFlattened().front();

			// check if loop variable is used after the loop
			// at the moment this check is very simple. 
			// TODO: replace stupid "use after loop" check by more sophisticated def use analysis.
			if(isUsedAfterLoop(original->getStatements(), whileAddr.getAddressedNode(), 
				pred.getAddressedNode())) {
				return original;
			}

			VLOG(1) << "======= START WHILE TO FOR ===========\n" << dumpColor(match.getRoot().getAddressedNode());
			
			// check if body contains flow alterating stmts (break or return. continue is allowed.)
			bool flowAlteration = core::analysis::hasFreeBreakStatement(body) || core::analysis::hasFreeReturnStatement(body);
			VLOG(1) << "--- flow alteration: " << flowAlteration << "\n";
			if(flowAlteration) return original;

			// if more than one free variable in the condition -> we certainly can't create a for for this
			auto cvars = core::analysis::getFreeVariables(condition);
			VLOG(1) << "--- cvars:\n " << cvars << "\n";
			if(cvars.size() > 1 || cvars.size() == 0) return original;

			auto cvar = cvars.front();
			core::TypePtr cvarType = core::analysis::getReferencedType(cvar->getType());
			VLOG(1) << "cvar: " << dumpColor(cvar) << "\n -- type: " << cvarType << "\n";
			
			// if it's not an integer, we bail out for now
			if(!basic.isInt(cvarType)) return original;

			/////////////////////////////////////////////////////////////////////////////////////// figuring out the step

			// find all uses of the variable in the while body
			core::ExpressionList writeStepExprs;
			core::NodeList toRemoveFromBody;
			core::visitDepthFirstPrunable(core::NodeAddress(body), [&](const core::NodeAddress& varA) {
				auto var = varA.getAddressedNode().isa<core::VariablePtr>();
				if(var == cvar) {
					// check if it's a write
					auto convertedPair = mapToStep(varA.getParentNode());
					if(!convertedPair.first) {
						writeStepExprs.push_back(convertedPair.second);
						toRemoveFromBody.push_back(varA.getParentNode());
					}
				}
				if(varA.isa<core::LambdaExprAddress>()) return true;
				return false;
			});
			VLOG(1) << "WriteStepExprs: " << writeStepExprs << "\n";
			
			// if there is not exactly one write, or there are free variables in the step, bail out
			// (could be much smarter about the step)
			if(writeStepExprs.size()!=1) return original;
			if(!core::analysis::getFreeVariables(writeStepExprs.back()).empty()) return original;
			auto convertedStepExpr = writeStepExprs.front();
			
			/////////////////////////////////////////////////////////////////////////////////////// figuring out start
			
			auto convertedStartPair = mapToStart(cvar, pred);
			VLOG(1) << "StartPair: " << convertedStartPair.first << " // " << convertedStartPair.second << "\n";

			// bail if no valid start found
			if(!convertedStartPair.first) return original;
			
			/////////////////////////////////////////////////////////////////////////////////////// figuring out end

			auto convertedEndPair = mapToEnd(cvar, condition);
			VLOG(1) << "EndPair: " << convertedEndPair.first << " // " << convertedEndPair.second << "\n";
			
			// bail if no valid end found
			if(!convertedEndPair.first) return original;
			
			/////////////////////////////////////////////////////////////////////////////////////// build the for

			auto forVar = builder.variable(cvarType);

			// prepare new body
			core::StatementList newBodyStmts;
			core::NodeMap loopVarReplacement;
			loopVarReplacement[builder.deref(cvar)] = forVar;
			for(auto stmt : body->getStatements()) {
				if(!::contains(toRemoveFromBody, stmt)) newBodyStmts.push_back(core::transform::replaceAllGen(mgr, stmt, loopVarReplacement));
			}
			//synchronize cvar and new forVar
			//newBodyStmts.push_back(builder.callExpr(cvar->getType(), fMod.getCStyleAssignment(), forVar, cvar));
			auto newBody = builder.compoundStmt(newBodyStmts);

			// if the old loop variable is free in the new body, we messed up and should bail
			if(::contains(core::analysis::getFreeVariables(newBody), cvar)) return original;

			auto forStmt = builder.forStmt(forVar, convertedStartPair.second, convertedEndPair.second, convertedStepExpr, newBody);
			VLOG(1) << "======> FOR:\n" << dumpColor(forStmt);

			/////////////////////////////////////////////////////////////////////////////////////// replace the while in the original compound
			
			core::StatementList replacementCompoundStmts;
			bool afterWhile = false;
			for(auto stmt : original->getStatements()) {
				if(stmt == whileAddr.getAddressedNode()) {
					afterWhile = true;
					replacementCompoundStmts.push_back(forStmt);
				} else {
					replacementCompoundStmts.push_back(stmt);
					// if cvar is used after while, bail (for now)
					if(afterWhile && ::contains(core::analysis::getFreeVariables(stmt), cvar)) return original;
				}
			}

			return builder.compoundStmt(replacementCompoundStmts);
		}).as<core::ProgramPtr>();

		return prog;
	}

} // extensions
} // frontend
} // insieme