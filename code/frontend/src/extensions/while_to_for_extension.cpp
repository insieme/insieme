/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include "insieme/core/lang/compound_operators.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

/// DELETEME
#include "insieme/frontend/omp/omp_annotation.h"

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
			auto& rMod = mgr.getLangExtension<core::lang::ReferenceExtension>();
			auto& compOpExt = mgr.getLangExtension<core::lang::CompoundOpsExtension>();

			auto call = operation.isa<core::CallExprPtr>();
			if(!call) return invalid;
			auto callee = call->getFunctionExpr();

			if(callee == compOpExt.getCompAssignAdd() || callee == compOpExt.getCompAssignSubtract()) {
				return std::make_pair(false, core::analysis::getArgument(operation, 1));
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

			// first check if declaration
			if(auto decl = operation.isa<core::DeclarationStmtPtr>()) {
				return std::make_pair(decl->getVariable() == var, decl->getInitialization());
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
			if(!callExpr || callExpr->getNumArguments() != 2) return invalid;

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

	bool isUsedAfterLoop(const core::StatementList& stmts, const core::WhileStmtPtr& whileStmt, const core::NodePtr& predecessor,
		                 const core::VariablePtr& cvar) {
		if(!predecessor.isa<core::DeclarationStmtPtr>()) return true;
		bool afterWhile = false;
		for(auto stmt : stmts) {
			if(stmt == whileStmt) {
				afterWhile = true;
			} else {
				if(afterWhile && ::contains(core::analysis::getFreeVariables(stmt), cvar)) return true;
			}
		}
		return false;
	}

	core::StatementPtr getPostCondition(const core::VariablePtr& forVar, const core::ExpressionPtr& start, const core::ExpressionPtr& stop, const core::ExpressionPtr& step) {
		core::IRBuilder builder(forVar->getNodeManager());
		// create following statement: tmpEndValue = end + ((beg-end) % step + step) % step
		return builder.assign(forVar, builder.add(stop, builder.mod(builder.add(builder.mod(builder.sub(start, stop), step), step),step)));
	}

	/// while statements can be for statements iff only one variable used in the condition is
	/// altered within the statement, and this alteration satisfies certain conditions
	core::tu::IRTranslationUnit WhileToForExtension::IRVisit(core::tu::IRTranslationUnit& tu) {

		for(auto fun : tu.getFunctions()) {

			auto lambdaExpr = fun.second;

			auto& mgr = lambdaExpr->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& basic = mgr.getLangBasic();
			auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();

			auto innerWhilePat = pattern::var("while", irp::whileStmt());
			auto markerNestedWhile = pattern::rT(innerWhilePat | irp::markerStmt(pattern::recurse, pattern::any));
			auto whilePat = irp::compoundStmt(pattern::anyList << pattern::var("predecessor", (irp::declarationStmt() | irp::callExpr(pattern::any)))
				                                               << markerNestedWhile << pattern::anyList);

			VLOG(2) << "While to for on function:\n" << core::printer::PrettyPrinter(lambdaExpr, core::printer::PrettyPrinter::PRINT_MARKERS);

			// match (and potentially replace) all generated while statements
			lambdaExpr = irp::replaceAll(whilePat, core::NodeAddress(lambdaExpr), [&](pattern::AddressMatch match) -> core::StatementPtr {
				auto original = match.getRoot().getAddressedNode().as<core::CompoundStmtPtr>();

				auto whileAddr = match["while"].getFlattened().front().as<core::WhileStmtAddress>();
				auto condition = whileAddr.getAddressedNode()->getCondition();
				auto body = whileAddr.getAddressedNode()->getBody();
				auto pred = match["predecessor"].getFlattened().front();

				VLOG(1) << "======= START WHILE TO FOR ===========\n" << dumpColor(match.getRoot().getAddressedNode());

				// check if body contains flow alterating stmts (break or return. continue is allowed.)
				bool flowAlteration = core::analysis::hasFreeBreakStatement(body) || core::analysis::hasFreeReturnStatement(body);
				VLOG(1) << "--- flow alteration: " << flowAlteration << "\n";
				if(flowAlteration) return original;

				// if more than one free variable in the condition -> we certainly can't create a for for this
				auto cvars = core::analysis::getFreeVariables(condition);

				VLOG(1) << "--- cvars:\n " << cvars << "\n";
				// remove all read only free vars from list
				for (auto it = cvars.rbegin(); it != cvars.rend(); ++it) {
					VLOG(1) << "---- check read only " << *it << "\n";
					if (core::analysis::isReadOnlyWithinScope(body, *it)) {
						cvars.erase(std::next(it).base());
						VLOG(1) << "---- erased, because var is read only " << *it << "\n";
					}
				}

				// check if we use literals (e.g., globals). If yes, return original
				bool litAccess = false;
				core::visitDepthFirstOnce(condition,
					[&](const core::LiteralPtr& lit) {
						if (core::analysis::isRefType(lit->getType())) {
							litAccess = true;;
						}
				});
				VLOG(1) << "--- used literals:\n " << litAccess << "\n";
				if (litAccess) return original;

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
				bool cvar_is_last_statement = true;
				core::visitDepthFirstPrunable(core::NodeAddress(body), [&](const core::NodeAddress& varA) {
					auto var = varA.getAddressedNode().isa<core::VariablePtr>();
					if(var == cvar) {
						// check if it's a write
						auto varParent = varA.getParentNode(2);
						auto convertedPair = mapToStep(varParent);
						if(convertedPair.second && !convertedPair.first) {
							writeStepExprs.push_back(convertedPair.second);
							core::NodePtr parentNode;
							// compound assignment operations are enclosed in an additional refDeref. We have to consider this here
							if(varA.getDepth()>4) {
								auto parentParent = varA.getParentAddress(4);
								if(refExt.isCallOfRefDeref(parentParent)) {
									toRemoveFromBody.push_back(parentParent);
									parentNode = parentParent.getAddressedNode();
								}
							}
							if(!parentNode) {
								toRemoveFromBody.push_back(varParent);
								parentNode = varA.getParentNode(2);
							}
							// additionally check if the condition var write
							// occurs at the end of the while body, otherwise
							// return original code.
							if(body->getStatements().back() != parentNode) cvar_is_last_statement = false;
						}
					}
					if(varA.isa<core::LambdaExprAddress>()) return true;
					return false;
				});
				VLOG(1) << "WriteStepExprs: " << writeStepExprs << "\n";

				VLOG(1) << "Step expression is last statement: " << cvar_is_last_statement << "\n";
				if(!cvar_is_last_statement) return original;

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
				loopVarReplacement[builder.deref(cvar)] = forVar; // TODO: replace cvar, not deref(cvar) -- issues with binds expecting refs
				for(auto stmt : body->getStatements()) {
					if(!::contains(toRemoveFromBody, stmt)) {
						newBodyStmts.push_back(core::transform::replaceAllGen(mgr, stmt, loopVarReplacement));
						if(!core::analysis::isReadOnly(stmt, cvar)) {
							VLOG(1) << "Loop var not readonly in: " << stmt;
							return original;
						}
					}
				}

				auto newBody = builder.compoundStmt(newBodyStmts);

				// if the old loop variable is free in the new body, we messed up and should bail
				VLOG(2) << core::printer::PrettyPrinter(newBody, core::printer::PrettyPrinter::NO_EVAL_LAZY);
				auto oldLoopVarFree = ::contains(core::analysis::getFreeVariables(newBody), cvar);
				VLOG(1) << "oldLoopVarFree: " << oldLoopVarFree << " // " << "free vars: " << core::analysis::getFreeVariables(newBody) << "\n";
				if(oldLoopVarFree) return original;

				auto forStmt = builder.forStmt(forVar, convertedStartPair.second, convertedEndPair.second, convertedStepExpr, newBody);
				core::transform::utils::migrateAnnotations(whileAddr.getAddressedNode(), forStmt);
				VLOG(1) << "======> FOR:\n" << dumpColor(forStmt);

				/////////////////////////////////////////////////////////////////////////////////////// replace the while in the original compound
				auto replacementCompoundStmt = core::transform::replaceAddress(mgr, whileAddr, forStmt).getRootNode().as<core::CompoundStmtPtr>();
				core::StatementList stmtlist = replacementCompoundStmt->getStatements();

				/////////////////////////////////////////////////////////////////////////////////////// check if post assignment is mandatory
				auto usedAfterLoop = isUsedAfterLoop(original->getStatements(), whileAddr.getAddressedNode(), pred, cvar);
				if(usedAfterLoop) {
					stmtlist.push_back(getPostCondition(cvar, convertedStartPair.second, convertedEndPair.second, convertedStepExpr));
				}

				/////////////////////////////////////////////////////////////////////////////////////// remove declaration if we can
				if(!usedAfterLoop && pred.isa<core::DeclarationStmtPtr>()) {
					stmtlist.erase(std::find(stmtlist.begin(), stmtlist.end(), pred.as<core::StatementPtr>()));
				}

				return builder.compoundStmt(stmtlist);
			}).as<core::LambdaExprPtr>();

			VLOG(1) << "While to for replacement - from function:\n" << dumpColor(fun.second) << " - to function:\n" << dumpColor(lambdaExpr);

			tu.replaceFunction(fun.first, lambdaExpr);
		}
		return tu;
	}

} // extensions
} // frontend
} // insieme
