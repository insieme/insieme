/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
#include "insieme/core/transform/simplify.h"

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

	namespace detail {
		// tries to map a given operation (assignment or increment/decrement) to a step for an IR for loop
		// returns (read/write, step expression)
		MapToStepResult mapToStep(core::NodePtr operation) {
			auto invalid = MapToStepResult{ false, core::ExpressionPtr() };

			auto& mgr = operation->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& rMod = mgr.getLangExtension<core::lang::ReferenceExtension>();
			auto& compOpExt = mgr.getLangExtension<core::lang::CompoundOpsExtension>();

			auto call = operation.isa<core::CallExprPtr>();
			if(!call) return invalid;
			auto callee = call->getFunctionExpr();

			auto litType = call->getType();
			if(core::analysis::isRefType(litType)) litType = core::analysis::getReferencedType(litType); // for compPrefix cases

			if(callee == compOpExt.getCompAssignAdd()) {
				return { false, core::analysis::getArgument(operation, 1) };
			} else if(callee == compOpExt.getCompAssignSubtract()) {
				return { false, builder.sub(builder.literal("0", litType), core::analysis::getArgument(operation, 1)) };
			} else if(rMod.isGenPreInc(callee) || rMod.isGenPostInc(callee) || callee == compOpExt.getCompPrefixInc()) {
				return { false, builder.literal("1", litType) };
			} else if(rMod.isGenPreDec(callee) || rMod.isGenPostDec(callee) || callee == compOpExt.getCompPrefixDec()) {
				return { false, builder.literal("-1", litType) };
			} else if(callee == rMod.getRefDeref()) {
				// just a read, we can ignore this
				return { true, core::ExpressionPtr() };
			}
			return invalid;
		}

		// tries to map a given operation (declaration or assignment) to a start expr for an IR for loop
		// returns (valid, lhs, start expression)
		MapToStartResult mapToStart(core::VariablePtr var, core::NodePtr operation) {
			auto invalid = MapToStartResult{ false, core::ExpressionPtr(), core::ExpressionPtr() };

			auto& mgr = operation->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& fMod = mgr.getLangExtension<utils::FrontendInspireModule>();

			// first check if declaration
			if(auto decl = operation.isa<core::DeclarationStmtPtr>()) {
				return { decl->getVariable() == var, decl->getVariable(), decl->getInitialization() };
			}

			// otherwise assignment call
			auto call = operation.isa<core::CallExprPtr>();
			if(!call) return invalid;
			auto callee = call->getFunctionExpr();
			if(callee == fMod.getCStyleAssignment() || callee == fMod.getCxxStyleAssignment()) {
				auto lhs = call->getArgument(0);
				return { lhs == var, lhs, call->getArgument(1) };
			}
			return invalid;
		}

		// tries to map a given operation (conditional) to an end expr for an IR for loop
		MapToendResult mapToEnd(core::VariablePtr var, core::ExpressionPtr operation) {
			auto invalid = MapToendResult{ true, core::ExpressionPtr() };

			auto& mgr = operation->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& basic = mgr.getLangBasic();
			auto& rMod = mgr.getLangExtension<core::lang::ReferenceExtension>();

			// operation needs to be a call and have 2 arguments
			auto callExpr = operation.isa<core::CallExprPtr>();
			if(!callExpr || callExpr->getNumArguments() != 2) return invalid;
			auto callee = callExpr->getFunctionExpr();

			auto lhs = callExpr->getArgument(0).isa<core::CallExprPtr>();
			auto rhs = callExpr->getArgument(1);

			auto rhsType = rhs->getType();
			// if the LHS is wrapped in a numCast, we strip the cast but convert the RHS to the type of LHS - note that a numCast to the same type is a no-op
			if(basic.isNumericCast(lhs->getFunctionExpr())) {
				lhs = lhs->getArgument(0).isa<core::CallExprPtr>();
				rhsType = lhs->getType();
			}

			// first check if variable is on LHS
			// (should also work for rhs -- not yet)
			if(!rMod.isRefDeref(lhs->getFunctionExpr())) return invalid;
			if(lhs->getArgument(0) != var) return invalid;

			// now check type of comparison
			if(basic.isSignedIntNe(callee) || basic.isUnsignedIntNe(callee) || basic.isCharNe(callee) || basic.isSignedIntLt(callee)
			   || basic.isUnsignedIntLt(callee) || basic.isCharLt(callee)) {
				return { true, builder.numericCast(rhs, rhsType) };
			} else if(basic.isSignedIntGt(callee) || basic.isUnsignedIntGt(callee) || basic.isCharGt(callee)) {
				// don't handle these for now, complex to say for certain what to do in all cases
				return invalid;
			} else if(basic.isSignedIntLe(callee) || basic.isUnsignedIntLe(callee) || basic.isCharLe(callee)) {
				return { true, builder.numericCast(builder.add(rhs, builder.literal("1", rhs->getType())), rhsType) };
			} else if(basic.isSignedIntGe(callee) || basic.isUnsignedIntGe(callee) || basic.isCharGe(callee)) {
				//return builder.numericCast(builder.sub(rhs, builder.literal("1", rhs->getType())), rhsType);
				// don't handle these for now, complex to say for certain what to do in all cases
				return invalid;
			}

			return invalid;
		}

	} // detail namespace

	namespace {
		core::NodePtr removeNoopsFromCompound(const core::NodePtr& ptr) {
			auto comp = ptr.isa<core::CompoundStmtPtr>();
			if(!comp) return ptr;

			core::IRBuilder builder(ptr->getNodeManager());
			core::StatementList newStmts;
			for(const auto& stmt : comp->getStatements()) {
				if(stmt != builder.getNoOp()) newStmts.push_back(stmt);
			}
			auto newCompound = builder.compoundStmt(newStmts);
			core::transform::utils::migrateAnnotations(comp, newCompound);
			return newCompound;
		}

	} // anon namespace

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

				// check if body contains flow altering stmts (break or return. continue is allowed.)
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
				VLOG(1) << "--- used literals in condition:\n " << litAccess << "\n";
				if(litAccess) return original;

				if(cvars.size() > 1 || cvars.size() == 0) return original;

				auto cvar = cvars.front();

				core::TypePtr cvarType = core::analysis::getReferencedType(cvar->getType());
				VLOG(1) << "cvar: " << dumpColor(cvar) << "\n -- type: " << cvarType << "\n";

				VLOG(1) << "condition: " << dumpColor(condition) << "\n";

				// if it's not an integer, we bail out for now
				if(!basic.isInt(cvarType)) return original;

				/////////////////////////////////////////////////////////////////////////////////////// figuring out the step

				// find all uses of the variable in the while body
				core::ExpressionPtr writeStepExpr = nullptr;
				core::NodeList toRemoveFromBody;
				bool cvarIsLastStatementOrBeforeContinue = true;
				bool writeStepExprsAreTheSame = true;
				core::visitDepthFirstPrunable(core::NodeAddress(body), [&](const core::NodeAddress& varA) {
					auto var = varA.getAddressedNode().isa<core::VariablePtr>();
					if(var == cvar && varA.getDepth()>2) {
						// check if it's a write
						auto varParent = varA.getParentNode(2);
						auto stepResult = detail::mapToStep(varParent);
						if(stepResult.stepExpr && !stepResult.readOnly) {
							if(writeStepExpr && stepResult.stepExpr != writeStepExpr) writeStepExprsAreTheSame = false;
							writeStepExpr = stepResult.stepExpr;
							core::NodeAddress parentAddr;
							// compound assignment operations are enclosed in an additional refDeref. We have to consider this here
							if(varA.getDepth()>4) {
								auto parentParent = varA.getParentAddress(4);
								if(refExt.isCallOfRefDeref(parentParent)) {
									toRemoveFromBody.push_back(parentParent);
									parentAddr = parentParent;
								}
							}
							if(!parentAddr) {
								toRemoveFromBody.push_back(varParent);
								parentAddr = varA.getParentAddress(2);
							}
							// additionally check if the condition var write
							// occurs at the end of the while body ...
							if(core::NodeAddress(body)->getChildList().back() != parentAddr) {
								// ..or before continue
								auto surroundingScope = parentAddr.getParentAddress();
								auto idx = parentAddr.getIndex();
								if(!surroundingScope
								  || idx+1 >= surroundingScope.getChildList().size()
								  || !surroundingScope->getChild(parentAddr.getIndex()+1).isa<core::ContinueStmtPtr>()) {
									cvarIsLastStatementOrBeforeContinue = false;
								}
							}
						}
					}
					if(varA.isa<core::LambdaExprAddress>()) return true;
					return false;
				});

				VLOG(1) << "Found step expression: " << writeStepExpr << "\n";
				if(!writeStepExpr) return original;

				VLOG(1) << "Step expressions are last statement or before continue: " << cvarIsLastStatementOrBeforeContinue << "\n";
				if(!cvarIsLastStatementOrBeforeContinue) return original;

				VLOG(1) << "Step expressions are are the same: " << writeStepExprsAreTheSame << "\n";
				if(!writeStepExprsAreTheSame) return original;

				VLOG(1) << "WriteStepExpr: " << writeStepExpr << "\n";
				// if there there are free variables in the step, bail out (could be much smarter about the step)
				if(!core::analysis::getFreeVariables(writeStepExpr).empty()) return original;
				auto convertedStepExpr = writeStepExpr;

				/////////////////////////////////////////////////////////////////////////////////////// figuring out start

				auto convertedStart = detail::mapToStart(cvar, pred);
				VLOG(1) << "StartPair: " << convertedStart.isValid << " // " << convertedStart.lhs << " // " << convertedStart.startExpr << "\n";

				// bail if no valid start found
				if(!convertedStart.isValid) return original;

				/////////////////////////////////////////////////////////////////////////////////////// figuring out end

				auto convertedEnd = detail::mapToEnd(cvar, condition);
				VLOG(1) << "End: " << convertedEnd.endExpr << "\n";

				// bail if no valid end found
				if(!convertedEnd.endExpr) return original;

				/////////////////////////////////////////////////////////////////////////////////////// build the for

				VLOG(1) << "toRemoveFromBody:\n";
				for(const auto& rem : toRemoveFromBody) {
					VLOG(1) << "\t" << dumpReadable(rem);
				}

				auto forVar = builder.variable(cvarType);

				// prepare new body
				auto newBody = core::transform::transformBottomUpGen(body, [&](const core::NodePtr ptr) -> core::NodePtr {
					if(::contains(toRemoveFromBody, ptr)) return builder.getNoOp();
					if(ptr == builder.deref(cvar)) return forVar; // TODO: replace cvar, not deref(cvar) -- issues with binds expecting refs
					return removeNoopsFromCompound(ptr);
				});

				// if the old loop variable is free in the new body, we messed up and should bail
				VLOG(2) << core::printer::PrettyPrinter(newBody, core::printer::PrettyPrinter::NO_EVAL_LAZY);
				auto oldLoopVarFree = ::contains(core::analysis::getFreeVariables(newBody), cvar);
				VLOG(1) << "oldLoopVarFree: " << oldLoopVarFree << " // " << "free vars: " << core::analysis::getFreeVariables(newBody) << "\n";
				if(oldLoopVarFree) return original;

				auto forStmt = builder.forStmt(forVar, convertedStart.startExpr, convertedEnd.endExpr, convertedStepExpr, newBody);
				core::transform::utils::migrateAnnotations(whileAddr.getAddressedNode(), forStmt);
				VLOG(1) << "======> FOR:\n" << dumpColor(forStmt);

				/////////////////////////////////////////////////////////////////////////////////////// replace the while in the original compound
				auto replacementCompoundStmt = core::transform::replaceAddress(mgr, whileAddr, forStmt).getRootNode().as<core::CompoundStmtPtr>();
				core::StatementList stmtlist = replacementCompoundStmt->getStatements();

				/////////////////////////////////////////////////////////////////////////////////////// check if post assignment is mandatory
				auto usedAfterLoop = isUsedAfterLoop(original->getStatements(), whileAddr.getAddressedNode(), pred, cvar);
				if(usedAfterLoop) {
					stmtlist.push_back(getPostCondition(cvar, convertedStart.startExpr, convertedEnd.endExpr, convertedStepExpr));
				}

				/////////////////////////////////////////////////////////////////////////////////////// remove declaration if we can
				if(!usedAfterLoop && pred.isa<core::DeclarationStmtPtr>()) {
					stmtlist.erase(std::find(stmtlist.begin(), stmtlist.end(), pred.as<core::StatementPtr>()));
				}

				return builder.compoundStmt(stmtlist);
			}).as<core::LambdaExprPtr>();

			if(fun.second != lambdaExpr) {
				VLOG(1) << "While to for replacement - from function:\n" << dumpColor(fun.second) << " - to function:\n" << dumpColor(lambdaExpr);
			}
			else {
				VLOG(1) << "Not replacing this while\n";
			}

			tu.replaceFunction(fun.first, lambdaExpr);
		}
		return tu;
	}

} // extensions
} // frontend
} // insieme
