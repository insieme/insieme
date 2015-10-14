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

#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace checks {

	namespace {
		NodeAddress firstAddress(NodeAddress start, NodePtr node) {
			NodeAddress retval;
			visitDepthFirstInterruptible(start, [&](const NodeAddress& addr) -> bool {
				if(*node == *addr.getAddressedNode()) {
					retval = addr;
					return true;
				}
				return false;
			});
			return retval;
		}
	}

	OptionalMessageList ScalarArrayIndexRangeCheck::visitCallExpr(const CallExprAddress& curcall) {
		OptionalMessageList res;
		auto& mgr = curcall->getNodeManager();
		auto& refExt = mgr.getLangExtension<lang::ReferenceExtension>();
		IRBuilder builder(mgr);

		CallExprPtr curPtr = curcall.getAddressedNode();

		for(unsigned argIndex = 0; argIndex < curPtr->getArguments().size(); ++argIndex) {
			// the potential outer call to scalar.to.array in one of curcall's parameters
			ExpressionPtr curArg = curPtr->getArgument(argIndex);
			if(!core::analysis::isCallOf(curArg, refExt.getRefScalarToRefArray())) { continue; }
			LambdaExprPtr called = dynamic_pointer_cast<const LambdaExpr>(curPtr->getFunctionExpr());
			if(!called) { continue; }
			VariablePtr param = called->getParameterList()[argIndex];
			// LOG(INFO) << "**************************************\n====\nparam:\n " << printer::PrettyPrinter(param) << "\n*********************\n";
			NodeAddress addr = firstAddress(curcall, called->getBody());
			if(addr) {
				visitDepthFirst(addr, [&](const VariableAddress& var) {
					if(*var.getAddressedNode() != *param) { return; }
					if(var.isRoot() || var.getDepth() < 2) { return; }
					if(!analysis::isCallOf(var.getParentAddress(1), refExt.getRefDeref())) { return; }
					if(var.getParentAddress(2).getNodeType() != NT_CallExpr) { return; }
					CallExprAddress useCallAdr = var.getParentAddress(2).as<CallExprAddress>();
					CallExprPtr usecall = useCallAdr;
					if(usecall) {
						if(refExt.isRefArrayElement(usecall->getFunctionExpr())) {
							try {
								auto formula = arithmetic::toFormula(usecall->getArgument(1));
								if(formula.isZero()) {
									// correct use
								} else {
									add(res, Message(useCallAdr, EC_SEMANTIC_ARRAY_INDEX_OUT_OF_RANGE,
									                 format("Potentially unsafe indexing of single-element array %s using formula %s",
									                        *param, formula),
									                 Message::WARNING));
								}
							} catch(const arithmetic::NotAFormulaException& e) {
								add(res, Message(useCallAdr, EC_SEMANTIC_ARRAY_INDEX_OUT_OF_RANGE,
								                 format("Potentially unsafe indexing of single-element array %s using expression %s",
								                        *param, *(usecall->getArgument(1))),
								                 Message::WARNING));
							}
						} else {
							// warn here as well? (used in unexpected call)
						}
					} else {
						// warn here as well? (used in non-call)
					}
				});
			}
		}

		return res;
	}

	// OptionalMessageList UndefinedCheck::visitCallExpr(const CallExprAddress& curcall) {
	//	OptionalMessageList res;
	//
	//	if (curcall.isRoot()) {
	//		return res;
	//	}
	//
	//	auto& mgr = curcall->getNodeManager();
	//	auto& basic = mgr.getLangBasic();
	//	if(!core::analysis::isCallOf(curcall.getAddressedNode(), basic.getUndefined())) return res;
	//
	//	// find first non-marker / helper parent
	//	NodeAddress cur = curcall.getParentAddress();
	//	while(!cur.isRoot() && cur->getNodeCategory() == NC_Support) {
	//		cur = cur.getParentAddress();
	//	}
	//
	//	NodePtr parent = cur.getAddressedNode();
	//
	//	// check if parent in allowed set
	//	NodeType pnt = parent->getNodeType();
	//	if(core::analysis::isCallOf(parent, basic.getRefNew())
	//		|| core::analysis::isCallOf(parent, basic.getRefVar())
	//		|| core::analysis::isCallOf(parent, basic.getVectorInitUniform())
	//		|| (core::analysis::isConstructorExpr(parent) && pnt != NT_JobExpr)) {
	//
	//		return res;
	//	}
	//	// error if not
	//	std::cout << "\n~~~~~~~~~~~~~~~~~~ Node type of parent: " << pnt << std::endl;
	//	add(res, Message(curcall,
	//		EC_SEMANTIC_INCORRECT_UNDEFINED,
	//		string("Call to undefined(...) not enclosed within ref.new, ref.var or constructor expression"),
	//		Message::ERROR));
	//	return res;
	//}

	OptionalMessageList FreeBreakInsideForLoopCheck::visitForStmt(const ForStmtAddress& curfor) {
		OptionalMessageList res;
		auto& mgr = curfor->getNodeManager();
		IRBuilder builder(mgr);

		core::visitDepthFirstPrunable(curfor->getBody(), [&](const core::NodeAddress& cur) -> bool {
			if(cur.isa<core::BreakStmtAddress>()) {
				add(res, Message(cur, EC_SEMANTIC_FREE_BREAK_INSIDE_FOR_LOOP,
				                 format("Free break statements are not allowed inside for loops. Consider while loop instead."), Message::ERROR));
			}

			if(cur.isa<core::LambdaExprAddress>() || cur.isa<core::ForStmtAddress>() || cur.isa<core::WhileStmtAddress>()
			   || cur.isa<core::SwitchStmtAddress>()) {
				return true;
			} else {
				return false;
			}
		});

		return res;
	}

	namespace {
		class ReturnStmtCheckVisitor : public IRVisitor<bool, Pointer, bool> {
			bool visitReturnStmt(const ReturnStmtPtr&, bool) override {
				LOG(DEBUG) << "Visit return!\n";
				return true;
			}

			bool visitThrowStmt(const ThrowStmtPtr&, bool) override {
				LOG(DEBUG) << "Visit throw!\n";
				return true;
			}

			bool visitIfStmt(const IfStmtPtr& ifst, bool inInfiniteLoop) override {
				LOG(DEBUG) << "Visit if ---- \n" << dumpColor(ifst) << "\n-----";
				bool ifSide = visit(ifst->getThenBody(), inInfiniteLoop);
				bool elseSide = visit(ifst->getElseBody(), inInfiniteLoop);
				return inInfiniteLoop ? (ifSide || elseSide) : (ifSide && elseSide);
			}

			bool visitSwitchStmt(const SwitchStmtPtr& switchst, bool inInfiniteLoop) override {
				LOG(DEBUG) << "Visit switch ---- \n" << dumpColor(switchst) << "\n-----";
				bool ok = true;
				auto cases = switchst->getCases();
				for(auto switchcase : cases) {
					bool caseOk = visit(switchcase->getBody(), inInfiniteLoop);
					ok = inInfiniteLoop ? (ok || caseOk) : (ok && caseOk);
				}
				bool defaultOk = visit(switchst->getDefaultCase(), inInfiniteLoop);
				ok = inInfiniteLoop ? (ok || defaultOk) : (ok && defaultOk);
				return ok;
			}

			bool visitWhileStmt(const WhileStmtPtr& whileStmt, bool) override {
				LOG(DEBUG) << "Visit while ---- \n" << dumpColor(whileStmt) << "\n-----";
				auto cond = whileStmt->getCondition();
				bool infiniteLoop = false;
				try {
					auto constraint = arithmetic::toConstraint(cond);
					infiniteLoop = constraint.isValid();
				} catch(arithmetic::NotAConstraintException) { /* not a constraint, not our problem */ }
				LOG(DEBUG) << " --> infinite? " << infiniteLoop;
				return visit(whileStmt->getBody(), infiniteLoop);
			}

			bool visitCompoundStmt(const CompoundStmtPtr& comp, bool inInfiniteLoop) override {
				LOG(DEBUG) << "Visit compound ---- \n" << dumpColor(comp) << "\n-----";
				auto elems = comp->getStatements();
				if(elems.empty()) { return false; }
				for(auto i = elems.cend() - 1; i != elems.cbegin(); --i) {
					bool r = visit(*i, inInfiniteLoop);
					if(r) { return true; }
				}
				return visit(elems.front(), inInfiniteLoop);
			}
		};
	}

	OptionalMessageList MissingReturnStmtCheck::visitLambdaExpr(const LambdaExprAddress& lambdaExpr) {
		OptionalMessageList res;

		auto& nodeMan = lambdaExpr->getNodeManager();
		auto& basic = nodeMan.getLangBasic();

		// check non-unit return types
		if(!basic.isUnit(lambdaExpr->getFunctionType()->getReturnType())) {
			// skip constructors and destructors
			const auto kind = lambdaExpr->getFunctionType()->getKind();
			if(kind != FK_CONSTRUCTOR && kind != FK_DESTRUCTOR) {
				LOG(DEBUG) << " --> engaging check\n";
				ReturnStmtCheckVisitor checker;
				if(!checker.visit(lambdaExpr->getBody(), false)) {
					LOG(DEBUG) << "MissingReturnStmtCheck failed for: \n" << dumpColor(lambdaExpr);
					VLOG(1) << " -> Text dump:\n" << dumpText(lambdaExpr);
					add(res, Message(lambdaExpr, EC_SEMANTIC_MISSING_RETURN_STMT, format("Not all control paths of non-unit lambdaExpr return a value."),
					                 Message::ERROR));
				}
			}
		}

		return res;
	}

	OptionalMessageList ArrayCreateArgumentCheck::visitCallExpr(const CallExprAddress& callExpr) {
		OptionalMessageList res;

		auto& manager = callExpr->getNodeManager();
		const core::lang::ArrayExtension& arrayExt = manager.getLangExtension<core::lang::ArrayExtension>();

		// check if we have a call to array_create
		CallExprPtr curPtr = callExpr.getAddressedNode();
		if(arrayExt.isCallOfArrayCreate(curPtr)) {
			if(!encoder::isEncodingOf<ExpressionList, encoder::DirectExprListConverter>(curPtr[1])) {
				add(res, Message(callExpr, EC_SEMANTIC_ARRAY_CREATE_INVALID_ARGUMENT,
				    format("Invalid initializer argument in array_create call."), Message::ERROR));
			}
		}
		return res;
	}

} // end namespace check
} // end namespace core
} // end namespace insieme
