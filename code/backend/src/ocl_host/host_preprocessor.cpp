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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/ir_check.h"
#include "insieme/utils/logging.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/ocl_host/host_extensions.h"
#include "insieme/backend/ocl_host/host_preprocessor.h"

#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/pattern/ir_generator.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/backend/ocl_kernel/kernel_extensions.h"

#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/annotations/c/naming.h"

using namespace insieme::transform::pattern;
using namespace insieme::core;
namespace irg = insieme::transform::pattern::generator::irg;

namespace insieme {
namespace backend {
namespace ocl_host {

using insieme::transform::pattern::any;
using insieme::transform::pattern::anyList;

	core::VariablePtr getVar(MatchOpt& match, const std::string& str){
		return static_pointer_cast<const Variable>(match->getVarBinding(str).getValue());
	}

	core::NodePtr HostPreprocessor::process(core::NodeManager& manager, const core::NodePtr& code) {
		core::IRBuilder builder(manager);
		auto& ext = manager.getLangExtension<Extensions>();
		auto& kernelExt = manager.getLangExtension<ocl_kernel::Extensions>();

		// create new buffer type
		TypePtr bufType = ext.bufferType;
		TypePtr refBufType = ext.refBufferType;
		std::vector<VariablePtr> bufVars; // vector of buffer variable

		// Used only in case of tuple (not for icl library)
		std::vector<LiteralPtr> litVec;
		VariablePtr tupleVar;
		TypePtr newTupleType;

		// Extract the tuple and the access num from the kernel call
		TreePatternPtr wrapGlobalTuple = irp::callExpr(irp::literal("_ocl_wrap_global"),
									irp::callExpr(any, irp::literal("tuple.member.access"),
									irp::callExpr(irp::literal("ref.deref"), var("tupleVar") << *any)
									<< var("num") << *any) << *any);

		TreePatternPtr wrapGlobalVar = irp::callExpr(irp::literal("_ocl_wrap_global"),
									irp::callExpr(irp::literal("ref.deref"), var("bufVar") << *any) << *any);

		visitDepthFirst(code, [&](const CallExprPtr& call) {
			auto&& match = wrapGlobalTuple->matchPointer(call);
			if (match) {
				tupleVar = getVar(match, "tupleVar");
				litVec.push_back(static_pointer_cast<const Literal>(match->getVarBinding("num").getValue()));
			}
			match = wrapGlobalVar->matchPointer(call);
			if (match) {
				bufVars.push_back(getVar(match, "bufVar"));
			}
		});

		if(tupleVar) {
			// inserts in bufVars all the variables of type array(will become buffers) getting them from the tuple
			std::vector<TreePatternPtr> treeVecInsertTuple;
			for_each(litVec, [&](const LiteralPtr& lit) {
				treeVecInsertTuple.push_back(irp::callExpr(
						aT(irp::callExpr(any, irp::literal("tuple.ref.elem"), any << atom(lit) << *any)),
						atom(tupleVar) << irp::callExpr(irp::literal("ref.deref"), var("bufVar") << *any)));
			});

			visitDepthFirst(code, [&](const CallExprPtr& call) {
				for_each(treeVecInsertTuple, [&](const TreePatternPtr& pattern) {
					auto&& match = pattern->matchPointer(call);
					if (match) {
						bufVars.push_back(getVar(match, "bufVar"));
					}
				});
			});

			const TupleTypePtr oldTupleType = static_pointer_cast<const TupleType>(static_pointer_cast<const RefType>(tupleVar.getType())->getElementType());
			std::vector<TypePtr> oldTypeList = oldTupleType.getElementTypes();
			std::vector<TypePtr> newTypeList;
			for (uint i = 0; i < litVec.size(); ++i)
				newTypeList.push_back(refBufType);
			for (uint i = litVec.size(); i < oldTypeList.size(); ++i)
				newTypeList.push_back(oldTypeList[i]);
			// create the new tuple type
			newTupleType = builder.tupleType(newTypeList);
		}

		// modify buffer declaration (map to new one)
		utils::map::PointerMap<NodePtr, NodePtr> nodeMap;
		auto& basic = manager.getLangBasic();
		visitDepthFirst(code, [&](const DeclarationStmtPtr& decl) {
			auto&& fit = std::find_if(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){ return *vr == *(decl->getVariable());});
			if (fit != bufVars.end()) {
				CallExprPtr init = static_pointer_cast<const CallExpr>(decl->getInitialization());
				if (core::analysis::isCallOf(init->getArgument(0), builder.getLangBasic().getUndefined())) {
					DeclarationStmtPtr newDecl = builder.declarationStmt(
							builder.variable(builder.refType(refBufType), decl.getVariable().getID()),
							builder.refVar(builder.callExpr(refBufType, basic.getUndefined(),builder.getTypeLiteral(refBufType))));
					nodeMap.insert(std::make_pair(decl, newDecl));
					return;
				}

				CallExprPtr tmp = static_pointer_cast<const CallExpr>((init->getArgument(0)));
				DeclarationStmtPtr newDecl = builder.declarationStmt(
						builder.variable(builder.refType(refBufType), decl.getVariable().getID()),
						builder.refVar(builder.callExpr(ext.createBuffer, tmp->getArgument(1),
						builder.callExpr(basic.getUnsignedIntMul(), builder.callExpr(basic.getSizeof(), tmp->getArgument(0)), tmp->getArgument(2)))));
				nodeMap.insert(std::make_pair(decl, newDecl));
				return;
			}

			if (tupleVar) { // modify tuple declaration
				if (*tupleVar == *(decl->getVariable())) {
					DeclarationStmtPtr newDecl = builder.declarationStmt(
						builder.variable(builder.refType(newTupleType), decl.getVariable().getID()),
						builder.refNew(builder.callExpr(newTupleType, basic.getUndefined(),builder.getTypeLiteral(newTupleType))));
					nodeMap.insert(std::make_pair(decl, newDecl));
					return;
				}
			}
		});

		TreePatternPtr bufVarIntuple = irp::callExpr(aT(irp::lambda(any, any << var("oldpar2"), irp::compoundStmt(
											irp::callExpr(any, irp::callExpr(any, any << var("lit") << *any) << *any) << *any))),
											var("tuple", irp::variable(any, any)) << irp::callExpr(irp::literal("ref.deref"),
											single(var("bufVar", irp::variable(any, any)))));

		// Tree to match write buffer
		TreePatternPtr readWriteBuf = irp::callExpr(aT(irp::lambda(any, var("1var") << *any, aT(irp::callExpr(atom(builder.getLangBasic().getRefAssign()),
								irp::callExpr(irp::literal("array.ref.elem.1D"), var("2var") << *any) <<
								irp::callExpr(irp::literal("ref.deref"), single(irp::callExpr(irp::literal("array.ref.elem.1D"), var("3var") << *any))))))),
								irp::callExpr(irp::literal("ref.deref"), single(var("bufVar", irp::variable(any, any)))) <<
								irp::castExpr(any, irp::literal(any, any)) << *any);

		TreePatternPtr delTree = irp::callExpr(any, irp::literal("ref.delete"), single(var("bufVar", irp::variable(any, any))));

		// Tree to match the kernel call
		TreePatternPtr kernelCall = irp::callExpr(irp::literal("call_kernel"), irp::callExpr(irp::literal("_ocl_kernel_wrapper"), single(any)) << *any << var("varlist"));

		TreePatternPtr tupleAccess = irp::callExpr(any, irp::literal("tuple.member.access"),
										irp::callExpr(irp::literal("ref.deref"), var("tupleVar") << *any) << var("lit") << var("typeVar"));

		visitDepthFirst(code, [&](const CallExprPtr& call) {
			// modify buffer assignment operation
			if (core::analysis::isCallOf(call, builder.getLangBasic().getRefAssign())) {
				auto&& fit = std::find_if(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){ return *vr == *(call->getArgument(0));});
				if (fit != bufVars.end()) {
					CallExprPtr tmp = static_pointer_cast<const CallExpr>((call->getArgument(1)));
					VariablePtr newVar = builder.variable(builder.refType(refBufType), static_pointer_cast<const Variable>(call->getArgument(0)).getID());
					CallExprPtr newCall = builder.callExpr(call->getFunctionExpr(), newVar,
							builder.callExpr(ext.createBuffer, tmp->getArgument(1),
							builder.callExpr(basic.getUnsignedIntMul(), builder.callExpr(basic.getSizeof(), tmp->getArgument(0)), tmp->getArgument(2))));
					nodeMap.insert(std::make_pair(call, newCall));
					return;
				}
			}

			if (tupleVar) { // modify insert in the tuple
				auto&& matchBuf = bufVarIntuple->matchPointer(call);
				if (matchBuf) { // FIXME: WRITE IN A BETTER WAY
					if (*tupleVar == *(getVar(matchBuf, "tuple"))) {
						VariablePtr newTupleVar = builder.variable(builder.refType(newTupleType), tupleVar.getID());
						bool var_flag = false;
						for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
							if (*vr == *(getVar(matchBuf, "bufVar"))) {
								var_flag = true;
								VariablePtr newVar = builder.variable(refBufType, vr.getID());
								VariablePtr refNewVar = builder.variable(builder.refType(refBufType), vr.getID());
								VariablePtr intVar = builder.variable(refBufType);
								VariablePtr intTuple = builder.variable(builder.refType(newTupleType));
								FunctionTypePtr newFunType = builder.functionType(toVector(newTupleVar->getType(), newVar->getType()), basic.getInt4());
								// build the new body
								std::vector<core::StatementPtr> newBodyStmts;
								ExpressionPtr lit = static_pointer_cast<const ExpressionPtr>(matchBuf->getVarBinding("lit").getValue());
								CallExprPtr refCall = builder.callExpr(basic.getTupleRefElem(), intTuple, lit, builder.getTypeLiteral(refBufType));
								newBodyStmts.push_back(builder.callExpr(basic.getRefAssign(), refCall, intVar));
								newBodyStmts.push_back(builder.returnStmt(builder.intLit(0)));
								// build the new call
								insieme::core::VariableList varList;
								varList.push_back(intTuple);
								varList.push_back(intVar);
								CallExprPtr newCall = builder.callExpr(builder.lambdaExpr(newFunType, varList, builder.compoundStmt(newBodyStmts)),
															newTupleVar, builder.callExpr(basic.getRefDeref(), refNewVar));
								nodeMap.insert(std::make_pair(call, newCall));
							}
						});
						if (!var_flag){
							VariablePtr newVar = getVar(matchBuf, "bufVar");
							VariablePtr oldpar2 = getVar(matchBuf, "oldpar2");
							VariablePtr intTuple = builder.variable(builder.refType(newTupleType));
							FunctionTypePtr newFunType = builder.functionType(toVector(newTupleVar->getType(), oldpar2->getType()), basic.getInt4());
							// build the new body
							std::vector<core::StatementPtr> newBodyStmts;
							ExpressionPtr lit = static_pointer_cast<const ExpressionPtr>(matchBuf->getVarBinding("lit").getValue());
							CallExprPtr refCall = builder.callExpr(basic.getTupleRefElem(), intTuple, lit, builder.getTypeLiteral(oldpar2->getType()));
							newBodyStmts.push_back(builder.callExpr(basic.getRefAssign(), refCall, oldpar2));
							newBodyStmts.push_back(builder.returnStmt(builder.intLit(0)));
							// build the new call
							insieme::core::VariableList varList;
							varList.push_back(intTuple);
							varList.push_back(oldpar2);
							CallExprPtr newCall = builder.callExpr(builder.lambdaExpr(newFunType, varList, builder.compoundStmt(newBodyStmts)),
															newTupleVar, builder.callExpr(basic.getRefDeref(), newVar));
							nodeMap.insert(std::make_pair(call, newCall));
						}
						return;
					}
				}
			}

			// modify read write buffer
			auto&& matchReadWriteBuf = readWriteBuf->matchPointer(call);
			if (matchReadWriteBuf) {
				for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
					if (*vr == *(getVar(matchReadWriteBuf, "bufVar"))) {
						LiteralPtr op;
						if (*(getVar(matchReadWriteBuf, "1var")) == *(getVar(matchReadWriteBuf, "2var")))
							op = ext.writeBuffer;
						else
							op = ext.readBuffer;
						// (*(getVar(matchReadWriteBuf, "1var")) == *(getVar(matchReadWriteBuf, "3var")))
						ExpressionPtr newVar = builder.variable(builder.refType(refBufType), vr.getID());
						ExpressionPtr refDeref = builder.callExpr(basic.getRefDeref(), newVar);
						CallExprPtr newCall = builder.callExpr(op, toVector(refDeref, call->getArgument(1),
								call->getArgument(2), call->getArgument(3), call->getArgument(4)));
						nodeMap.insert(std::make_pair(call, newCall));
					}
				});
			}

			// modify delete buffer
			auto&& matchDel = delTree->matchPointer(call);
			if (matchDel) {
				for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
					VariablePtr tmpVar = getVar(matchDel, "bufVar");
					if (*vr == *tmpVar) {
						VariablePtr newVar = builder.variable(builder.refType(refBufType), vr.getID());
						CallExprPtr newCall = builder.callExpr(ext.releaseBuffer, builder.callExpr(basic.getRefDeref(), newVar));
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
					if (tupleVar) { // replace tuple type for the delete operation
						if (*tupleVar == *tmpVar) {
							VariablePtr newTupleVar = builder.variable(builder.refType(newTupleType), tupleVar.getID());
							CallExprPtr newCall = builder.callExpr(basic.getRefDelete(), newTupleVar);
							nodeMap.insert(std::make_pair(call, newCall));
							return;
						}
					}
				});
			}

			// replace tuple type in the kernel call
			auto&& matchKernel = kernelCall->matchPointer(call);
			if (matchKernel) {
				CallExprPtr varlist = static_pointer_cast<const CallExpr>(matchKernel->getVarBinding("varlist").getValue());
				visitDepthFirst(varlist, [&](const CallExprPtr& call) {
					auto&& matchAccess = tupleAccess->matchPointer(call);
					if (matchAccess) {
						VariablePtr tupleVar = getVar(matchAccess, "tupleVar");
						VariablePtr newTuple = builder.variable(builder.refType(newTupleType), tupleVar.getID());
						ExpressionPtr lit = static_pointer_cast<const ExpressionPtr>(matchAccess->getVarBinding("lit").getValue());
						LiteralPtr litTypeVar = static_pointer_cast<const Literal>(matchAccess->getVarBinding("typeVar").getValue());
						GenericTypePtr type = static_pointer_cast<const GenericType>(litTypeVar->getType());
						RefTypePtr ref = dynamic_pointer_cast<const RefType>(type->getTypeParameter(0));
						CallExprPtr newCall;
						if (ref)
							newCall = builder.callExpr(basic.getTupleMemberAccess(), builder.callExpr(basic.getRefDeref(), newTuple), lit, builder.getTypeLiteral(refBufType));
						else
							newCall = builder.callExpr(basic.getTupleMemberAccess(), builder.callExpr(basic.getRefDeref(), newTuple), lit, litTypeVar);
						nodeMap.insert(std::make_pair(call, newCall));
					}

					// replace the buffer type in the kernel call
					matchAccess = wrapGlobalVar->matchPointer(call);
					if (matchAccess) {
						VariablePtr bufVar = getVar(matchAccess, "bufVar");
						ExpressionPtr newVar = builder.variable(builder.refType(refBufType), bufVar.getID());
						CallExprPtr newCall = builder.callExpr(kernelExt.wrapGlobal, builder.callExpr(basic.getRefDeref(), newVar));
						nodeMap.insert(std::make_pair(call, newCall));
					}

				});
			}
		});


		NodePtr code2 = core::transform::replaceAll(manager, code, nodeMap, true);
		nodeMap.clear();

		// Semantic check on code2
		auto semantic = core::check(code2, insieme::core::checks::getFullCheck());
		auto warnings = semantic.getWarnings();
		std::sort(warnings.begin(), warnings.end());
		for_each(warnings, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});

		auto errors = semantic.getErrors();
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});

		//std::cout << core::printer::PrettyPrinter(code2, core::printer::PrettyPrinter::OPTIONS_DETAIL);
		//return code2;

		// Generate the Work Item from the OpenCL kernel
		auto& runtimeExt = manager.getLangExtension<runtime::Extensions>();

		auto at = [&manager](string str) { return irp::atom(manager, str); };
		TreePatternPtr splitPoint = irp::ifStmt(at("(lit<uint<4>, 1> != CAST<uint<4>>(0))"), any, any);

		// search for the cname: size fix with better pattern when we have def use analysis
		ExpressionPtr sizeExpr;
		visitDepthFirstInterruptible(code2, [&](const VariablePtr& var) -> bool {
			if(var->hasAnnotation(annotations::c::CNameAnnotation::KEY)){
				auto cName = var->getAnnotation(annotations::c::CNameAnnotation::KEY);
				//std::cout << "TEST " << var << " " << cName->getName() << std::endl;
				if ((cName->getName()).compare("size") == 0) {
					sizeExpr = builder.callExpr(basic.getRefDeref(), var);
					//std::cout << "TEST " << core::printer::PrettyPrinter(sizeExpr, core::printer::PrettyPrinter::OPTIONS_DETAIL);
					return true;
				}
			}
			return false;
		});


		visitDepthFirst(code2, [&](const IfStmtPtr& ifSplit) {
			auto&& matchIf = splitPoint->matchPointer(ifSplit);
			if (matchIf) {
				auto parLambda = insieme::core::transform::extractLambda(manager, ifSplit->getThenBody());
				auto range = builder.getThreadNumRange(builder.literal(basic.getInt4(), "0"), sizeExpr); // put here the expression of the variable size declaration
				JobExprPtr job = builder.jobExpr(range, vector<core::DeclarationStmtPtr>(), vector<core::GuardedExprPtr>(), parLambda);

				// joinWorkItem
				CallExprPtr newCall = builder.callExpr(runtimeExt.joinWorkItem, (builder.callExpr(builder.refType(runtimeExt.workItemType),runtimeExt.ocl_parallel, job)));
				nodeMap.insert(std::make_pair(ifSplit, newCall));
			}
		});

		NodePtr code3 = core::transform::replaceAll(manager, code2, nodeMap, true);
		std::cout << core::printer::PrettyPrinter(code3, core::printer::PrettyPrinter::OPTIONS_DETAIL);

		// Semantic check on code3
		semantic = core::check(code3, insieme::core::checks::getFullCheck());
		warnings = semantic.getWarnings();
		std::sort(warnings.begin(), warnings.end());
		for_each(warnings, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});

		errors = semantic.getErrors();
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});
		return code3;
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme
