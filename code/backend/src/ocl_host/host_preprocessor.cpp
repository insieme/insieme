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


#include "insieme/core/ir_check.h"
#include "insieme/utils/logging.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/ocl_host/host_extensions.h"
#include "insieme/backend/ocl_host/host_preprocessor.h"

#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/pattern/ir_generator.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"

using namespace insieme::transform::pattern;
using namespace insieme::core;
namespace irg = insieme::transform::pattern::generator::irg;

namespace insieme {
namespace backend {
namespace ocl_host {

using insieme::transform::pattern::any;
using insieme::transform::pattern::anyList;

	/*class BufferReplacer : public core::transform::CachedNodeMapping {

		core::NodeManager& manager;
		const Extensions& extensions;

	public:

		BufferReplacer(core::NodeManager& manager) :
			manager(manager),  extensions(manager.getLangExtension<Extensions>()) {}

		const core::NodePtr resolveElement(const core::NodePtr& ptr) {
		//LOG(INFO) << "Before Host preprocessing: " << core::printer::PrettyPrinter(ptr);
			//core::IRBuilder builder(manager);
			//auto& basic = manager.getLangBasic();
			//auto& hostExt = manager.getLangExtension<ocl_host::Extensions>();


			// perform conversion in post-order
			core::NodePtr res = ptr->substitute(manager, *this);

			// only interested in lambda expressions
			if (res->getNodeType() != core::NT_LambdaExpr) {
				return res;
			}

			//LOG(INFO) << "CODE: " << core::printer::PrettyPrinter(res);

			return res;
		}
	};*/

	core::NodePtr HostPreprocessor::process(core::NodeManager& manager, const core::NodePtr& code) {
		// the converter does the magic
		//BufferReplacer replacer(manager);
		//return replacer.map(code);

		core::IRBuilder builder(manager);
		auto& ext = manager.getLangExtension<Extensions>();

		// Extract the tuple and the access num from the kernel call
		TreePatternPtr wrapGlobal = irp::callExpr(any, irp::literal("_ocl_wrap_global"),
									irp::callExpr(any, irp::literal("tuple.member.access"),
									irp::callExpr(irp::literal("ref.deref"), var("tupleVar") << *any)
									<< var("num") << *any) << *any);

		std::vector<LiteralPtr> litVec;
		VariablePtr tupleVar;

		visitDepthFirst(code, [&](const CallExprPtr& call) {
			auto&& match = wrapGlobal->matchPointer(call);
			if (match) {
				tupleVar = static_pointer_cast<const Variable>(match->getVarBinding("tupleVar").getValue());
				litVec.push_back(static_pointer_cast<const Literal>(match->getVarBinding("num").getValue()));
			}
		});

		if(!tupleVar) return code;

		// inserts in bufVars all the variables of type array(will become buffers) getting them from the tuple
		std::vector<VariablePtr> bufVars;
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
						bufVars.push_back(static_pointer_cast<const Variable>(match->getVarBinding("bufVar").getValue()));
				}
			});
		});

		const TupleTypePtr oldTupleType = static_pointer_cast<const TupleType>(static_pointer_cast<const RefType>(tupleVar.getType())->getElementType());
		std::vector<TypePtr> oldTypeList = oldTupleType.getElementTypes();
		std::vector<TypePtr> newTypeList;
		for (uint i = 0; i < litVec.size(); ++i)
			newTypeList.push_back(ext.bufferType);
		for (uint i = litVec.size(); i < oldTypeList.size(); ++i)
			newTypeList.push_back(oldTypeList[i]);

		// create the new type for buffer and the tuple
		TypePtr bufType = ext.bufferType;
		TypePtr newTupleType = builder.tupleType(newTypeList);


		// collect the declarationStmt of buffers and map them to the new one
		utils::map::PointerMap<NodePtr, NodePtr> nodeMap;
		auto& basic = manager.getLangBasic();
		visitDepthFirst(code, [&](const DeclarationStmtPtr& decl) {
			auto&& fit = std::find_if(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){ return *vr == *(decl->getVariable());});
			if (fit != bufVars.end()) {
				CallExprPtr init = static_pointer_cast<const CallExpr>(decl->getInitialization());
				if (core::analysis::isCallOf(init->getArgument(0), builder.getLangBasic().getUndefined())) {
					DeclarationStmtPtr newDecl = builder.declarationStmt(
							builder.variable(builder.refType(bufType), decl.getVariable().getID()),
							builder.refVar(builder.callExpr(bufType, basic.getUndefined(),builder.getTypeLiteral(bufType))));
					nodeMap.insert(std::make_pair(decl, newDecl));
					return;
				}

				CallExprPtr tmp = static_pointer_cast<const CallExpr>((init->getArgument(0)));
				DeclarationStmtPtr newDecl = builder.declarationStmt(
						builder.variable(builder.refType(bufType), decl.getVariable().getID()),
						builder.refVar(builder.callExpr(ext.createBuffer, tmp->getArgument(1), tmp->getArgument(2))));
				nodeMap.insert(std::make_pair(decl, newDecl));
				return;
			}

			// collect the tuple declaration
			if (*tupleVar == *(decl->getVariable())) {
				DeclarationStmtPtr newDecl = builder.declarationStmt(
					builder.variable(builder.refType(newTupleType), decl.getVariable().getID()),
					builder.refNew(builder.callExpr(newTupleType, basic.getUndefined(),builder.getTypeLiteral(newTupleType))));
				nodeMap.insert(std::make_pair(decl, newDecl));
				return;
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

		// Update the kernel call
		TreePatternPtr kernelCall = irp::callExpr(irp::literal("call_kernel"), irp::callExpr(irp::literal("_ocl_kernel_wrapper"), *any) << var("test") << *any);

		visitDepthFirst(code, [&](const CallExprPtr& call) {
			// Match & convert the assignment operation
			if (core::analysis::isCallOf(call, builder.getLangBasic().getRefAssign())) {
				auto&& fit = std::find_if(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){ return *vr == *(call->getArgument(0));});
				if (fit != bufVars.end()) {
					CallExprPtr tmp = static_pointer_cast<const CallExpr>((call->getArgument(1)));
					VariablePtr newVar = builder.variable(builder.refType(bufType), static_pointer_cast<const Variable>(call->getArgument(0)).getID());
					CallExprPtr newCall = builder.callExpr(call->getFunctionExpr(), newVar,
									builder.callExpr(ext.createBuffer, tmp->getArgument(1), tmp->getArgument(2)));
					nodeMap.insert(std::make_pair(call, newCall));
					return;
				}
			}

			// Match insert in the tuple // FIXME: REMOVE ME
			/*auto&& matchBuf = bufVarIntuple->matchPointer(call);
			if (matchBuf) {
				if (*tupleVar == *(static_pointer_cast<const Variable>(matchBuf->getVarBinding("tuple").getValue()))) {
					// for tuple
					utils::map::PointerMap<VariablePtr, VariablePtr> varMap;
					VariablePtr newTupleVar = builder.variable(builder.refType(newTupleType), tupleVar.getID());
					varMap.insert(std::make_pair(tupleVar, newTupleVar));
					// for bufVars
					for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
						if (*vr == *(static_pointer_cast<const Variable>(matchBuf->getVarBinding("bufVar").getValue()))) {
							VariablePtr newVar = builder.variable(builder.refType(refBuf), vr.getID());
							varMap.insert(std::make_pair(vr, newVar));
						}
					});
					NodePtr newCall = core::transform::replaceVarsRecursive(manager, call, varMap, true);
					nodeMap.insert(std::make_pair(call, newCall));
					return;
				}
			}*/

			// Match insert in the tuple
			auto&& matchBuf = bufVarIntuple->matchPointer(call);
			if (matchBuf) { // FIXME: WRITE IN A BETTER WAY
				if (*tupleVar == *(static_pointer_cast<const Variable>(matchBuf->getVarBinding("tuple").getValue()))) {
					VariablePtr newTupleVar = builder.variable(builder.refType(newTupleType), tupleVar.getID());
					bool var_flag = false;
					for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
						if (*vr == *(static_pointer_cast<const Variable>(matchBuf->getVarBinding("bufVar").getValue()))) {
							var_flag = true;
							VariablePtr newVar = builder.variable(bufType, vr.getID());
							VariablePtr refNewVar = builder.variable(builder.refType(bufType), vr.getID());
							VariablePtr intVar = builder.variable(bufType);
							VariablePtr intTuple = builder.variable(builder.refType(newTupleType));
							FunctionTypePtr newFunType = builder.functionType(toVector(newTupleVar->getType(), newVar->getType()), basic.getInt4());
							// build the new body
							std::vector<core::StatementPtr> newBodyStmts;
							ExpressionPtr lit = static_pointer_cast<const ExpressionPtr>(matchBuf->getVarBinding("lit").getValue());
							CallExprPtr refCall = builder.callExpr(basic.getTupleRefElem(), intTuple, lit, builder.getTypeLiteral(bufType));
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
						VariablePtr newVar = static_pointer_cast<const Variable>(matchBuf->getVarBinding("bufVar").getValue());
						VariablePtr oldpar2 = static_pointer_cast<const Variable>(matchBuf->getVarBinding("oldpar2").getValue());
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

			auto&& matchReadWriteBuf = readWriteBuf->matchPointer(call);
			if (matchReadWriteBuf) {
				for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
					if (*vr == *(static_pointer_cast<const Variable>(matchReadWriteBuf->getVarBinding("bufVar").getValue()))) {
						 if (*(static_pointer_cast<const Variable>(matchReadWriteBuf->getVarBinding("1var").getValue())) ==
							 *(static_pointer_cast<const Variable>(matchReadWriteBuf->getVarBinding("2var").getValue()))) {
							ExpressionPtr newVar = builder.variable(builder.refType(bufType), vr.getID());
							CallExprPtr newCall = builder.callExpr(ext.writeBuffer, toVector(newVar, call->getArgument(1), call->getArgument(2), call->getArgument(3), call->getArgument(4)));
							nodeMap.insert(std::make_pair(call, newCall));
							return;
						} else if (*(static_pointer_cast<const Variable>(matchReadWriteBuf->getVarBinding("1var").getValue())) ==
								*(static_pointer_cast<const Variable>(matchReadWriteBuf->getVarBinding("3var").getValue()))) {
							ExpressionPtr newVar = builder.variable(builder.refType(bufType), vr.getID());
							CallExprPtr newCall = builder.callExpr(ext.readBuffer, toVector(newVar, call->getArgument(1), call->getArgument(2), call->getArgument(3), call->getArgument(4)));
							nodeMap.insert(std::make_pair(call, newCall));
							return;
						}
					}
				});
			}

			auto&& matchDel = delTree->matchPointer(call);
			if (matchDel) {
				for_each(bufVars.begin(), bufVars.end(), [&](const VariablePtr& vr){
					VariablePtr tmpVar = static_pointer_cast<const Variable>(matchDel->getVarBinding("bufVar").getValue());
					if (*vr == *tmpVar) {
						VariablePtr newVar = builder.variable(builder.refType(bufType), vr.getID());
						CallExprPtr newCall = builder.callExpr(ext.releaseBuffer, newVar);
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
					if (*tupleVar == *tmpVar) {
						VariablePtr newTupleVar = builder.variable(builder.refType(newTupleType), tupleVar.getID());
						CallExprPtr newCall = builder.callExpr(basic.getRefDelete(), newTupleVar);
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
				});
			}

			/*auto&& matchKernel = kernelCall->matchPointer(call);
			if (matchKernel) {
				std::cout << call << std::endl;
				std::cout << matchKernel->getVarBinding("test").getValue() << std::endl;
			}*/
		});


		NodePtr code2 = core::transform::replaceAll(manager, code, nodeMap, true);

		//NodePtr code2 = core::transform::replaceVarsRecursiveGen(manager, code3, map, false);

		//---------- check if code2 is correct
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
		//-----------

		//TreePatternPtr pattern2 = irp::declarationStmt(irp::variable(at("int<4>"), any), at("3"));

		std::cout << core::printer::PrettyPrinter(code2);
		return code;
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme
