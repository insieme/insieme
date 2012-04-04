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

// TODO: BRING to the runtime informations: kernel is splittable or not.
#include <fstream>

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
#include "insieme/annotations/data_annotations.h"
#include "insieme/backend/ocl_kernel/kernel_poly.h"

using namespace insieme::transform::pattern;
using namespace insieme::core;
namespace irg = insieme::transform::pattern::generator::irg;

namespace insieme {
namespace backend {
namespace ocl_host {

using insieme::transform::pattern::any;
using insieme::transform::pattern::anyList;

	/*
	 * This struct hold information of how much data has to be copied from/to the devices to run it
	 */
	struct DataToTransfer {
		ExpressionPtr splittalbeToDevice;
		ExpressionPtr nonSplittableToDevice;
		ExpressionPtr splittableFromDevice;
		ExpressionPtr nonSplittableFromDevice;
	};

	/*
	 * Builds a ref.deref call around an expression if the it is of ref-type
	 */
	core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr, const core::IRBuilder& builder) {
		// core::ExpressionPtr retExpr = expr;
		if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
			return builder.callExpr(refTy->getElementType(), builder.getLangBasic().getRefDeref(), expr);
		}
		return expr;
	}


	void printVarAddressMap(utils::map::PointerMap<VariableAddress, VariableAddress> varMap){
		for_each(varMap, [&](std::pair<VariableAddress, VariableAddress> vp){
				 std::cout << *vp.first << " = " << *vp.second << "; ";
		});
		std::cout << "\n";
	}

	void printVarVector(std::vector<VariableAddress> varVec){
		for_each(varVec, [&](VariableAddress va){
				 std::cout << *va << " ";
		});
		std::cout << "\n";
	}

	core::VariablePtr getVar(MatchOpt& match, const std::string& str){
		return static_pointer_cast<const Variable>(match->getVarBinding(str).getValue());
	}

	ExpressionPtr replaceGetId(ExpressionPtr expr, ExpressionPtr replacement) {
		// TODO: Handle multi-dimensional cases, handle get local & group id
		TreePatternPtr getId = aT(var("id_call", irp::castExpr(any, irp::callExpr(irp::literal("get_global_id"), *any))));

		auto&& match = getId->matchPointer(expr);
		if (match){
			return core::transform::replaceAll(expr->getNodeManager(), expr, match->getVarBinding("id_call").getValue(), replacement).as<ExpressionPtr>();
		}
		return expr;
	}

	core::VariablePtr getVariableArg(const core::ExpressionPtr& function, const core::IRBuilder& builder) {
		if(const core::CallExprPtr& call = dynamic_pointer_cast<const core::CallExpr>(function))
			return getVariableArg(call->getArgument(0), builder);
		return dynamic_pointer_cast<const core::Variable>(function);
	}

	class RangeExpressionApplier : public core::IRVisitor<void>{
	private:
		std::vector<annotations::Range>& ranges;
		utils::map::PointerMap<NodePtr, NodePtr> nodeMap;
		IRBuilder builder;
		VariablePtr begin;
		VariablePtr end;
		VariablePtr step;
		VariablePtr unsplittedSize;
		VariablePtr originalSize;
		DataToTransfer dataToTransfer;

		TreePatternPtr sizeOfPattern;

		/*
		 * adds an expression to another expression
		 */
		void addToExpression(ExpressionPtr& base, ExpressionPtr extension) {
			if(base) // there is already something in base, add extension
				base = builder.add(base, extension);
			else // base is empty, replace with extension
				base = extension;
		}

		/*
		 * adds the size of a host/device datatransfer to the appropriate field in the DataToTransfer struct
		 */
		void countDataToTransfer(ExpressionPtr sizeExpr, bool toDevice, bool isSplittable) {
			// replace the variable with a literal with the name of the size argument in the Ruby script
			ExpressionPtr sizeExpr2 = core::transform::replaceAll(builder.getNodeManager(), sizeExpr, builder.deref(originalSize),
					builder.literal("size", builder.getNodeManager().getLangBasic().getString())).as<ExpressionPtr>();
			sizeExpr2 = core::transform::replaceAll(builder.getNodeManager(), sizeExpr2, originalSize,
					builder.literal("size", builder.getNodeManager().getLangBasic().getString())).as<ExpressionPtr>();

			if(toDevice) {
				if(isSplittable)
					addToExpression(dataToTransfer.splittalbeToDevice, sizeExpr2);
				else
					addToExpression(dataToTransfer.nonSplittableToDevice, sizeExpr2);
			} else {
				if(isSplittable)
					addToExpression(dataToTransfer.splittableFromDevice, sizeExpr2);
				else
					addToExpression(dataToTransfer.nonSplittableFromDevice, sizeExpr2);
			}
		}

		ExpressionPtr getRWFlag(insieme::ACCESS_TYPE access) {
			LiteralPtr shift;
			switch(access){
				case ACCESS_TYPE::readWrite: shift = builder.intLit(0); break;
				case ACCESS_TYPE::write: shift = builder.intLit(1); break;
				case ACCESS_TYPE::read: shift = builder.intLit(2); break;
				default: shift = builder.intLit(0);
			}

			return builder.castExpr(builder.getNodeManager().getLangBasic().getUInt8(),
				builder.callExpr(builder.getNodeManager().getLangBasic().getSignedIntLShift(), builder.intLit(1), shift));
		}

		ExpressionPtr getElemsCreateBuf(annotations::Range range, ExpressionPtr expr, const insieme::core::lang::BasicGenerator& basic, CallExprPtr& sizeOfCall){
			auto&& match = sizeOfPattern->matchPointer(expr);
			if (match)
				sizeOfCall = match->getVarBinding("sizeof").getValue().as<CallExprPtr>();
			else
				assert(false && "Sizeof not present :(");

			return tryDeref(unsplittedSize.as<ExpressionPtr>(), builder);
		}

		ExpressionPtr getElems(annotations::Range range, ExpressionPtr expr, const insieme::core::lang::BasicGenerator& basic, CallExprPtr& sizeOfCall){
			auto&& match = sizeOfPattern->matchPointer(expr);
			if (match)
				sizeOfCall = match->getVarBinding("sizeof").getValue().as<CallExprPtr>();
			else
				assert(false && "Sizeof not present :(");

			return range.isSplittable() ?
						builder.callExpr(basic.getUnsignedIntSub(), range.getUpperBoundary(), range.getLowerBoundary()).as<ExpressionPtr>() :
						tryDeref(unsplittedSize.as<ExpressionPtr>(), builder);
		}

		ExpressionPtr updateCreateBuffer(VariablePtr buffer, ExpressionPtr create) {
			auto& ext = builder.getNodeManager().getLangExtension<Extensions>();
			auto& basic = builder.getNodeManager().getLangBasic();
			ExpressionPtr ret;
			if(CallExprPtr call = dynamic_pointer_cast<const CallExpr>(create)) {
				if (LiteralPtr fun = dynamic_pointer_cast<const Literal>(call->getFunctionExpr())) {
					if (*fun == *ext.createBuffer){
						for_each(ranges, [&](annotations::Range range){
							 if(*buffer == *range.getVariable()) {
								CallExprPtr sizeOfCall;
								ExpressionPtr shift = getRWFlag(range.getAccessType());
								ExpressionPtr nElems = getElemsCreateBuf(range, call->getArgument(1), basic, sizeOfCall);

								ExpressionPtr size = builder.callExpr(basic.getUnsignedIntMul(), sizeOfCall, builder.castExpr(basic.getUInt8(), nElems));
								ret = builder.callExpr(call->getFunctionExpr(), shift, size);
								return;
							}

						});
					}
				}
			}
			return ret;
		}

		void visitDeclarationStmt(const DeclarationStmtPtr& decl) {
			NodeManager& mgr = decl->getNodeManager();aT(var("sizeof", irp::callExpr(irp::literal("sizeof"), *any)));
			if(CallExprPtr call = dynamic_pointer_cast<const CallExpr>(decl->getInitialization())) {
				if(mgr.getLangBasic().isRefNew(call->getFunctionExpr()) || mgr.getLangBasic().isRefVar(call->getFunctionExpr())) {
					VariablePtr var = decl->getVariable();
					ExpressionPtr initExpr = call->getArgument(0);
					ExpressionPtr replacement = updateCreateBuffer(var, initExpr);
					if(replacement)
						nodeMap[decl] = builder.declarationStmt(var, builder.callExpr(call->getFunctionExpr(), replacement));
				}
			}
		}


		void visitCallExpr(const CallExprPtr& call) {
			NodeManager& manager = call->getNodeManager();
			if(manager.getLangBasic().isRefAssign(call->getFunctionExpr())){
				if(VariablePtr var = dynamic_pointer_cast<const Variable>(call->getArgument(0))) {
					ExpressionPtr replacement = updateCreateBuffer(var, call->getArgument(1));
					if(replacement)
						nodeMap[call] = builder.assign(call->getArgument(0), replacement);
				}
			}
			auto& ext = manager.getLangExtension<Extensions>();

			if(*call->getFunctionExpr() == *ext.writeBuffer || *call->getFunctionExpr() == *ext.readBuffer) {
				auto& basic = builder.getNodeManager().getLangBasic();
				for_each(ranges, [&](annotations::Range range){
					if(*getVariableArg(call->getArgument(0), builder) == *range.getVariable()) {
						CallExprPtr sizeOfCall;
						ExpressionPtr nElems = getElems(range, call->getArgument(3), basic, sizeOfCall);
						ExpressionPtr subScriptValue = range.isSplittable() ? range.getLowerBoundary() : builder.uintLit(0).as<ExpressionPtr>();
						ExpressionPtr size = builder.callExpr(basic.getUnsignedIntMul(), sizeOfCall, builder.castExpr(basic.getUInt8(), nElems));
						ExpressionPtr offset = builder.callExpr(basic.getUnsignedIntMul(), sizeOfCall, builder.castExpr(basic.getUInt8(), range.getLowerBoundary()));
						ExpressionPtr pos = builder.callExpr(basic.getRefToAnyRef(), builder.callExpr(basic.getScalarToArray(),
												builder.callExpr(basic.getArrayRefElem1D(),builder.callExpr(basic.getRefDeref(),
												getVariableArg(call->getArgument(4), builder)), builder.castExpr(basic.getUInt4(), subScriptValue))));
						ExpressionList args;
						args.push_back(call->getArgument(0));
						args.push_back(call->getArgument(1));
						args.push_back(offset);
						args.push_back(size);
						args.push_back(pos);

						nodeMap[call] = builder.callExpr(call->getFunctionExpr(), args);

						countDataToTransfer(call->getArgument(3), *call->getFunctionExpr() == *ext.writeBuffer, range.isSplittable());
					}
				});

			}
		}


	public:
		RangeExpressionApplier(std::vector<annotations::Range>& ranges, VariablePtr begin, VariablePtr end, VariablePtr step, VariablePtr unsplittedSize,
				VariablePtr originalSize, IRBuilder& build)
			: IRVisitor<void>(false), ranges(ranges), builder(build), begin(begin), end(end), step(step), unsplittedSize(unsplittedSize),
			  originalSize(originalSize) {
			sizeOfPattern = aT(var("sizeof", irp::callExpr(irp::literal("sizeof"), *any)));
		}

		utils::map::PointerMap<NodePtr, NodePtr>& getNodeMap() { return nodeMap; }

		void printMap() {
			std::cout << "RangeExpressionApplier nodeMap:\n";
			for_each(nodeMap, [](std::pair<NodePtr, NodePtr> nodes){
				std::cout << nodes.first << " -> \n\t" << nodes.second << std::endl;
			});
		}

		/*
		 * writes the DataToTransfer struct to a file as a string
		 */
		void dumpDataToTransfer(std::string filename) {
/*			std::cerr << "splittalbeToDevice " << printer::PrettyPrinter(dataToTransfer.splittalbeToDevice)
				<< "\nnonSplittableToDevice " << printer::PrettyPrinter(dataToTransfer.nonSplittableToDevice)
				<< "\nsplittableFromDevice " << printer::PrettyPrinter(dataToTransfer.splittableFromDevice)
				<< "\nnonSplittableFromDevice " << printer::PrettyPrinter(dataToTransfer.nonSplittableFromDevice)
				<< std::endl;
*/
			std::ofstream os(filename);
			assert(os.is_open() && "Could not open file to write data to transfer");
			if(dataToTransfer.splittalbeToDevice )
				os << printer::PrettyPrinter(dataToTransfer.splittalbeToDevice) << std::endl;
			else
				os << 0 << std::endl;
			if(dataToTransfer.nonSplittableToDevice)
				os << printer::PrettyPrinter(dataToTransfer.nonSplittableToDevice) << std::endl;
			else
				os << 0 << std::endl;
			if(dataToTransfer.splittableFromDevice)
				os << printer::PrettyPrinter(dataToTransfer.splittableFromDevice) << std::endl;
			else
				os << 0 << std::endl;
			if(dataToTransfer.nonSplittableFromDevice)
				os << printer::PrettyPrinter(dataToTransfer.nonSplittableFromDevice) << std::endl;
			else
				os << 0 << std::endl;

			os.close();
		}
	};

	class sizeCorrelationVisitor: public core::IRVisitor<void, Address> {
		VariablePtr sizeVar;
		std::vector<VariablePtr> roots;
		std::vector<VariableAddress> toReplace;
		NodeManager& manager;

		void checkValuesFlow (ExpressionAddress source, ExpressionAddress sink){
			if (findInRoots(dynamic_address_cast<const Variable>(extractVariable(sink)))) {
				if(VariableAddress tmp = dynamic_address_cast<const Variable>(extractVariable(source))) {
					if(*tmp == *sizeVar)
						toReplace.push_back(tmp);
					else
						roots.push_back(tmp);
				}
			}

		}

		void visitCallExpr(const CallExprAddress& call) {
			if(LambdaExprAddress lambda = dynamic_address_cast<const LambdaExpr>(call->getFunctionExpr())) {
				std::vector<ExpressionAddress> vec = call->getArguments();
				std::vector<VariableAddress> vec2 = lambda->getLambda()->getParameters()->getElements();


				for_range(make_paired_range(call->getArguments(), lambda->getLambda()->getParameters()->getElements()),
						[&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
						  checkValuesFlow(pair.first, pair.second);
				});
			}

			if(manager.getLangBasic().isRefAssign(call->getFunctionExpr())) {
				checkValuesFlow(call->getArgument(0), call->getArgument(1));
			}
		}

		void visitForStmt(const ForStmtAddress& loop){
			VariableAddress inductionVar = loop->getIterator();

			ExpressionAddress inductionBegin = loop->getStart();
			checkValuesFlow(inductionBegin, inductionVar);

			ExpressionAddress inductionEnd = loop->getEnd();
			checkValuesFlow(inductionEnd, inductionVar);
		}

		ExpressionAddress extractVariable(ExpressionAddress exp){
			if(VariableAddress var = dynamic_address_cast<const Variable>(exp))
				return var;

			if(CastExprAddress cast = dynamic_address_cast<const CastExpr>(exp))
				return extractVariable(cast->getSubExpression());

			if(CallExprAddress call = dynamic_address_cast<const CallExpr>(exp)){
				NodeManager& manager = exp->getNodeManager();
				if (manager.getLangBasic().isRefDeref(call->getFunctionExpr())){
					return extractVariable(call->getArgument(0));
				}

			}

			return exp;
		}

		bool findInRoots(VariableAddress var) {
			if(!var)
				return false;
			for(auto I = roots.begin(); I != roots.end(); I++)
				if(**I == *var) return true;
			return false;
		}

	public:

		std::vector<VariableAddress>& getToReplaceVec(){
			return toReplace;
		}

		sizeCorrelationVisitor(const VariablePtr& root, const VariablePtr sizeVar): IRVisitor<void, Address>(false), sizeVar(sizeVar), manager(sizeVar->getNodeManager()) {
			roots.push_back(root);
		}
	};







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
		TreePatternPtr kernelCall = irp::callExpr(irp::literal("call_kernel"), var("okw", irp::callExpr(irp::literal("_ocl_kernel_wrapper"),
										single(var("kernLambda")))) << *any << var("offset") << any << any << var("varlist"));

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

			if (tupleVar) { // modify insert in the callExprtuple
				auto&& matchBuf = bufVarIntuple->matchPointer(call);
				if (matchBuf) { // TODO: WRITE IN A BETTER WAY
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
						//std::cout <<"nedeed value "<< call->getArgument(4) << std::endl;
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
		std::cout << "ERROR: " << std::endl;
		for_each(errors, [](const core::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});

		std::cout << "CODE 2" << std::endl;
		std::cout << core::printer::PrettyPrinter(code2, core::printer::PrettyPrinter::OPTIONS_DETAIL);

		// add kernel data range annotation
		insieme::backend::ocl_kernel::KernelPoly polyAnalyzer(code2);

		auto at = [&manager](string str) { return irp::atom(manager, str); };
		TreePatternPtr splitPoint = irp::ifStmt(at("(lit<uint<4>, 1> != CAST<uint<4>>(0))"), any, any);

		visitDepthFirst(code2, [&](const IfStmtPtr& ifSplit) {
			auto&& matchIf = splitPoint->matchPointer(ifSplit);
			if (matchIf) {
				CallExprPtr call = insieme::core::transform::outline(manager, ifSplit->getThenBody()); // create the new isolated call

				std::cout << "=== NEW CALL AFTER OUTLINE ===\n" << printer::PrettyPrinter(call);

				CallExprPtr varlist;
				visitDepthFirst(call, [&](const CallExprPtr& cl) {
					auto&& matchKernel = kernelCall->matchPointer(cl);
					if (matchKernel) varlist = static_pointer_cast<const CallExpr>(matchKernel->getVarBinding("varlist").getValue());
				});

				LambdaExprPtr oldLambda = static_pointer_cast<const LambdaExpr>(call->getFunctionExpr());

				// define begin, end, step variable
				VariablePtr begin = builder.variable(basic.getInt4());
				VariablePtr end = builder.variable(basic.getInt4());
				VariablePtr step = builder.variable(basic.getInt4());

				VariablePtr endMinusOne = builder.variable(basic.getInt4());

				VariablePtr beginArg = builder.variable(basic.getInt4());
				VariablePtr endArg = builder.variable(basic.getInt4());
				VariablePtr stepArg = builder.variable(basic.getInt4());

				// check for kernel annotation
				CallExprPtr kernel;
				LambdaExprPtr kernLambda;
				annotations::DataRangeAnnotationPtr dataRangeAn;
				visitDepthFirstOnceInterruptible(call, [&](const CallExprPtr& kernelCandidate)->bool {
					auto&& matchKernel = kernelCall->matchPointer(kernelCandidate);
					if (matchKernel) {
						kernLambda = static_pointer_cast<const LambdaExpr>(matchKernel->getVarBinding("kernLambda").getValue());
						if (kernLambda->hasAnnotation(annotations::DataRangeAnnotation::KEY)){
							dataRangeAn = kernLambda->getAnnotation(annotations::DataRangeAnnotation::KEY);
							return true;
						}
					}
					return false;
				});

				utils::map::PointerMap<VariablePtr, int> boundaryVars;
				// visitor to find variables
				auto getAllVars = makeLambdaVisitor([&](const VariablePtr& var) {
						boundaryVars[var] = 1;
				});

				ExpressionPtr callBody = call->getFunctionExpr();

				// visitor to collect 1) all the buffers in the range annotation 2) all the variables in the range boundary expressions
				std::vector<VariableAddress> buffers;
				for_each(dataRangeAn->getRanges(), [&](annotations::Range range) {
						buffers.push_back(core::Address<const core::Variable>::find(range.getVariable(), callBody));
						visitDepthFirst(range.getLowerBoundary(), getAllVars);
						visitDepthFirst(range.getUpperBoundary(), getAllVars);
				});

				std::cout << "=== ANNOTATION RANGES BOUNDARY VARS ===\n" << boundaryVars << std::endl;
				std::cout << "=== BUFFERS VARS ===\n";
				for_each(buffers,[](VariableAddress bufAdd) { std::cout << *bufAdd << " ";}); std::cout << "\n";

				utils::map::PointerMap<VariableAddress, VariableAddress> bufferMap;
				// this map is used to replace the variables (not buffer) in the boundary
				utils::map::PointerMap<VariableAddress, VariableAddress> boundaryVarMap;
				// extract the arguments from the kernel and mapping then to the variables in the host code
				TupleExprPtr tuple = static_pointer_cast<const TupleExpr>(varlist->getArgument(0));
				for_range(make_paired_range(tuple->getExpressions(), kernLambda->getLambda()->getParameters()->getElements()),
						[&](const std::pair<const core::ExpressionPtr, const core::VariablePtr>& pair) {
						//std::cout << "First " << getVariableArg(pair.first, builder) << " " << *pair.second << std::endl;
						for_each(buffers,[&](VariableAddress bufAdd) {
						//std::cout << *bufAdd << " == " << *pair.second << std::endl;
							if (*bufAdd == *pair.second) {
								CallExprPtr deref = static_pointer_cast<const CallExpr>(static_pointer_cast<const CallExpr>(pair.first)->getArgument(0));
								VariablePtr varPtr = static_pointer_cast<const Variable>(deref->getArgument(0));
								bufferMap[bufAdd] = core::Address<const core::Variable>::find(varPtr, callBody);
							}
						});
						if(boundaryVars.find(pair.second) != boundaryVars.end()) {
							VariablePtr varPtr = static_pointer_cast<const Variable>(static_pointer_cast<const CallExpr>(pair.first)->getArgument(0));
							boundaryVarMap[core::Address<const core::Variable>::find(pair.second, callBody)] =
									core::Address<const core::Variable>::find(varPtr, callBody);
						}
				});

				std::cout << "=== ANNOTATION RANGES BOUNDARY MAP ===\n"; printVarAddressMap(boundaryVarMap);
				std::cout << "=== BUFFER VAR MAP ===\n"; printVarAddressMap(bufferMap);

				core::analysis::getRenamedVariableMap(bufferMap);
				core::analysis::getRenamedVariableMap(boundaryVarMap);

				std::cout << "=== ANNOTATION RANGES BOUNDARY MAP AFTER RENAMING ===\n"; printVarAddressMap(boundaryVarMap);
				std::cout << "=== BUFFER VAR MAP AFTER RENAMING===\n"; printVarAddressMap(bufferMap);

				// search in the arguments for the size and remove it
				utils::map::PointerMap<VariableAddress, VariableAddress> renamedArgsMap;
				std::vector<VariableAddress> argAddress;
				for_each(call->getArguments(), [&](ExpressionPtr exp) {
						 argAddress.push_back(core::Address<const core::Variable>::find(exp.as<VariablePtr>(), call));
				});

				renamedArgsMap = core::analysis::getRenamedVariableMap(argAddress);

				std::cout << "=== ARGUMENTS ===\n"; printVarVector(argAddress);
				std::cout << "=== ARGUMENST MAP AFTER RENAMING===\n"; printVarAddressMap(renamedArgsMap);


				VariablePtr sizeVar;
				for_each(renamedArgsMap, [&](std::pair<VariableAddress, VariableAddress> variablePair){
						VariablePtr var = variablePair.second.as<VariablePtr>();
						if(var->hasAnnotation(annotations::c::CNameAnnotation::KEY)){
							 auto cName = var->getAnnotation(annotations::c::CNameAnnotation::KEY);
							 if ((cName->getName()).compare("size") == 0){
								 sizeVar = variablePair.first.as<VariablePtr>();
							}
						}
				});
				// create a new variable for the size to avoid it from beeing replaced, will only be used if required in some range annotation
				VariablePtr firstSizeVar, newSize, unsplittedSizeVar;
				bool unsplittedSizeNeededInsideKernel = false;
				firstSizeVar = sizeVar; // the variable size in the outer most part of the program

				std::cout << "=== OUTER MOST SIZE VAR ===\n" << firstSizeVar << std::endl;


				// add all the paramaters that are different from the size one
				std::vector<VariablePtr> parameters;
				std::vector<ExpressionPtr> arguments;
				for_range(make_paired_range(oldLambda->getLambda()->getParameters()->getElements(), call->getArguments()),
						[&](const std::pair<const core::VariablePtr, const core::ExpressionPtr> pair) {
						  std::cout << "Pair second " << pair.second << " " << sizeVar << std::endl;
						if (*pair.second != *sizeVar){
							parameters.push_back(pair.first);
							arguments.push_back(pair.second);
						} else {
							sizeVar = pair.first; // update the sizeVar with the parameters value of the new wi isolated function
							// alwasys keep the unsplitted size. Otherwise everything is wheired and complicated. Use a fresh variable for it
							newSize = builder.variable(pair.first->getType());
							parameters.push_back(newSize);
							arguments.push_back(pair.second);
							for_each(boundaryVarMap, [&](std::pair<VariableAddress, VariableAddress> v){
								// mark if the unsplited size is needed inside the kernel
								 if(!unsplittedSizeNeededInsideKernel && (*sizeVar == *v.second.as<VariablePtr>())){
									unsplittedSizeNeededInsideKernel = true;
								 }
							});
						}
				});

				std::cout << "=== NEW ISOLATED FUN ARGUMENT SIZE VAR ===\n" << sizeVar << std::endl;
				std::cout << "=== FRESH ISOLATED FUN SIZE VAR ===\n" << newSize << std::endl;

				VariablePtr kernelSizeParameter; // this variable represents the splitted size parameter for the kernel

				for_range(make_paired_range(tuple->getExpressions(), kernLambda->getLambda()->getParameters()->getElements()),
						[&](const std::pair<const core::ExpressionPtr, const core::VariablePtr>& pair) {
						// recognize the size from the arguments of the kernel call & match to the one in the parameters
						if(*getVariableArg(pair.first, builder) == *sizeVar)
							kernelSizeParameter = pair.second;
				});

				std::cout << "=== KERNEL PARAMETER SIZE VAR ===\n" << kernelSizeParameter << std::endl;

				/*fun(ref<int<4>> v105, ...){ // sizeVar -> in the new function that we are building we replace it with "newSize"
					....
					call_kernel(_ocl_kernel_wrapper(fun(..., int<4> v48) { // kernelSizeParameter
					...
					}..., varlist.pack((..., v105)));
					...
				}(v3,...) // firstSizeVar
				*/

				// add the begin, end, step parameters & arguments
				parameters.insert(parameters.begin(), step);
				parameters.insert(parameters.begin(), end);
				parameters.insert(parameters.begin(), begin);

				arguments.insert(arguments.begin(), stepArg);
				arguments.insert(arguments.begin(), endArg);
				arguments.insert(arguments.begin(), beginArg);


				utils::map::PointerMap<NodePtr, NodePtr> boundaryVarPtrMap;
				for_each(boundaryVarMap, [&](std::pair<VariableAddress, VariableAddress> variablePair){
					ExpressionPtr oldVar = variablePair.first.as<VariablePtr>();
					ExpressionPtr newVar = variablePair.second.as<VariablePtr>();

					// trick: we replace the real size variable with another one created before
					// so will be easier to then raplace the size in only some situations
					if(*newVar == *sizeVar) newVar = newSize;

					if(oldVar->getType()->getNodeType() == core::NT_RefType && !(newVar->getType()->getNodeType() == core::NT_RefType))
						newVar = builder.callExpr(basic.getRefVar(), newVar);
					if(!(oldVar->getType()->getNodeType() == core::NT_RefType) && (newVar->getType()->getNodeType() == core::NT_RefType))
						newVar = builder.callExpr(basic.getRefDeref(), newVar);
					if(oldVar->getType() != newVar->getType())
						newVar = builder.castExpr(oldVar->getType(), newVar);

					boundaryVarPtrMap[oldVar] = newVar;
				});

				// replace in the rangeAnnotation the kernel arguments with the buffer variable used in the host
				size_t i = 0;
				bool kernelIsSplittable = true;
				std::vector<annotations::Range>updatedRanges;
				std::cout << "=== RANGES ===\n";
				for_each(dataRangeAn->getRanges(), [&](annotations::Range range) {
					std::cout << range << std::endl;

					ExpressionPtr low = core::transform::replaceAll(manager, replaceGetId(range.getLowerBoundary(), begin), boundaryVarPtrMap).as<ExpressionPtr>();
					ExpressionPtr up = core::transform::replaceAll(manager, replaceGetId(range.getUpperBoundary(), endMinusOne), boundaryVarPtrMap).as<ExpressionPtr>();

					updatedRanges.push_back(annotations::Range(bufferMap[buffers.at(i)].as<VariablePtr>(), low, up, range.getAccessType(), range.isSplittable()));
					++i;

					// check if the entire kernel is splittable
					if(!range.isSplittable() && range.getAccessType() != insieme::ACCESS_TYPE::read)
						kernelIsSplittable = false;
				});

				std::cout << "=== UPDATED RANGES ===\n";
				for_each(updatedRanges, [](annotations::Range range) {
					std::cout << range << std::endl;
				});

				if(kernelIsSplittable)
					LOG(INFO) << "======== KERNEL IS SPLITTABLE :) ===========" << std::endl;
				else
					LOG(INFO) << "======== KERNEL IS NOT SPLITTABLE :°°°°°° ===========" << std::endl;


				CompoundStmtPtr oldBody = oldLambda->getBody();

				RangeExpressionApplier rea(updatedRanges, begin, endMinusOne, step, newSize, sizeVar, builder);
				visitDepthFirstOnce(oldBody, rea);
				rea.dumpDataToTransfer("dataToTransfer.txt");

				CompoundStmtPtr newBody = core::transform::replaceAll(manager, oldBody, rea.getNodeMap()).as<CompoundStmtPtr>();

				// replace the remaining size variable with end - begin
				CompoundStmtPtr newBody2 = core::transform::replaceAll(manager, newBody,
											builder.callExpr(basic.getRefDeref(),sizeVar), builder.callExpr(basic.getSignedIntSub(), end, begin)).as<CompoundStmtPtr>();

				visitDepthFirst(newBody2, [&](const CallExprPtr& cl) {
					auto&& matchKernel = kernelCall->matchPointer(cl);
					if (matchKernel) {
						// replace the previous inserted end-begin with the correct end because of the offset
						CallExprPtr varlist = static_pointer_cast<const CallExpr>(matchKernel->getVarBinding("varlist").getValue());
						CallExprPtr varlistNew = core::transform::replaceAll(manager, varlist, builder.callExpr(basic.getSignedIntSub(), end, begin), end).as<CallExprPtr>();
						newBody2 = core::transform::replaceAll(manager, newBody2, varlist, varlistNew).as<CompoundStmtPtr>();

						// replace the offset value {0, 0, 0} with {begin, 0, 0}
						ExpressionPtr zero = builder.literal(manager.getLangBasic().getUInt8(), "0");
						ExpressionPtr offsetNew = builder.refVar(builder.vectorExpr(toVector(builder.castExpr(zero->getType(), begin).as<ExpressionPtr>() , zero, zero)));
						ExpressionPtr offset = static_pointer_cast<const CallExpr>(matchKernel->getVarBinding("offset").getValue());
						newBody2 = core::transform::replaceAll(manager, newBody2, offset, offsetNew).as<CompoundStmtPtr>();
					}
				});

				if (unsplittedSizeNeededInsideKernel && kernelIsSplittable) {
					CallExprPtr varlist, kernelWrapper;
					LambdaExprPtr kernLambda;
					if(visitDepthFirstInterruptible(newBody2, [&](const CallExprPtr& cl)->bool {
						auto&& matchKernel = kernelCall->matchPointer(cl);
						if (matchKernel) {
							varlist = matchKernel->getVarBinding("varlist").getValue().as<CallExprPtr>();
							kernLambda = matchKernel->getVarBinding("kernLambda").getValue().as<LambdaExprPtr>();
							kernelWrapper = matchKernel->getVarBinding("okw").getValue().as<CallExprPtr>();
							return true;
						}
						return false;
					})) {
						// generate new varlistPack, adding the unchanged size
						TupleExprPtr tuple = varlist->getArgument(0).as<TupleExprPtr>();

						ExpressionList tupleElems = tuple->getExpressions()->getExpressions();
						tupleElems.push_back(tryDeref(newSize, builder));
						TypePtr unsplittedSizeType = tupleElems.back()->getType();
						unsplittedSizeVar = builder.variable(unsplittedSizeType);

						TupleExprPtr newTuple = builder.tupleExpr(tupleElems);

						CallExprPtr newVarlist = builder.callExpr(basic.getVarlistPack(), newTuple);

						// generate a new kernel call, adding the unchanged size
						VariableList params = kernLambda->getLambda()->getParameters()->getElements();
						params.push_back(unsplittedSizeVar);

						CompoundStmtPtr kernBody = kernLambda->getBody();

						//std::cout << "Kernel Body " << kernBody << std::endl;

						VariableList unsplittableBuffer; // we are inside kernel we have to use dataRangeAn->getRanges() to get kernel variables
						for_each(dataRangeAn->getRanges(), [&](annotations::Range range) {
							if(!range.isSplittable()) unsplittableBuffer.push_back(range.getVariable());
						});

						std::map<NodeAddress, NodePtr> toReplace;
						visitDepthFirst(kernBody, [&](const CallExprPtr& call) {
							if (basic.isSubscriptOperator(call->getFunctionExpr())) {
								utils::map::PointerMap<VariablePtr, int> accessVars;
								for_each(unsplittableBuffer, [&](VariablePtr buf) {
									if (*buf == *getVariableArg(call->getArgument(0), builder)){
										visitDepthFirst(call->getArgument(1), [&](const VariablePtr& var) {
											accessVars[var] = 1;
										});
									}
								});
								//std::cout << " VARS: " << accessVars << std::endl;

								for_each(accessVars, [&](std::pair<VariablePtr, int> accessPtr) {
									VariableAddress accessAddr = core::Address<const core::Variable>::find(accessPtr.first, kernBody);
									sizeCorrelationVisitor scv(accessAddr, kernelSizeParameter);
									visitPathBottomUp(accessAddr, scv);
									std::vector<VariableAddress> tmp = scv.getToReplaceVec();
									for_each(tmp, [&](VariableAddress tmpAddr) {
											 toReplace[tmpAddr] = unsplittedSizeVar;
									});
								});
							}
						});


						//std::cout << "TOREPLACE: " << toReplace << std::endl;


						CompoundStmtPtr newKernBody = core::transform::replaceAll(manager, toReplace).as<CompoundStmtPtr>();

						LambdaExprPtr newKernLambda = builder.lambdaExpr(kernLambda->getFunctionType()->getReturnType(), newKernBody, params);

						// create new ocl_kernel_wrapper
						FunctionTypePtr okwType = static_pointer_cast<const FunctionType>(kernelWrapper->getType());
						TypeList okwParamTypes = okwType->getParameterTypes()->getElements();
						okwParamTypes.push_back(unsplittedSizeType);

						CallExprPtr newKernelWrapper = builder.callExpr(kernelExt.kernelWrapper, newKernLambda);

						// replace the kernel arguments and parameters with the extended ones
						utils::map::PointerMap<NodePtr, NodePtr>replacements;
						replacements[varlist] = newVarlist;
						replacements[kernelWrapper] = newKernelWrapper;

						newBody2 = core::transform::replaceAll(manager, newBody2, replacements).as<CompoundStmtPtr>();
					}
				}

				// decrement the end variable to make it work with the runtime not included last value
				newBody2 = builder.compoundStmt(toVector<StatementPtr>(builder.declarationStmt(endMinusOne, builder.sub(end, builder.intLit(1))), newBody2));

				// building the new Call
				LambdaExprPtr newLambda = builder.lambdaExpr(call->getType(), newBody2, parameters);
				CallExprPtr newCall = builder.callExpr(newLambda, arguments);

				// Semantic check on newCall
				auto semantic = core::check(newCall, insieme::core::checks::getFullCheck());
				auto warnings = semantic.getWarnings();
				std::sort(warnings.begin(), warnings.end());
				for_each(warnings, [](const core::Message& cur) {
					LOG(INFO) << cur << std::endl;
				});

				auto errors = semantic.getErrors();
				std::sort(errors.begin(), errors.end());
				std::cout << "ERROR New CAll: " << std::endl;
				for_each(errors, [](const core::Message& cur) {
					LOG(INFO) << cur << std::endl;
				});

				std::cout << "NEWCALL " << core::printer::PrettyPrinter(newCall, core::printer::PrettyPrinter::OPTIONS_DETAIL) << std::endl;

				// add bind for the begin, end, step around the call
				VariableList besArgs;
				besArgs.push_back(beginArg);
				besArgs.push_back(endArg);
				besArgs.push_back(stepArg);

				auto body = builder.bindExpr(besArgs, newCall);
				auto pfor = builder.pfor(body, builder.intLit(0), builder.deref(firstSizeVar));
				auto parLambda = insieme::core::transform::extractLambda(manager, pfor);
				auto range = builder.getThreadNumRange(1); // if no range is specified, assume 1 to infinity
				auto jobExp = builder.jobExpr(range, vector<core::DeclarationStmtPtr>(), vector<core::GuardedExprPtr>(), parLambda);
				auto parallelCall = builder.callExpr(basic.getParallel(), jobExp);
				auto mergeCall = builder.callExpr(basic.getMerge(), parallelCall);

				std::cout << "MERGE CALL: " << printer::PrettyPrinter(mergeCall) << std::endl;

				nodeMap.clear();
				nodeMap.insert(std::make_pair(ifSplit, mergeCall));

				code2 = core::transform::replaceAll(manager, code2, nodeMap, true);
				std::cout << core::printer::PrettyPrinter(code2, core::printer::PrettyPrinter::OPTIONS_DETAIL);

				// Semantic check on code2
				semantic = core::check(code2, insieme::core::checks::getFullCheck());
				warnings = semantic.getWarnings();
				std::sort(warnings.begin(), warnings.end());
				for_each(warnings, [](const core::Message& cur) {
					LOG(INFO) << cur << std::endl;
				});

				errors = semantic.getErrors();
				std::sort(errors.begin(), errors.end());
				for_each(errors, [](const core::Message& cur) {
					LOG(INFO) << "---- SEMANTIC ERROR - FINAL STEP ---- \n" << cur << std::endl;
				});
			}
		});

		return code2;
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme
