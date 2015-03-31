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

// TODO: BRING to the runtime informations: kernel is splittable or not.
#include <fstream>
#define MIN_CONTEXT 40

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/annotations/naming.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/analysis/features/type_features.h"

#include "insieme/backend/ocl_host/host_extensions.h"
#include "insieme/backend/ocl_host/host_preprocessor.h"

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/ir_generator.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/backend/ocl_kernel/kernel_extensions.h"

#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/annotations/data_annotations.h"
#include "insieme/backend/ocl_kernel/kernel_poly.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/full_check.h"


using namespace insieme::core;
using namespace insieme::core::pattern;
namespace irg = insieme::core::pattern::generator::irg;

namespace insieme {
namespace backend {
namespace ocl_host {

using insieme::core::pattern::any;
using insieme::core::pattern::anyList;

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

	void printVarAddressVec(std::vector<VariableAddress> varVec){
		std::cout << "[ ";
		for_each(varVec, [&](VariableAddress var){
				 std::cout << *var << " ";
		});
		std::cout << "]" << std::endl;
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
		TreePattern getId = aT(var("id_call", irp::castExpr(any, irp::callExpr(irp::literal("get_global_id"), *any))));

		auto&& match = getId.matchPointer(expr);
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
		VariablePtr unsplittedSize;
		VariablePtr originalSize;
		DataToTransfer dataToTransfer;

		TreePattern sizeOfPattern;

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
			auto&& match = sizeOfPattern.matchPointer(expr);
			if (match)
				sizeOfCall = match->getVarBinding("sizeof").getValue().as<CallExprPtr>();
			else
				assert_fail() << "Sizeof not present :(";

            // case of a buffer that is not "size" long.
            VariablePtr varMatch = match->getVarBinding("variable").getValue().as<VariablePtr>();
            return (varMatch != originalSize) ?
                tryDeref(varMatch.as<ExpressionPtr>(), builder) :
                tryDeref(unsplittedSize.as<ExpressionPtr>(), builder);
		}

		ExpressionPtr getElems(annotations::Range range, ExpressionPtr expr, const insieme::core::lang::BasicGenerator& basic, CallExprPtr& sizeOfCall){
			auto&& match = sizeOfPattern.matchPointer(expr);
			if (match)
				sizeOfCall = match->getVarBinding("sizeof").getValue().as<CallExprPtr>();
			else
				assert_fail() << "Sizeof not present :(";

            // case of a buffer that is not "size" long.
            VariablePtr varMatch = match->getVarBinding("variable").getValue().as<VariablePtr>();

			return range.isSplittable() ?
						builder.sub(range.getUpperBoundary(), range.getLowerBoundary()).as<ExpressionPtr>() :
                        (varMatch != originalSize) ?
                            tryDeref(varMatch.as<ExpressionPtr>(), builder) :
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

								ExpressionPtr size = builder.mul(sizeOfCall, builder.castExpr(basic.getUInt8(), nElems));
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
            NodeManager& mgr = decl->getNodeManager();
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
						ExpressionPtr size = builder.mul(sizeOfCall, builder.castExpr(basic.getUInt8(), nElems));
                        //ExpressionPtr offset = builder..mul(sizeOfCall, builder.castExpr(basic.getUInt8(), range.getLowerBoundary()));
                        ExpressionPtr offset = builder.mul(sizeOfCall, builder.castExpr(basic.getUInt8(),
                                                range.isSplittable() ? range.getLowerBoundary() : builder.uintLit(0).as<ExpressionPtr>()));
                        ExpressionPtr hostPtr = builder.arrayAccess(builder.callExpr(basic.getRefDeref(),
								getVariableArg(call->getArgument(4), builder)), builder.castExpr(basic.getUInt4(), subScriptValue));
                        if(hostPtr->getType()->getNodeType() != NT_RefType) // to do a scalarToAnyRef we need an Expression with ref type
                        	hostPtr = builder.refVar(hostPtr);
						ExpressionPtr pos = builder.callExpr(basic.getScalarToArray(), hostPtr);
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
			: IRVisitor<void>(false), ranges(ranges), builder(build), unsplittedSize(unsplittedSize),
			  originalSize(originalSize) {
            sizeOfPattern = (irp::callExpr(any, aT(var("sizeof", irp::callExpr(irp::literal("sizeof"), *any))) << aT(var("variable", irp::variable(any, any))) << *any) |
                    irp::callExpr(any, aT(var("variable", irp::variable(any, any))) << aT(var("sizeof", irp::callExpr(irp::literal("sizeof"), *any))) << *any));
		}

		utils::map::PointerMap<NodePtr, NodePtr>& getNodeMap() { return nodeMap; }

		void printMap() {
			std::cout << "RangeExpressionApplier nodeMap:\n";
			for_each(nodeMap, [](std::pair<NodePtr, NodePtr> nodes){
				std::cout << nodes.first << " -> \n\t" << nodes.second << std::endl;
			});
		}

		/*
		 * replaces the sizeof(type) expressions inside dataToTransfer with integer literals
		 */
		void transformTypeToItsSize(ExpressionPtr& expr) {
			NodeMapping* sizeofEvaluator;

			NodeManager& mgr = builder.getNodeManager();
			auto mapper = makeLambdaMapper([&](unsigned index, const NodePtr& element)->NodePtr{
				if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(element)) {
					if(mgr.getLangBasic().isSizeof(call->getFunctionExpr())) {
						TypePtr ty = call->getArgument(0)->getType().as<GenericTypePtr>()->getTypeParameter(0);
						// replace the sizeof call with an estimation of its result
						return builder.intLit(analysis::features::getSizeInBytes(ty));
					}
				}
				return element->substitute(mgr, *sizeofEvaluator);
			});

			sizeofEvaluator = &mapper;
			expr = sizeofEvaluator->map(0, expr);

		}

		/*
		 * writes the DataToTransfer struct to a file as a string
		 */
		void dumpDataToTransfer(std::string filename) {
			transformTypeToItsSize(dataToTransfer.splittalbeToDevice);
			transformTypeToItsSize(dataToTransfer.nonSplittableToDevice);
			transformTypeToItsSize(dataToTransfer.splittableFromDevice);
			transformTypeToItsSize(dataToTransfer.nonSplittableFromDevice);

			std::ofstream os(filename);
			assert_true(os.is_open()) << "Could not open file to write data to transfer";
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



	core::NodePtr HostPreprocessor::process(const Converter& converter, const core::NodePtr& code) {
		Logger::get(std::cerr, ERROR, 0);
		// Semantic check on code
		//LOG(INFO) << "Errors Before OCL host preprocess: " << core::checks::check(code, core::checks::getFullCheck());
		//LOG(DEBUG) << "Code before Host Preprocessing: " << core::printer::PrettyPrinter(code, core::printer::PrettyPrinter::OPTIONS_DETAIL);

		auto& manager = converter.getNodeManager();

		core::IRBuilder builder(manager);
		auto& ext = manager.getLangExtension<Extensions>();
		auto& kernelExt = manager.getLangExtension<ocl_kernel::Extensions>();

		// create new buffer type
		TypePtr bufType = ext.bufferType;
		TypePtr refBufType = ext.refBufferType;
		TypePtr refRefBufType = builder.refType(refBufType);

		std::vector<TypePtr>  vecNewTupleType;
		std::vector<TypePtr>  vecRefNewTupleType;
		std::vector<std::vector<LiteralPtr>> vecTupleAccessVec;
		std::vector<std::vector<VariablePtr>> vecTupleVarNameVec;
		std::vector<std::vector<VariablePtr>> vecBufVarNames; // vector of buffer variables

		// find the tuple variable in the call of the kernel and all the index that are global buffers
		TreePattern callKernel = irp::callExpr(irp::literal("call_kernel"), var("okw", irp::callExpr(irp::literal("_ocl_kernel_wrapper"),
										single(var("kernLambda")))) << *any << var("offset") << any << any << var("varlist"));

		TreePattern tupleAccess = irp::arrayRefElem1D(irp::tupleMemberAccess(var("tupleVar"), var("index"), var("memberType")), var("arrayIndex"));

		TreePattern reinterpretTupleAccess = irp::refReinterpret(tupleAccess);

		TreePattern wrapGlobalTuple = irp::callExpr(irp::literal("_ocl_wrap_global"),
										irp::callExpr(manager.getLangBasic().getRefDeref(), (reinterpretTupleAccess | tupleAccess)));

		TreePattern tupleAccessOrWrap = (tupleAccess | wrapGlobalTuple);


		visitDepthFirst(code, [&](const CallExprPtr& call) {
			auto&& matchKernel = callKernel.matchPointer(call);
			if (matchKernel) {
				std::vector<LiteralPtr> tupleAccessVec;
				VariablePtr tupleVar;
				CallExprPtr varlist = static_pointer_cast<const CallExpr>(matchKernel->getVarBinding("varlist").getValue());
				visitDepthFirst(varlist, [&](const CallExprPtr& call) {

					auto&& matchGlobalWrap = wrapGlobalTuple.matchPointer(call);
					if (matchGlobalWrap) {
						 if (!tupleVar) {
							tupleVar = getVar(matchGlobalWrap, "tupleVar");
							vecTupleVarNameVec.push_back(core::analysis::getVariableNames(tupleVar, code));
							std::cout << "\n== Kernel Call Parameter Tuple == \nTupleVar-> " << tupleVar << std::endl;
							std::cout << vecTupleVarNameVec.back() << std::endl;
						}
						std::cout << "Index -> " << matchGlobalWrap->getVarBinding("index").getValue() << std::endl;
						tupleAccessVec.push_back(static_pointer_cast<const Literal>(matchGlobalWrap->getVarBinding("index").getValue()));
					}
				});

				// Create new type for the tuple (with Buffers)
				const TupleTypePtr oldTupleType = static_pointer_cast<const TupleType>(tupleVar.getType()); //FIXME BUG!!
				std::vector<TypePtr> oldTypeList = oldTupleType.getElementTypes();
				std::vector<TypePtr> newTypeList;
				for (uint i = 0; i < tupleAccessVec.size(); ++i) newTypeList.push_back(builder.refType(builder.arrayType(refBufType)));
				for (uint i = tupleAccessVec.size(); i < oldTypeList.size(); ++i) newTypeList.push_back(oldTypeList[i]);
				vecNewTupleType.push_back(builder.tupleType(newTypeList));
				vecRefNewTupleType.push_back(builder.refType(vecNewTupleType.back()));

				// Collect all the buffer names in the different scopes
				std::vector<TreePattern> treeVecInsertTuple;
				for_each(vecTupleVarNameVec.back(), [&](const VariablePtr& v) {
					for_each(tupleAccessVec, [&](const LiteralPtr& lit) {
					 treeVecInsertTuple.push_back(irp::assignment(irp::tupleRefElem(atom(v), atom(lit), any), irp::scalarToArray(var("bufVar"))));
					});
				});

				std::vector<VariablePtr> bufVarNames;
				visitDepthFirst(code, [&](const CallExprPtr& call) {
					for_each(treeVecInsertTuple, [&](const TreePattern& pattern) {
						auto&& match = pattern.matchPointer(call);
						if (match) {
							std::vector<VariablePtr> bufVarNameVec = core::analysis::getVariableNames(getVar(match, "bufVar"), code);
							// will contain the name of the buffer variables present in the different scopes
							bufVarNames.insert(std::end(bufVarNames), std::begin(bufVarNameVec), std::end(bufVarNameVec));
						}
					});
				});
				vecTupleAccessVec.push_back(tupleAccessVec);
				vecBufVarNames.push_back(bufVarNames);
				std::cout << vecBufVarNames.back() << std::endl;

			}
		});
		// At this point the vectors are full of information
		//----------------------------


		TreePattern bufDecl = irp::declarationStmt(var("Var"), aT(irp::literal("undefined")));
		TreePattern bufDeclAndInit = irp::declarationStmt(var("Var"), aT(irp::scalarToArray(aT(irp::literal("undefined")))));
		TreePattern tupleDeclAndInit = irp::declarationStmt(var("Var"), aT(irp::callExpr(manager.getLangBasic().getRefNew(), any)));

		utils::map::PointerMap<NodePtr, NodePtr> nodeMap;
		auto& basic = manager.getLangBasic();
		for (uint n = 0; n < vecBufVarNames.size(); ++n) {
			std::cout << "\n" << n+1 << " Kernel";
			auto bufVarNames = vecBufVarNames[n];
			auto tupleVarNameVec = vecTupleVarNameVec[n];
			auto refNewTupleType = vecRefNewTupleType[n];
			auto newTupleType = vecNewTupleType[n];
			auto tupleAccessVec = vecTupleAccessVec[n];

			visitDepthFirst(code, [&](const DeclarationStmtPtr& decl) {
				auto&& match = bufDecl.matchPointer(decl);
				if (match) {
					VariablePtr tmpVar = getVar(match, "Var");

					// Modify declaration and initialization of Buffer variables
					auto&& fit = std::find_if(begin(bufVarNames), end(bufVarNames), [&](const VariablePtr& vr){ return vr == tmpVar;});
					if (fit != end(bufVarNames)) {
						auto&& matchDeclInit = bufDeclAndInit.matchPointer(decl);
						if (matchDeclInit) {
							std::cout << "-> icl_buffer* " << tmpVar << " = create_buffer (...); \n";
							CallExprPtr init = static_pointer_cast<const CallExpr>(decl->getInitialization());
							CallExprPtr tmp = static_pointer_cast<const CallExpr>((init->getArgument(0)));

							VariablePtr newVar = builder.variable(refRefBufType, decl.getVariable().getID());
							ExpressionPtr size = builder.mul(builder.callExpr(basic.getUInt8(), basic.getSizeof(), tmp->getArgument(0)), tmp->getArgument(1));
							DeclarationStmtPtr newDecl = builder.declarationStmt(
										newVar, // left side
										builder.refVar(builder.callExpr(ext.createBuffer, builder.uintLit(0), size))); // right side
							nodeMap.insert(std::make_pair(decl, newDecl));
							return;
						} else {
							std::cout << "-> icl_buffer* " << tmpVar << ";\n";
							VariablePtr newVar = builder.variable(refRefBufType, decl.getVariable().getID());
							DeclarationStmtPtr newDecl = builder.declarationStmt(
										newVar, // left side
										builder.refVar(builder.callExpr(refBufType, basic.getUndefined(),builder.getTypeLiteral(refBufType)))); // right side
							nodeMap.insert(std::make_pair(decl, newDecl));
							return;
						}
					}

					// Modify declaration of the tuple variable
					fit = std::find_if(begin(tupleVarNameVec), end(tupleVarNameVec), [&](const VariablePtr& vr){ return vr == tmpVar;});
					if (fit != end(tupleVarNameVec)) {
						auto&& matchTupleDeclAndInit = tupleDeclAndInit.matchPointer(decl);
						if (matchTupleDeclAndInit) {
							std::cout << "-> tuple declaration and initialization: " << tmpVar << std::endl;
							VariablePtr newTuple = builder.variable(builder.refType(refNewTupleType), decl.getVariable().getID());
							DeclarationStmtPtr newDecl = builder.declarationStmt(
										newTuple,
										builder.refVar(builder.refNew(builder.callExpr(newTupleType, basic.getUndefined(),builder.getTypeLiteral(newTupleType)))));
							nodeMap.insert(std::make_pair(decl, newDecl));
							return;
						} else {
							std::cout << "-> tuple declaration: " << tmpVar << std::endl;
							VariablePtr newTuple = builder.variable(builder.refType(refNewTupleType), decl.getVariable().getID());
							DeclarationStmtPtr newDecl = builder.declarationStmt(
										newTuple,
										builder.refVar(builder.callExpr(refNewTupleType, basic.getUndefined(),builder.getTypeLiteral(refNewTupleType))));
							nodeMap.insert(std::make_pair(decl, newDecl));
							return;
						}
					}
				}
			});


			TreePattern readWriteBuf = irp::callExpr(aT(irp::lambda(any, var("parVar") << *any, aT(irp::compoundStmt(
																									   irp::declarationStmt(any, any) <<
																									   irp::declarationStmt(any,any) <<
																									   irp::declarationStmt(any,any) <<
																									   irp::forStmt(irp::assignment(aT(irp::arrayRefElem1D(var("leftVar"), any)), aT(irp::arrayRefElem1D(var("rightVar"), any)))) <<
																									   *any)))), aT(var("bufVar", irp::variable(any, any))) << *any);

			TreePattern delTree = irp::callExpr(irp::literal("ref.delete"), irp::callExpr(manager.getLangBasic().getRefDeref(), single(var("delVar", irp::variable(any, any)))));

			TreePattern tupleAssign = irp::assignment(irp::tupleRefElem(var("innerTupleVar"),var("index"),any), irp::scalarToArray(var("innerBufVar")));

			visitDepthFirst(code, [&](const CallExprPtr& call) {
				if (core::analysis::isCallOf(call, builder.getLangBasic().getRefAssign())) {
					// Modify buffer assignment operation
					auto&& fit = std::find_if(begin(bufVarNames), end(bufVarNames), [&](const VariablePtr& vr){ return vr == call->getArgument(0);});
					if (fit != end(bufVarNames)) {
						// skip cast expression
						CastExprPtr tmpCast = static_pointer_cast<const CastExpr>((call->getArgument(1)));
						CallExprPtr tmp = static_pointer_cast<const CallExpr>(tmpCast->getSubExpression());

						VariablePtr newVar = builder.variable(refRefBufType, static_pointer_cast<const Variable>(call->getArgument(0)).getID());
						ExpressionPtr size = builder.mul(builder.callExpr(basic.getUInt8(), basic.getSizeof(), tmp->getArgument(0)), tmp->getArgument(1));
						CallExprPtr newCall = builder.callExpr(call->getFunctionExpr(), newVar, builder.callExpr(ext.createBuffer, builder.uintLit(0), size));
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
					// Modify tuple assignment operation
					fit = std::find_if(begin(tupleVarNameVec), end(tupleVarNameVec), [&](const VariablePtr& vr){ return vr == call->getArgument(0);});
					if (fit != end(tupleVarNameVec)) {
						VariablePtr newTuple = builder.variable(builder.refType(refNewTupleType), static_pointer_cast<const Variable>(call->getArgument(0)).getID());
						CallExprPtr newCall = builder.assign(newTuple, builder.refNew(builder.callExpr(newTupleType, basic.getUndefined(),builder.getTypeLiteral(newTupleType))));
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
				}

				// Modify buffer read and write operations
				auto&& matchReadWriteBuf = readWriteBuf.matchPointer(call);
				if (matchReadWriteBuf) {
					VariablePtr bufVar = getVar(matchReadWriteBuf, "bufVar");
					VariablePtr parVar = getVar(matchReadWriteBuf, "parVar");
					VariablePtr leftVar = getVar(matchReadWriteBuf, "leftVar");
					VariablePtr rightVar = getVar(matchReadWriteBuf, "rightVar");
					LiteralPtr op;
					auto&& fit = std::find_if(begin(bufVarNames), end(bufVarNames), [&](const VariablePtr& vr){ return vr == bufVar;});
					if (fit != end(bufVarNames)) {
						if (parVar == leftVar) {
							std::cout << "-> write_buffer(" << bufVar << ", .., ..)\n";
							op = ext.writeBuffer;
						}
						else if (parVar == rightVar) {
							std::cout << "-> read_buffer(" << bufVar << ", .., ..)\n";
							op = ext.readBuffer;
						} else {
							assert_fail() << "Matching wrong readWrite Buffer function";
						}
						ExpressionPtr newVar = builder.variable(refRefBufType, bufVar.getID());
						ExpressionPtr deref = builder.deref(newVar);
						CallExprPtr newCall = builder.callExpr(op, toVector(deref, call->getArgument(1),
																			call->getArgument(2), call->getArgument(3), call->getArgument(4)));
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
				}

				// Modify buffer delete operation
				auto&& matchDel = delTree.matchPointer(call);
				if (matchDel) {
					VariablePtr delVar = getVar(matchDel, "delVar");
					// buffer case
					auto&& fit = std::find_if(begin(bufVarNames), end(bufVarNames), [&](const VariablePtr& vr){ return vr == delVar;});
					if (fit != end(bufVarNames)) {
						std::cout << "-> release_buffer(" << delVar << ");\n";
						VariablePtr newVar = builder.variable(refRefBufType, delVar.getID());
						CallExprPtr newCall = builder.callExpr(ext.releaseBuffer, builder.deref(newVar));
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
					// tuple case
					fit = std::find_if(begin(tupleVarNameVec), end(tupleVarNameVec), [&](const VariablePtr& vr){ return vr == delVar;});
					if (fit != end(tupleVarNameVec)) {
						std::cout << "-> Tuple: free(" << delVar << ");\n";
						VariablePtr newTupleVar = builder.variable(builder.refType(refNewTupleType), delVar.getID());
						CallExprPtr newCall = builder.refDelete(builder.deref(newTupleVar));
						nodeMap.insert(std::make_pair(call, newCall));
						return;
					}
				}

				// Modify tuple assignment
				auto&& matchTupleAssign = tupleAssign.matchPointer(call);
				if (matchTupleAssign) {
					VariablePtr tupleVar = getVar(matchTupleAssign, "innerTupleVar");
					std::vector<VariablePtr> tupleNames = core::analysis::getVariableNames(tupleVar, code);
					auto&& fit = std::find_if(begin(tupleVarNameVec), end(tupleVarNameVec), [&](const VariablePtr& vr){ return vr == tupleNames.back();});
					if (fit != end(tupleVarNameVec)) {
						LiteralPtr index = static_pointer_cast<const Literal>(matchTupleAssign->getVarBinding("index").getValue());
						auto&& find = std::find_if(begin(tupleAccessVec), end(tupleAccessVec), [&](const LiteralPtr& i) { return i == index;});
						VariablePtr newTupleVar = builder.variable(refNewTupleType, tupleVar.getID());
						VariablePtr bufVar = getVar(matchTupleAssign, "innerBufVar");
						VariablePtr newVar;
						if (find != end(tupleAccessVec))
							newVar = builder.variable(refRefBufType, bufVar.getID());
						else
							newVar = bufVar;
						CallExprPtr newCall = builder.assign(
									builder.refComponent(newTupleVar, utils::numeric_cast<int64_t>(index->getStringValue())),
									builder.callExpr(builder.getLangBasic().getScalarToArray(), newVar));
						nodeMap.insert(std::make_pair(call, newCall));
						std::cout << "-> tuple assignment: " << tupleVar << "[" << index << "] = " << bufVar << std::endl;
					}
				}

				// Modify tuple access
				auto&& matchTupleAccess = tupleAccessOrWrap.matchPointer(call);
				auto&& matchWrapGlobal = wrapGlobalTuple.matchPointer(call);
				if (matchTupleAccess) {
					VariablePtr tupleVar = getVar(matchTupleAccess, "tupleVar");
					auto&& fit = std::find_if(begin(tupleVarNameVec), end(tupleVarNameVec), [&](const VariablePtr& vr){ return vr == tupleVar;});
					if (fit != end(tupleVarNameVec)) {
						VariablePtr newTuple = builder.variable(newTupleType, tupleVar.getID());
						LiteralPtr index = static_pointer_cast<const Literal>(matchTupleAccess->getVarBinding("index").getValue());
						LiteralPtr memberType = static_pointer_cast<const Literal>(matchTupleAccess->getVarBinding("memberType").getValue());
						CallExprPtr newCall;
						ExpressionPtr arrayIndex = static_pointer_cast<const Expression>(matchTupleAccess->getVarBinding("arrayIndex").getValue());
						auto&& find = std::find_if(begin(tupleAccessVec), end(tupleAccessVec), [&](const LiteralPtr& i) { return i == index;});
						if (find != end(tupleAccessVec)) {
							if (matchWrapGlobal == false)
								return;
							LiteralPtr newMemberType = builder.getTypeLiteral(builder.refType(builder.arrayType(refBufType)));
							newCall = builder.callExpr(kernelExt.wrapGlobal, builder.deref(
														   builder.arrayRefElem(builder.callExpr(basic.getTupleMemberAccess(), newTuple, index, newMemberType), arrayIndex)));
						} else {
							newCall = builder.arrayRefElem(builder.callExpr(basic.getTupleMemberAccess(), newTuple, index, memberType), arrayIndex);
						}
						nodeMap.insert(std::make_pair(call, newCall));
						std::cout << "-> tuple member access: " << tupleVar << "["  << index << "]" << std::endl;
					}
				}

			});
		}

		NodePtr code2 = core::transform::replaceAll(manager, code, nodeMap, true);

		nodeMap.clear();
		for (uint n = 0; n < vecBufVarNames.size(); ++n) {
			auto bufVarNames = vecBufVarNames[n];
			auto tupleVarNameVec = vecTupleVarNameVec[n];
			auto refNewTupleType = vecRefNewTupleType[n];
			auto newTupleType = vecNewTupleType[n];

			visitDepthFirst(code2, [&](const CallExprPtr& call) {
				// interested only in call to a lambdaExpr
				const LambdaExprPtr le = dynamic_pointer_cast<const LambdaExpr>(call->getFunctionExpr());
				if (le) {
					std::vector<ExpressionPtr> args = call->getArguments();
					std::vector<VariablePtr> params = le->getParameterList();

					std::vector<ExpressionPtr> newArgs;
					std::vector<VariablePtr> newParams;
					bool insertInMap = false;
					bool containsRefType = false;
					bool containsRefRefType = false;
					for(uint i = 0; i < args.size(); ++i) {
						ExpressionPtr exp = args[i];
						// remove the refDeref around the variable
						bool refDeref = false;
						if(CallExprPtr call = dynamic_pointer_cast<const CallExpr>(exp)){
							if (manager.getLangBasic().isRefDeref(call->getFunctionExpr())){
								exp = call->getArgument(0);
								refDeref = true;
							}
						}

						VariablePtr tmpVar = dynamic_pointer_cast<const Variable>(exp);
						if (tmpVar) {
							TypePtr tmpVarType = tmpVar->getType();
							if (insieme::core::analysis::isRefType(tmpVarType)) {
								containsRefType = true;
								TypePtr rf = tmpVarType.as<core::RefTypePtr>()->getElementType();
								if (insieme::core::analysis::isRefType(rf))
									containsRefRefType = true;
							}
							// case buffer
							auto&& fit = std::find_if(begin(bufVarNames), end(bufVarNames), [&](const VariablePtr& vr){ return vr == tmpVar;});
							auto&& fitTuple = std::find_if(begin(tupleVarNameVec), end(tupleVarNameVec), [&](const VariablePtr& vr){ return vr == tmpVar;});
							if (fit != end(bufVarNames)) {
								insertInMap = true;

								if (refDeref) {
									newArgs.push_back(builder.callExpr(builder.getLangBasic().getRefDeref(), builder.variable(refRefBufType, tmpVar.getID())));
									newParams.push_back(builder.variable(refBufType, params[i].getID()));
								} else {
									newArgs.push_back(builder.variable(refRefBufType, tmpVar.getID()));
									newParams.push_back(builder.variable(refRefBufType, params[i].getID()));
								}
								// case tuple
							} else if (fitTuple != end(tupleVarNameVec)) { // TODO: Improve it
								insertInMap = true;
								TypePtr newArgType;
								TypePtr newParamType;
								if (containsRefRefType) {
									newArgType = builder.refType(refNewTupleType);
									if (refDeref)
										newParamType = refNewTupleType;
									else
										newParamType = newArgType;
								} else if (containsRefType) {
									newArgType = refNewTupleType;
									if (refDeref)
										newParamType = newTupleType;
									else
										newParamType = newArgType;
								} else {
									newArgType = newTupleType;
									newParamType = newArgType;
								}

								if (refDeref)
									newArgs.push_back(builder.callExpr(builder.getLangBasic().getRefDeref(), builder.variable(newArgType, tmpVar.getID())));
								else
									newArgs.push_back(builder.variable(newArgType, tmpVar.getID()));
								newParams.push_back(builder.variable(newParamType, params[i].getID()));
							}
							else {
								newArgs.push_back(args[i]);
								newParams.push_back(params[i]);
							}
						} else {
							newArgs.push_back(args[i]);
							newParams.push_back(params[i]);
						}
					}
					if (!insertInMap) return;

					LambdaExprPtr newLambdaEx = builder.lambdaExpr(le->getFunctionType()->getReturnType(), le->getBody(), newParams);
					CallExprPtr newCall = builder.callExpr(newLambdaEx, newArgs);
					nodeMap.insert(std::make_pair(call, newCall));
					code2 = core::transform::replaceAll(manager, code2, nodeMap, false);
					nodeMap.clear();
				}
			});
		}

		LOG(INFO) << "Errors before starting split: \n" << core::checks::check(code2, core::checks::getFullCheck());
		LOG(DEBUG) << "Code before starting split: \n" << core::printer::PrettyPrinter(code2, core::printer::PrettyPrinter::OPTIONS_DETAIL);

		// Semantic check on code2
		/*auto sem = core::checks::check(code2, insieme::core::checks::getFullCheck());
		auto warn = sem.getWarnings();
		std::sort(warn.begin(), warn.end());
		for_each(warn, [](const core::checks::Message& cur) {
			LOG(INFO) << cur << std::endl;
		});

		auto errs = sem.getErrors();
		std::sort(errs.begin(), errs.end());
		if (errs.begin()!= errs.end()) {
			LOG(ERROR) << "\n\nError in the final code before splitting:" << std::endl
					   <<     "=========================================" << std::endl;
			//---------------------------------------------------------------------------------
			for_each(errs, [&](const core::checks::Message& cur) {
				LOG(INFO) << cur;
				NodeAddress address = cur.getOrigin();
				std::stringstream ss;
				unsigned contextSize = 1;
				do {

					ss.str("");
					ss.clear();
					NodePtr&& context = address.getParentNode(
							std::min((unsigned)contextSize, address.getDepth()-contextSize)
						);
					ss << printer::PrettyPrinter(context, printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 1+2*contextSize);

				} while(ss.str().length() < MIN_CONTEXT && contextSize++ < 5);
				// LOG(INFO) << "\t Source-Node-Type: " << address->getNodeType();
				LOG(INFO) << "\t Source: " << printer::PrettyPrinter(address, printer::PrettyPrinter::OPTIONS_SINGLE_LINE);
				LOG(INFO) << "\t Context: " << ss.str() << std::endl;

				// find enclosing function
				auto fun = address;
				while(!fun.isRoot() && fun->getNodeType() != core::NT_LambdaExpr) {
					fun = fun.getParentAddress();
				}
				if (fun->getNodeType() == core::NT_LambdaExpr) {
					LOG(INFO) << "\t Context:\n" << printer::PrettyPrinter(fun, printer::PrettyPrinter::PRINT_DEREFS |
																	   printer::PrettyPrinter::JUST_OUTHERMOST_SCOPE |
																	   printer::PrettyPrinter::PRINT_CASTS) << std::endl;
				}
			});
			//---------------------------------------------------------------------------------
			LOG(ERROR) <<     "=========================================" << std::endl;
		}*/

		// UNCOMMENT TO AVOID THE SPLITTING
		return code2;



		// add kernel data range annotation
        insieme::backend::ocl_kernel::KernelPoly polyAnalyzer(code2);

		auto at = [&manager](string str) { return irp::atom(manager, str); };
		TreePattern splitPoint = irp::ifStmt(at("1u != 0u"), any, any);

		bool foundErrors = false;
		visitDepthFirst(code2, [&](const IfStmtPtr& ifSplit) {
			auto&& matchIf = splitPoint.matchPointer(ifSplit);
			if (matchIf) {
				// create the new isolated call
				/*let fun005 = fun(ref<ref<array<type000,1>>> v323, ref<int<4>> v324, ...) -> unit {}; // new variable Names
				fun005(v3, v35, ...) // Old variable names
				*/
				CallExprPtr call = insieme::core::transform::outline(manager, ifSplit->getThenBody());

				//LOG(DEBUG) << "=== NEW CALL AFTER OUTLINE ===\n" << core::printer::PrettyPrinter(call, core::printer::PrettyPrinter::OPTIONS_DETAIL);
				//LOG(INFO) << "\n==== ERROR CHECK START ====\n " << core::checks::check(call, core::checks::getFullCheck()) << "\n==== ERROR CHECK STOP ====\n ";

				CallExprPtr varlist;
				visitDepthFirst(call, [&](const CallExprPtr& cl) {
					auto&& matchKernel = callKernel.matchPointer(cl);
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
					auto&& matchKernel = callKernel.matchPointer(kernelCandidate);
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
								VariablePtr varPtr = getVariableArg(pair.first, builder);//static_pointer_cast<const Variable>(deref->getArgument(0));
								bufferMap[bufAdd] = core::Address<const core::Variable>::find(varPtr, callBody);
							}
						});
						if(boundaryVars.find(pair.second) != boundaryVars.end()) {
							VariablePtr varPtr = getVariableArg(pair.first, builder);//static_pointer_cast<const Variable>(static_pointer_cast<const CallExpr>(pair.first)->getArgument(0));
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
						if(insieme::core::annotations::hasNameAttached(var)){
							 auto cName = insieme::core::annotations::getAttachedName(var);
							 if (cName.compare("size") == 0){
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
							// alwasys keep the unsplitted size. Otherwise everything is complicated. Use a fresh variable for it
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
						newVar = builder.deref(newVar);
					if(!(oldVar->getType()->getNodeType() == core::NT_RefType) && (newVar->getType()->getNodeType() == core::NT_RefType))
						newVar = builder.deref(newVar);
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
											builder.deref(sizeVar), builder.sub(end, begin)).as<CompoundStmtPtr>();
				// NEW CODE
				//newBody2 = core::transform::replaceAll(manager, newBody2, sizeVar, newSize).as<CompoundStmtPtr>();

				//LOG(DEBUG) << "=== NEWBODY ===\n" << core::printer::PrettyPrinter(newBody, core::printer::PrettyPrinter::OPTIONS_DETAIL);
				//LOG(INFO) << "\n==== ERROR NEWBODY START ====\n " << core::checks::check(newBody, core::checks::getFullCheck()) << "\n==== ERROR CHECK STOP ====\n ";

				//LOG(DEBUG) << "=== NEWBODY2 ===\n" << core::printer::PrettyPrinter(newBody2, core::printer::PrettyPrinter::OPTIONS_DETAIL);
				//LOG(INFO) << "\n==== ERROR NEWBODY2 START ====\n " << core::checks::check(newBody2, core::checks::getFullCheck()) << "\n==== ERROR CHECK STOP ====\n ";

				visitDepthFirst(newBody2, [&](const CallExprPtr& cl) {
					auto&& matchKernel = callKernel.matchPointer(cl);
					if (matchKernel) {
						// replace the previous inserted end-begin with the correct end because of the offset
						CallExprPtr varlist = static_pointer_cast<const CallExpr>(matchKernel->getVarBinding("varlist").getValue());
						CallExprPtr varlistNew = core::transform::replaceAll(manager, varlist, builder.sub(end, begin), end).as<CallExprPtr>();
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
						auto&& matchKernel = callKernel.matchPointer(cl);
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


						CompoundStmtPtr newKernBody = toReplace.empty() ? kernBody : core::transform::replaceAll(manager, toReplace).as<CompoundStmtPtr>();

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
				newBody2 = builder.compoundStmt(toVector<StatementPtr>(builder.declarationStmt(endMinusOne,
						builder.sub(end, builder.literal(end->getType(), "1"))), newBody2));

				// building the new Call
				LambdaExprPtr newLambda = builder.lambdaExpr(call->getType(), newBody2, parameters);

				CallExprPtr newCall = builder.callExpr(newLambda, arguments);

				//LOG(DEBUG) << "=== NEW CALL ===\n" << core::printer::PrettyPrinter(newCall, core::printer::PrettyPrinter::OPTIONS_DETAIL);
				//LOG(INFO) << "\n==== ERROR NEW CALL ====\n " << core::checks::check(newCall, core::checks::getFullCheck()) << "\n==== ERROR STOP ====\n ";

				// add bind for the begin, end, step around the call
				VariableList besArgs;
				besArgs.push_back(beginArg);
				besArgs.push_back(endArg);
				besArgs.push_back(stepArg);

				auto body = builder.bindExpr(besArgs, newCall);
				auto pfor = builder.pfor(body, builder.intLit(0), builder.deref(firstSizeVar));
				auto parLambda = insieme::core::transform::extractLambda(manager, pfor);
				auto range = builder.getThreadNumRange(1); // if no range is specified, assume 1 to infinity
				auto jobExp = builder.jobExpr(range, vector<core::DeclarationStmtPtr>(), parLambda);
				auto parallelCall = builder.callExpr(basic.getParallel(), jobExp);
				auto mergeCall = builder.callExpr(basic.getMerge(), parallelCall);

				std::cout << "MERGE CALL: " << printer::PrettyPrinter(mergeCall) << std::endl;

				nodeMap.clear();
				nodeMap.insert(std::make_pair(ifSplit, mergeCall));

				code2 = core::transform::replaceAll(manager, code2, nodeMap, false);

				std::cout << "FINAL CODE " << core::printer::PrettyPrinter(code2, core::printer::PrettyPrinter::OPTIONS_DETAIL) << std::endl;

				// Semantic check on code2
				auto semantic = core::checks::check(code2, insieme::core::checks::getFullCheck());
				auto warnings = semantic.getWarnings();
				std::sort(warnings.begin(), warnings.end());
				for_each(warnings, [](const core::checks::Message& cur) {
					LOG(INFO) << cur << std::endl;
				});

				std::cout << "CHECKING FOR ERRORS" << std::endl;
				auto errors = semantic.getErrors();
				std::sort(errors.begin(), errors.end());
				for_each(errors, [&builder, &foundErrors](const core::checks::Message& cur) {
					LOG(INFO) << "---- SEMANTIC ERROR - FINAL STEP ---- \n" << cur << std::endl;
					foundErrors = true;
				});
			}
		});

		assert_false(foundErrors) << "Semantic errors when generating the splitting";

		return code2;
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme
