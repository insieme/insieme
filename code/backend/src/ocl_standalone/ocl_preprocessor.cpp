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

#include "insieme/backend/ocl_standalone/ocl_preprocessor.h"

#include <algorithm>

#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/expressions.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/backend/ocl_standalone/ocl_standalone_extensions.h"

namespace insieme {
namespace backend {
namespace ocl_standalone {

	using namespace insieme::annotations::ocl;

	namespace {

		enum AddressSpace {
			PRIVATE,
			LOCAL,
			GLOBAL,
			CONSTANT,
		};

		/**
		 * Tests whether the given lambda is marked to converted into an OpenCL kernel.
		 */
		bool isKernel(const core::LambdaExprPtr& lambda) {
			if (!lambda->hasAnnotation(BaseAnnotation::KEY)) {
				return false;
			}

			BaseAnnotationPtr&& annotations = lambda->getAnnotation(BaseAnnotation::KEY);
			assert(annotations && "BaseAnnotation is empty");

			return any(annotations->getAnnotationList(), [](const BaseAnnotation::SubAnnotationPtr& cur) {
				return dynamic_pointer_cast<KernelFctAnnotation>(cur);
			});
		}

		core::JobExprPtr getGlobalJob(const core::LambdaExprPtr& kernel) {
			core::StatementPtr body = kernel->getLambda()->getBody();
			core::StatementPtr expr = static_pointer_cast<const core::CompoundStmt>(body)->getStatements()[1];
			return static_pointer_cast<const core::JobExpr>(core::analysis::getArgument(expr, 0));
		}

		typedef utils::map::PointerMap<core::VariablePtr, AddressSpace> AddressSpaceMap;

		/**
		 * Determines for each of the parameters of the given kernel whether it is referencing
		 * a memory location within the local or global memory space.
		 */
		AddressSpaceMap getAddressSpaces(const core::LambdaExprPtr& kernel) {

			// The address spaces are deduced as follows:
			//	- variables used to initialized values within the local declarations of
			//		the global job are referencing memory locations within the global memory
			//	- all other variables are referencing local memory locations


			// get outer-most job expression
			core::JobExprPtr job = getGlobalJob(kernel);

			// collect list of initialization values
			std::vector<core::ExpressionPtr> initValues;
			for_each(job->getLocalDecls(), [&](const core::DeclarationStmtPtr& cur) {
				initValues.push_back(cur->getInitialization());
			});

			// fix address spaces for each parameter
			AddressSpaceMap res;
			for_each(kernel->getParameterList(), [&](const core::VariablePtr& cur) {
				AddressSpace space = AddressSpace::PRIVATE;
				if (cur->getType()->getNodeType() == core::NT_RefType) {
					space = (contains(initValues, cur)) ? AddressSpace::GLOBAL : AddressSpace::LOCAL;
				}
				res[cur] = space;
			});
			return res;
		}


		/**
		 * Maps variables to their origins. The origin of a variable is either another variable
		 * within an outer scope or a initialization value.
		 */
		typedef utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> VariableMap;

		/**
		 * Combines the given variable maps by computing the concatenation of res and other.
		 * Hence, if A was mapped to B in res and B was mapped to C in other, the resulting
		 * map will map A to C. The resulting map will be the res map - which is modified
		 * during the execution.
		 */
		VariableMap& compose(VariableMap& res, const VariableMap& other) {
			for_each(res, [&](VariableMap::value_type& entry){
				const core::ExpressionPtr& value = entry.second;
				if (value->getNodeType() == core::NT_Variable) {
					auto pos = other.find(static_pointer_cast<const core::Variable>(value));
					if (pos != other.end()) {
						entry.second = pos->second;
					}
				}
			});
			return res;
		}


		VariableMap& mapBodyVars(VariableMap& res, const core::CallExprPtr& call);
		VariableMap& mapBodyVars(VariableMap& res, const core::JobExprPtr& job);


		VariableMap& mapBodyVars(VariableMap& res, const core::CallExprPtr& call) {
			auto& basic = call->getNodeManager().getBasicGenerator();

			core::LambdaExprPtr fun = static_pointer_cast<const core::LambdaExpr>(call->getFunctionExpr());
			auto body = fun->getBody();
			if (body->getNodeType() == core::NT_CompoundStmt) {
				body = static_pointer_cast<const core::CompoundStmt>(body)->getStatements()[0];
			}

			// check for termination
			if (!core::analysis::isCallOf(body, basic.getParallel())) {
				// this is the innermost lambda containing the kernel code
				for_each(fun->getParameterList(), [&](const core::VariablePtr& param) {
					res[param] = param;
				});
			} else {
				res = mapBodyVars(res, static_pointer_cast<const core::JobExpr>(core::analysis::getArgument(body, 0)));
			}

			// compute local parameter to argument mapping
			VariableMap cur;
			for_range(make_paired_range(fun->getParameterList(), call->getArguments()),
					[&](const std::pair<const core::VariablePtr, const core::ExpressionPtr>& pair) {
						cur[pair.first] = pair.second;
			});

			// compose result of sub-tree and call
			return compose(res, cur);
		}

		VariableMap& mapBodyVars(VariableMap& res, const core::JobExprPtr& job) {

			// compute variable map recursively
			core::BindExprPtr bind = static_pointer_cast<const core::BindExpr>(job->getDefaultStmt());
			mapBodyVars(res, bind->getCall());

			// update variable map ...
			core::DeclarationStmtPtr decl;

			// compute local local-declaration mapping
			VariableMap cur;
			for_each(job->getLocalDecls(), [&](const core::DeclarationStmtPtr& decl) {
				cur[decl->getVariable()] = decl->getInitialization();
			});

			// compose result of sub-tree and call
			return compose(res, cur);
		}

		VariableMap mapBodyVars(const core::LambdaExprPtr& kernel) {
			VariableMap map;
			return mapBodyVars(map, getGlobalJob(kernel));
		}


		// --------------------------------------------------------------------------------------------------------------
		//
		// --------------------------------------------------------------------------------------------------------------

		class TypeWrapper : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;
			const Extensions extensions;

		public:

			TypeWrapper(core::NodeManager& manager) :
				manager(manager),  extensions(manager) {};

			const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				// perform conversion in post-order
				core::NodePtr res = ptr->substitute(manager, *this);

				// only interested in lambda expressions
				if (ptr->getNodeType() != core::NT_LambdaExpr) {
					return res;
				}

				// extract lambda
				const core::LambdaExprPtr& lambda = static_pointer_cast<const core::LambdaExpr>(res);

				if (!isKernel(lambda)) {
					return res;
				}


				// TODO:
				// 		- replace build ins
				//		- add address space modifier

				// get required address space modifiers
				AddressSpaceMap varMap = getAddressSpaces(lambda);

				LOG(INFO) << "AddressSpaces: " << varMap;

				VariableMap&& map = mapBodyVars(lambda);
				LOG(INFO) << "Body Vars: " << map;


//				LOG(INFO) << "Function with some Opencl Annotation...\n";
//
//
//				for(auto iter = annotations->getAnnotationListBegin(); iter != annotations->getAnnotationListEnd(); ++iter) {
//
//					KernelFctAnnotationPtr kf = std::dynamic_pointer_cast<KernelFctAnnotation>(*iter);
//					if(!kf) { continue; }
//
//					LOG(INFO) << "Function with kernel annotation...\n";
//
//					// Done here:
//					//   - last two parameters replaced by get_global_size / get_local_size / get_num_groups
//					//	 - annotating parameters to be local / global ...
//
//					// -------------------------------------------------------------------------------------
//					// get sizes:
//
//					const LambdaPtr& oldLambda = ptr->getLambda();
//					const Lambda::ParamList& oldParams = oldLambda->getParameterList();
//					const CompoundStmtPtr& oldBody = dynamic_pointer_cast<const CompoundStmt>(oldLambda->getBody());
//					const FunctionTypePtr& oldFuncType = oldLambda->getType();
//
//					// new paramList (case IR created from OpenCL frontend)
//					for (uint i = 0; i < oldParams.size()-2; i++){
//						const VariablePtr& tmpVar = oldParams.at(i);
//						qualifierMap.insert(std::make_pair(tmpVar->getId(), tmpVar));
//					}
//
//					// add builtin annotation for get_global_size & get_local_size to the variable
//					addBuiltinAnnotation(builder, qualifierMap, oldParams.at(oldParams.size()-2), "get_global_size");
//					addBuiltinAnnotation(builder, qualifierMap, oldParams.at(oldParams.size()-1), "get_local_size");
//
//					// new functionType
//					const core::TypeList& oldArgs = oldFuncType->getParameterTypes();
//
//					//const core::TypePtr& retType = oldFuncType->getReturnType();
//					// set the return type of the kernel to void
//					const core::TypePtr& retType = (builder.getNodeManager()).basic.getUnit();
//
//					TypeList newArgs;
//					for (uint i = 0; i < oldArgs.size()-2; i++){
//						newArgs.push_back(oldArgs.at(i));
//					}
//					const FunctionTypePtr& newFuncType = builder.functionType(newArgs, retType);
//
//					const vector<StatementPtr>& bodyCompoundStmt = oldBody->getStatements();
//
//					// -------------------------------------------------------------------------------------
//					// num groups:
//
//					// add builtin for get_num_groups
//					const DeclarationStmtPtr& dcl = dynamic_pointer_cast<const DeclarationStmt>(bodyCompoundStmt.front());
//					addBuiltinAnnotation(builder, qualifierMap, dcl->getVariable(), "get_num_groups");
//
//					// -------------------------------------------------------------------------------------
//
//
//					// TODO: fix the [1] with the pattern matching
//					const CallExprPtr& globalParallel = dynamic_pointer_cast<const CallExpr>(bodyCompoundStmt[1]);
//
//					const vector<ExpressionPtr>& globalExpr = globalParallel->getArguments();
//
//					const JobExprPtr& globalJob = dynamic_pointer_cast<const JobExpr>(globalExpr.back());
//
//					// Check for global variables
//					const vector<DeclarationStmtPtr>& globalJobDecls = globalJob->getLocalDecls();
//					for_each(globalJobDecls, [&](const DeclarationStmtPtr& curDecl) {
//						unsigned newName = (curDecl->getVariable())->getId();
//						unsigned oldName = (dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId();
//
//						backwardVarNameMap.insert(std::make_pair(newName, oldName));
//						forwardVarNameMap.insert(std::make_pair(oldName, newName));
//						unsigned firstVal = getVarName(backwardVarNameMap, oldName);
//						// Add global qualifier
//						addQualifier(qualifierMap, firstVal, AddressSpaceAnnotation::addressSpace::GLOBAL);
//					});
//
//					const BindExprPtr& globalBind =  dynamic_pointer_cast<const BindExpr>(globalJob->getDefaultStmt());
//
//					const CallExprPtr& globalCall = globalBind->getCall();
//					const vector<ExpressionPtr> globalOldValues = globalCall->getArguments();
//					const LambdaExprPtr& globalParFct = dynamic_pointer_cast<const LambdaExpr>(globalCall->getFunctionExpr());
//
//					const std::vector<VariablePtr>& globalNewValues = globalParFct->getParameterList();
//
//					auto&& iter2 = globalOldValues.begin();
//					for (auto&& iter = globalNewValues.begin(); iter != globalNewValues.end(); ++iter, ++iter2){
//						unsigned newName = (*iter)->getId();
//						unsigned oldName = (dynamic_pointer_cast<const Variable>(*iter2))->getId();
//
//						backwardVarNameMap.insert(std::make_pair(newName, oldName));
//						forwardVarNameMap.insert(std::make_pair(oldName, newName));
//					}
//
//					const CompoundStmtPtr& globalParBody = dynamic_pointer_cast<const CompoundStmt>(globalParFct->getBody());
//
//					const vector<StatementPtr>& globalBodyStmts = globalParBody->getStatements();
//
//					const CallExprPtr& localParallel =  dynamic_pointer_cast<const CallExpr>(globalBodyStmts.front());
//
//					const vector<ExpressionPtr>& localExpr = localParallel->getArguments();
//
//					const JobExprPtr& localJob = dynamic_pointer_cast<const JobExpr>(localExpr.back());
//
//					const vector<DeclarationStmtPtr>& localJobDecls = localJob->getLocalDecls();
//
//					// declarations that we want to add to the body of the function
//					vector<DeclarationStmtPtr> newBodyDecls;
//
//					for_each(localJobDecls, [&](const DeclarationStmtPtr& curDecl) {
//						unsigned newName = (curDecl->getVariable())->getId();
//						if (dynamic_pointer_cast<const Variable>(curDecl->getInitialization())){
//							unsigned oldName = (dynamic_pointer_cast<const Variable>(curDecl->getInitialization()))->getId();
//
//							backwardVarNameMap.insert(std::make_pair(newName, oldName));
//							forwardVarNameMap.insert(std::make_pair(oldName, newName));
//							unsigned firstVal = getVarName(backwardVarNameMap, oldName);
//							// Add local qualifier
//							addQualifier(qualifierMap, firstVal, AddressSpaceAnnotation::addressSpace::LOCAL);
//						}
//						else {
//							// for example: v17 = initZero(ref<real<4>> // literal
//							qualifierMap.insert(std::make_pair(newName, curDecl->getVariable()));
//							addQualifier(qualifierMap, newName, AddressSpaceAnnotation::addressSpace::LOCAL);
//							newBodyDecls.push_back(curDecl);
//						}
//					});
//
//					const BindExprPtr& localBind =  dynamic_pointer_cast<const BindExpr>(localJob->getDefaultStmt());
//
//					const CallExprPtr& localCall = localBind->getCall();
//					const vector<ExpressionPtr> localOldValues = localCall->getArguments();
//					const LambdaExprPtr& localParFct = dynamic_pointer_cast<const LambdaExpr>(localCall->getFunctionExpr());
//
//					const std::vector<VariablePtr>& localNewValues = localParFct->getParameterList();
//
//					iter2 = localOldValues.begin();
//					for (auto&& iter = localNewValues.begin(); iter != localNewValues.end(); ++iter, ++iter2){
//						unsigned newName = (*iter)->getId();
//						unsigned oldName = (dynamic_pointer_cast<const Variable>(*iter2))->getId();
//						//LOG(INFO) << newName << " " << oldName << '\n';
//						backwardVarNameMap.insert(std::make_pair(newName, oldName));
//						forwardVarNameMap.insert(std::make_pair(oldName, newName));
//						qualifierMap.insert(std::make_pair(newName, *iter));
//					}
//
//					Lambda::ParamList newParams;
//					for (uint i = 0; i < oldParams.size()-2; i++){
//						unsigned oldName = (oldParams.at(i))->getId();
//						unsigned newName = getVarName(forwardVarNameMap, oldName);
//						moveQualifier(qualifierMap, oldName, newName);
//						auto&& fit = qualifierMap.find(newName);
//						if (fit != qualifierMap.end()) {
//							newParams.push_back(fit->second);
//						} else {
//							assert(false && "WTF: varName not in qualifierMap!!");
//						}
//					}
//
//					// modify the DeclarationStmts that we have to add to the body
//					vector<StatementPtr> newRenameBodyDecls;
//					for_each(newBodyDecls, [&](const DeclarationStmtPtr& curDecl) {
//						unsigned oldName = (curDecl->getVariable())->getId();
//						unsigned newName = getVarName(forwardVarNameMap, oldName);
//						moveQualifier(qualifierMap, oldName, newName);
//						auto&& fit = qualifierMap.find(newName);
//						if (fit != qualifierMap.end()) {
//							newRenameBodyDecls.push_back(builder.declarationStmt(fit->second,curDecl->getInitialization()));
//						} else {
//							assert(false && "WTF: varName not in qualifierMap!!");
//						}
//					});
//
//					const CompoundStmtPtr& localParBody = dynamic_pointer_cast<const CompoundStmt>(localParFct->getBody());
//
//					for_each(localParBody->getStatements(), [&](const StatementPtr& curStmt) {
//						newRenameBodyDecls.push_back(curStmt);
//					});
//
//					CompoundStmtPtr newBody = builder.compoundStmt(newRenameBodyDecls);
//
//					for (uint i = 0; i < oldArgs.size()-2; i++){
//						newArgs.push_back(oldArgs.at(i));
//					}
//
//					LambdaExprPtr&& newFunc = builder.lambdaExpr(newFuncType, newParams, newBody);
//					// add the annotation to the lambda and not to the lambdaExpr for then use
//					// the sinmple_backend with addFunctionPrefix
//					KernelFctAnnotationPtr an(new KernelFctAnnotation());
//					const LambdaPtr& newLambda = newFunc->getLambda();
//					newLambda->setAnnotations(oldLambda->getAnnotations());
//					newLambda->addAnnotation(an);
//
//					LOG(INFO) << "----- Insieme IR generated by the OpenCL Backend -----";
//					LOG(INFO) << printer::PrettyPrinter(newLambda) << std::endl;
//
//					FunctionManager& funManager = getConversionContext().getFunctionManager();
//					const CodeFragmentPtr& code = getCurrentCodeFragment();
//					code << funManager.getFunctionName(code, newFunc);
//				}



				return ptr;
			}
		};


	}


	core::NodePtr OCLPreprocessor::process(core::NodeManager& manager, const core::NodePtr& code) {

		// TODO:
		// 		- find kernel lambdas, updated types

		// the converter does the magic
		TypeWrapper wrapper(manager);
		return wrapper.map(code);
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme
