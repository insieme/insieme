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

#include <algorithm>
#include <fstream>

#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/dump/binary_dump.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/annotations/naming.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/ir_generator.h"
#include "insieme/transform/rulebased/transformations.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"

#include "insieme/backend/ocl_host/host_extensions.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

	using namespace insieme::annotations::ocl;
	namespace irp =  insieme::core::pattern::irp;
	namespace tr = insieme::core::pattern;
    namespace rb = insieme::transform::rulebased;

		/**
		 * Tests whether the given lambda is marked to converted into an OpenCL kernel.
		 */
		bool isKernel(const core::LambdaExprPtr& lambda) {
			if (!lambda->hasAnnotation(BaseAnnotation::KEY)) {
				return false;
			}

			BaseAnnotationPtr&& annotations = lambda->getAnnotation(BaseAnnotation::KEY);
			assert_true(annotations) << "BaseAnnotation is empty";

			return any(annotations->getAnnotationList(), [](const BaseAnnotation::SubAnnotationPtr& cur) {
				return dynamic_pointer_cast<KernelFctAnnotation>(cur);
			});
		}

		core::JobExprPtr getGlobalJob(const core::LambdaExprPtr& kernel) {
			core::StatementPtr body = kernel->getLambda()->getBody();
			core::StatementPtr expr = static_pointer_cast<const core::CompoundStmt>(body)->getStatements()[1];
			return static_pointer_cast<const core::JobExpr>(core::analysis::getArgument(expr, 0));
		}

		/**
		 * Determines for each of the parameters of the given kernel whether it is referencing
		 * a memory location within the local or global memory space.
		 */
		AddressSpaceMap getAddressSpaces(const core::LambdaExprPtr& kernel, const VariableMap& map) {

			// The address spaces are deduced as follows:
			//	- variables used to initialized values within the local declarations of
			//    the global job are referencing memory locations within the global memory
			//	- all other variables are referencing local memory locations


			// fix address spaces for each parameter
			AddressSpaceMap res;
			auto& params = kernel->getParameterList()->getElements();
			for_each(params.begin(), params.end()-2, [&](const core::VariablePtr& cur) {
				AddressSpace space = AddressSpace::PRIVATE;
				if (cur->getType()->getNodeType() == core::NT_RefType) {
					//get annotation
					namespace ocl = insieme::annotations::ocl;
					if(cur->hasAnnotation(ocl::BaseAnnotation::KEY)) {
						auto declAnn = cur->getAnnotation(ocl::BaseAnnotation::KEY);
						for(ocl::BaseAnnotation::AnnotationList::const_iterator I = declAnn->getAnnotationListBegin();
							I < declAnn->getAnnotationListEnd(); ++I) {
							if(ocl::AddressSpaceAnnotationPtr as = std::dynamic_pointer_cast<ocl::AddressSpaceAnnotation>(*I)) {
								switch(as->getAddressSpace()) {
									case ocl::AddressSpaceAnnotation::addressSpace::GLOBAL: space = AddressSpace::GLOBAL; break;
									case ocl::AddressSpaceAnnotation::addressSpace::LOCAL: space = AddressSpace::LOCAL; break;
									default: space = AddressSpace::PRIVATE; break;
								}
							}
						}
					}
				}
				//find the variable that is mapped to cur
				core::VariablePtr mappedVar = nullptr;
				auto it = map.find(cur);
				//FIXME: What happens if we have recursive kernels 
				//(e.g., fun0(v1,...) -> fun1(v10,...) -> core(v100,...).
				//We need a map that maps from outermost to innermost function
				//and not only one step beyond outermost function. 
				if((it != map.end()) && it->second.isa<core::VariablePtr>()) {
					mappedVar = it->second.as<core::VariablePtr>();
					res[mappedVar] = space;
				}
			});
			return res;
		}


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
			auto& basic = call->getNodeManager().getLangBasic();

			core::LambdaExprPtr fun = static_pointer_cast<const core::LambdaExpr>(call->getFunctionExpr());
			auto body = fun->getBody()->getStatements()[0];

			// check for termination
			if (!core::analysis::isCallOf(body, basic.getParallel())) {
				// this is the innermost lambda containing the kernel code
				for_each(fun->getParameterList()->getElements(), [&](const core::VariablePtr& param) {
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
			core::BindExprPtr bind = static_pointer_cast<const core::BindExpr>(job->getBody());
			mapBodyVars(res, bind->getCall());

			// compute local local-declaration mapping
			VariableMap cur;
// FIXME get rid of localDecls
//			for_each(job->getLocalDecls()->getElements(), [&](const core::DeclarationStmtPtr& decl) {
//				cur[decl->getVariable()] = decl->getInitialization();
//			});

			// compose result of sub-tree and call
			return compose(res, cur);
		}

		VariableMap mapBodyVars(const core::LambdaExprPtr& kernel) {
			VariableMap map;
			return mapBodyVars(map, getGlobalJob(kernel));
		}

		core::CallExprPtr getKernelCoreCall(const core::BindExprPtr& bind) {
			return bind->getCall();
		}

		core::StatementPtr getKernelCoreCall(const core::LambdaExprPtr& lambda) {
			return getKernelCoreCall(static_pointer_cast<const core::BindExpr>(getGlobalJob(lambda)->getBody()));
		}

		core::StatementPtr getKernelCore(const core::BindExprPtr& bind) {
			auto& basic = bind->getNodeManager().getLangBasic();

			core::StatementPtr body = static_pointer_cast<const core::LambdaExpr>(bind->getCall()->getFunctionExpr())->getBody();
			if (body->getNodeType() == core::NT_CompoundStmt) {
				const vector<core::StatementPtr>& stmts = static_pointer_cast<const core::CompoundStmt>(body)->getStatements();
				//find the parallel call
				for(auto stmt : stmts) {
					if (core::analysis::isCallOf(stmt, basic.getParallel())) {
						body = stmt;
					}
				}
			}

			assert(core::analysis::isCallOf(body, basic.getParallel()));
			//ok, we have a parallel call. Lets get the bind and the call
			core::JobExprPtr job = static_pointer_cast<const core::JobExpr>(core::analysis::getArgument(body, 0));
			core::CallExprPtr callExpr = static_pointer_cast<const core::BindExpr>(job->getBody())->getCall();
			//extract the functionexpr from the call.
			return callExpr->getFunctionExpr();
		}


		core::StatementPtr getKernelCore(const core::LambdaExprPtr& lambda) {
			return getKernelCore(static_pointer_cast<const core::BindExpr>(getGlobalJob(lambda)->getBody()));
		}


		bool isGetIDHelper(const core::ExpressionPtr& fun, int value) {
			core::NodeManager manager;
			core::IRBuilder builder(manager);
			insieme::core::pattern::TreePattern functionID = irp::lambdaExpr(tr::any, aT(tr::any,
									  irp::lambda(tr::any, *tr::any, var("body", irp::compoundStmt(*tr::any)))));

			insieme::core::pattern::TreePattern getThreadID = tr::aT(irp::callExpr(irp::literal("getThreadID"), tr::var("lit") << *tr::any));

			auto&& matchFunctionID = functionID.matchPointer(fun);
			if (matchFunctionID) {
				core::CompoundStmtPtr body = static_pointer_cast<const core::CompoundStmt>(matchFunctionID->getVarBinding("body").getValue());
				if(body->getStatements().size() >= 2) {
					core::StatementPtr stmt1 = static_pointer_cast<const core::Statement>(body->getStatements()[0]);
					core::StatementPtr stmt2 = static_pointer_cast<const core::Statement>(body->getStatements()[1]);
					auto&& matchGetThreadID1 = getThreadID.matchPointer(stmt1);
					auto&& matchGetThreadID2 = getThreadID.matchPointer(stmt2);
					if(matchGetThreadID1) {
						core::LiteralPtr lit = static_pointer_cast<const core::Literal>(matchGetThreadID1->getVarBinding("lit").getValue());
						if (value == 0 && matchGetThreadID1 && matchGetThreadID2)			  return true;
						if (value == 1 && matchGetThreadID1 && (*lit == *builder.uintLit(0))) return true;
						if (value == 2 && matchGetThreadID1 && (*lit == *builder.uintLit(1))) return true;
					}
				}
			}
			return false;
		}

		bool isGetGlobalID(const core::ExpressionPtr& expr) {
			return isGetIDHelper(expr, 0);
		}

		bool isGetLocalID(const core::ExpressionPtr& expr) {
			return isGetIDHelper(expr, 1);
		}

		bool isGetGroupID(const core::ExpressionPtr& expr) {
			return isGetIDHelper(expr, 2);
		}

namespace {

		class BuildInReplacer : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;

			core::VariablePtr globalSizeVar;
			core::VariablePtr localSizeVar;
			core::VariablePtr numGroupsVar;

		public:

			BuildInReplacer(core::NodeManager& manager, const core::VariablePtr& globalSize, const core::VariablePtr& localSize, const core::VariablePtr& numGroups)
					: manager(manager),  globalSizeVar(globalSize), localSizeVar(localSize), numGroupsVar(numGroups) {}


			const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				core::IRBuilder builder(manager);
				auto& basic = manager.getLangBasic();
				auto& extensions = manager.getLangExtension<Extensions>();

				// perform conversion in post-order
				core::NodePtr res = ptr->substitute(manager, *this);

				// only interested in lambda expressions
				if (ptr->getNodeType() != core::NT_CallExpr) {
					return res;
				}

				core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(res);

				// ceck for access to global ids, replace for getLocalID, getGlobalID, getGroup
				const core::TypePtr uint4 = basic.getUInt4();
				const auto& fun = call->getFunctionExpr();
				if (isGetGlobalID(fun))
					return builder.callExpr(uint4, extensions.getGlobalID, toVector(call->getArgument(0)));
				if (isGetLocalID(fun))
					return builder.callExpr(uint4, extensions.getLocalID, toVector(call->getArgument(0)));
				if (isGetGroupID(fun))
					return builder.callExpr(uint4, extensions.getGroupID, toVector(call->getArgument(0)));

				if (basic.isVectorSubscript(fun)) {
					auto target = call->getArgument(0);
					auto index = call->getArgument(1);

					if (*target == *globalSizeVar) {
						return builder.callExpr(uint4, extensions.getGlobalSize, toVector(index));
					}
					if (*target == *localSizeVar) {
						return builder.callExpr(uint4, extensions.getLocalSize, toVector(index));
					}
					if (*target == *numGroupsVar) {
						return builder.callExpr(uint4, extensions.getNumGroups, toVector(index));
					}
				}

				// Problem with the ThreadGroup type handling in the Backend
				/*if (basic.isBarrier(fun)) {
					auto threadGroup = call->getArgument(0);
					if (core::analysis::isCallOf(threadGroup, basic.getGetThreadGroup())) {
						auto arg = core::analysis::getArgument(threadGroup, 0);
						if (arg->getNodeType() == core::NT_Literal) {
							core::LiteralPtr argument = static_pointer_cast<const core::Literal>(arg);
							core::LiteralPtr lit;
							if (argument->getStringValue() == "0") {
								lit = builder.literal(threadGroup->getType(),"CLK_LOCAL_MEM_FENCE");
							} else if (argument->getStringValue() == "1") {
								lit = builder.literal(threadGroup->getType(),"CLK_GLOBAL_MEM_FENCE");
							}
							if (lit) {
								return builder.callExpr(call->getType(), basic.getBarrier(), toVector<core::ExpressionPtr>(lit));
							}
						}
					}
				}*/

				if (basic.isBarrier(fun)) {
					auto threadGroup = call->getArgument(0);
					if (core::analysis::isCallOf(threadGroup, basic.getGetThreadGroup())) {
						auto arg = core::analysis::getArgument(threadGroup, 0);
						if (arg->getNodeType() == core::NT_Literal) {
							core::LiteralPtr argument = static_pointer_cast<const core::Literal>(arg);
							core::LiteralPtr lit;
							if (argument->getStringValue() == "0") {
								return core::lang::getLiteral(manager, "unit", "barrier(CLK_LOCAL_MEM_FENCE)");
							} else if (argument->getStringValue() == "1") {
								return core::lang::getLiteral(manager, "unit", "barrier(CLK_GLOBAL_MEM_FENCE)");
							}
						}
					}
				}
				return res;
			}


		};

        class TypeUnwrapper : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;
            const Extensions& extensions;
            std::vector<core::VariablePtr> varVec;
            bool unwrapAll;

        public:
            TypeUnwrapper(core::NodeManager& manager, const std::vector<core::VariablePtr>& varVec)
                : manager(manager), extensions(manager.getLangExtension<Extensions>()), varVec(varVec), unwrapAll(false) {}

            TypeUnwrapper(core::NodeManager& manager)
                : manager(manager), extensions(manager.getLangExtension<Extensions>()), unwrapAll(true) {}

            const core::NodePtr resolveElement(const core::NodePtr& ptr) {
				core::IRBuilder builder(manager);

                // first descent recursively
                core::NodePtr res = ptr->substitute(manager, *this);



				// handle particular case:
				// decl _ocl_global<...> v201 = var(_ocl_unwrap_global(v70)); ==>
				// decl _ocl_global<...> v201 = _ocl_wrap_global(var(_ocl_unwrap_global(v70)));
				insieme::core::pattern::TreePattern declWrapGlobal = tr::aT(irp::literal("_ocl_unwrap_global"));
				if (res->getNodeType() == core::NT_DeclarationStmt){
					auto&& var = res.as<core::DeclarationStmtPtr>()->getVariable();
					auto&& init = res.as<core::DeclarationStmtPtr>()->getInitialization();
					auto&& match = declWrapGlobal.matchPointer(init);
					if (extensions.isGlobalType(var->getType()) && match){
						return builder.declarationStmt(var, builder.callExpr(extensions.wrapGlobal, init));
					}
					return res;
				}

				// check whether it is a call to an external literal
				if (res->getNodeType() != core::NT_CallExpr) {
					return res;
				}

				core::CallExprPtr call = res.as<core::CallExprPtr>();
				core::ExpressionPtr fun = call->getFunctionExpr();

				vector<core::ExpressionPtr> newArgs;
				for_each(call->getArguments(), [&](const core::ExpressionPtr& cur) {
					auto&& fit = std::find(varVec.begin(), varVec.end(), cur);
					if (fit != varVec.end()){
						newArgs.push_back(extensions.unWrapExpr(cur));}
					else
						newArgs.push_back(cur);
				});

				return builder.callExpr(fun, newArgs);
			}
		};

        core::StatementPtr unwrapTypes(const core::StatementPtr body, const std::vector<core::VariablePtr>& varVec = std::vector<core::VariablePtr>()) {
            return TypeUnwrapper(body->getNodeManager(), varVec).map(body);
		}


		// --------------------------------------------------------------------------------------------------------------
		//
		// --------------------------------------------------------------------------------------------------------------



		class TypeWrapper : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;
			const Extensions& extensions;
			const std::string outFilePath;

		public:

			TypeWrapper(core::NodeManager& manager, const std::string outFilePath) :
				manager(manager),  extensions(manager.getLangExtension<Extensions>()), outFilePath(outFilePath) {}

			const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				core::IRBuilder builder(manager);
				auto& basic = manager.getLangBasic();
				auto& ext = manager.getLangExtension<Extensions>();
				auto& hostExt = manager.getLangExtension<ocl_host::Extensions>();

				// perform conversion in post-order
				core::NodePtr res = ptr->substitute(manager, *this);
				// check whether this is the call to a kernel
				if (res->getNodeType() == core::NT_CallExpr) {
					core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(res);
					core::ExpressionPtr fun = call->getFunctionExpr();

					// if this is a call to the kernel ...
					if (core::analysis::isCallOf(fun, extensions.kernelWrapper)) {
						// ... drop final two arguments
						const core::ExpressionList& args = call->getArguments();
						assert_ge(args.size(), 2) << "Call should have 2 or more arguments";
						core::ExpressionList newArgs = core::ExpressionList(args.begin(), args.end()-2);

						core::FunctionTypePtr funType = static_pointer_cast<const core::FunctionType>(fun->getType());
						const core::TypeList& paramTypes = funType->getParameterTypes()->getElements();
						assert_eq(paramTypes.size(), newArgs.size());

						// add type wrappers where necessary for the parameters of the kernel
						for(std::size_t i = 0; i < paramTypes.size(); i++) {
							if (extensions.isGlobalType(paramTypes[i])) {
								newArgs[i] = builder.callExpr(paramTypes[i], extensions.wrapGlobal, newArgs[i]);
							} else if (extensions.isLocalType(paramTypes[i])) {
								newArgs[i] = builder.callExpr(paramTypes[i], extensions.wrapLocal, newArgs[i]);
							} else if (extensions.isConstType(paramTypes[i])) {
								newArgs[i] = builder.callExpr(paramTypes[i], extensions.wrapConst, newArgs[i]);
							}
						}


						core::CallExprPtr kernel_args = builder.callExpr(basic.getVarList(), basic.getVarlistPack(), builder.tupleExpr(newArgs));

						// building the offset 3 elements array
						core::ExpressionPtr zero = builder.literal(basic.getUInt8(), "0").as<core::ExpressionPtr>();
						core::ExpressionPtr offset = builder.refVar(builder.vectorExpr(toVector(zero, zero, zero)));

						return builder.callExpr(basic.getUnit(), hostExt.callKernel, toVector(fun, offset, *(args.end()-2), *(args.end()-1), kernel_args));
					}
				}

				// only interested in lambda expressions
				if (res->getNodeType() != core::NT_LambdaExpr) {
					return res;
				}

				// extract lambda
				const core::LambdaExprPtr& kernel = static_pointer_cast<const core::LambdaExpr>(res);

				if (!isKernel(kernel)) {
					return res;
				}

				// create kernel function
				core::StatementPtr core = getKernelCore(kernel);

				// - get_num_groups => first declaration
				// - get_*_size => last two parameters

				LOG(INFO) << "Core Before: " << core::printer::PrettyPrinter(core, core::printer::PrettyPrinter::OPTIONS_DETAIL);
				LOG(INFO) << "Errors Before: " << core::checks::check(core, core::checks::getFullCheck());

				// ------------------ Update variable names within kernel core -------------------

				// exchange variables within core
				// Map first: internal_variable - second: external_variable in the kernel
				VariableMap&& map = mapBodyVars(kernel);
				//std::cout << "MAP " << map << std::endl;

				// collect map mapping external variables to their address spaces (local, global ...)
				AddressSpaceMap varMap = getAddressSpaces(core.as<core::LambdaExprPtr>(), map);
				//std::cout << "VARMAP " << varMap << std::endl;

				// Separate variable map into local variable declarations and parameters
				VariableMap localVars;
				core::NodeMap  parameters;
				core::NodeMap  arguments;
				for_each(map, [&](const VariableMap::value_type& cur) {
					if (cur.second->getNodeType() == core::NT_Variable) {
                        			core::VariablePtr var = cur.second.as<core::VariablePtr>();
						if(core::annotations::hasAttachedName(cur.first)) {
							core::annotations::attachName(var, core::annotations::getAttachedName(cur.first));
						}
						auto pos = varMap.find(var);
						if (pos != varMap.end()) {
							// create a new variable with the right address space
							var = builder.variable(extensions.getType(pos->second, var->getType()), cur.first.as<core::VariablePtr>()->getId());
							// add the unwrap in front of the variable to maintain the correct semantic
							parameters.insert(std::make_pair(cur.first, extensions.unWrapExpr(var)));
							arguments.insert(std::make_pair(cur.first, var));
						} else {
							parameters.insert(std::make_pair(cur.first, var));
							arguments.insert(std::make_pair(cur.first, var));
						}
					} else {
						// we are in the case of local variables, for example:
						// cur.first  => v86
						// cur.second => ref.var(undefined(vector<int<4>,258>

						// build a local variable
						core::VariablePtr var = builder.variable(extensions.getType(AddressSpace::LOCAL, cur.first->getType()), cur.first->getId());
						// create a wrapper expression to use in the declaration later..
						core::ExpressionPtr value = extensions.wrapExpr(AddressSpace::LOCAL, cur.second);
						localVars.insert(std::make_pair(var, value));

						parameters.insert(std::make_pair(cur.first, var));
					}
				});


				//change all var uses in the body of the lambda
				core = static_pointer_cast<const core::Statement>(core::transform::replaceAll(manager, core, parameters,
					[&](const core::NodePtr& ptr){
						if(ptr.isa<core::ParametersPtr>()) return core::transform::ReplaceAction::Prune;
						return core::transform::ReplaceAction::Process;
				}));
				//modify the argument types of the lambda
				core = static_pointer_cast<const core::Statement>(core::transform::replaceAll(manager, core, arguments,
					[&](const core::NodePtr& ptr){
						//skip the body
						if(ptr.isa<core::CompoundStmtPtr>()) return core::transform::ReplaceAction::Prune;
						return core::transform::ReplaceAction::Process;
				}));

				//lets build a new function type and replace it
				core::TypeList typeList;
				core::VariableList varList;
				auto paramList = core.as<core::LambdaExprPtr>()->getParameterList();
				for(auto it = paramList->begin(); it != paramList->end()-2; ++it) {
					typeList.push_back(it->getType());
					varList.push_back((*it).as<core::VariablePtr>());
				}
				auto oldFunType = core.as<core::LambdaExprPtr>()->getFunctionType();
				auto newFunType = builder.functionType(typeList, builder.getLangBasic().getUnit());
				core = static_pointer_cast<const core::Statement>(core::transform::replaceAll(manager, 
					core, oldFunType, newFunType, core::transform::globalReplacement));


				// float* gl = &g[0] where g is global => __global float* gl
				utils::map::PointerMap<core::VariablePtr, core::VariablePtr> varToGlobalize;
				std::vector<core::VariablePtr> varVec;
				insieme::core::pattern::TreePattern declUnwrapGlobal = tr::aT(irp::literal("_ocl_unwrap_global"));
				visitDepthFirst(core, [&](const core::DeclarationStmtPtr& decl) {
				    auto&& var = decl->getVariable();
				    auto&& init = decl->getInitialization();

				    const core::TypePtr elementType = core::analysis::getReferencedType(core::analysis::getReferencedType(var->getType()));
				    if (elementType && elementType->getNodeType() == core::NT_ArrayType){
				        auto&& match = declUnwrapGlobal.matchPointer(init);
				        if (match) {
				            core::VariablePtr newVar = builder.variable(extensions.getType(AddressSpace::GLOBAL, var->getType()));
				            varToGlobalize.insert(std::make_pair(var, newVar));
				            varVec.push_back(newVar);
				        }
				    }
				});

				core = core::transform::replaceVarsRecursiveGen(manager, core, varToGlobalize, false, core::transform::no_type_fixes);
				//std::cout << "CORE OUTPUT 2: " << core::printer::PrettyPrinter(core)  << std::endl;

				// add locals ...
				if (!localVars.empty()) {
					vector<core::StatementPtr> stmts;
					for_each(localVars, [&](const VariableMap::value_type& cur) {
						stmts.push_back(builder.declarationStmt(cur.first, cur.second));
						varVec.push_back(cur.first);
					});
					stmts.push_back(core);
					core = builder.compoundStmt(stmts);
				}

				core = unwrapTypes(core, varVec);



				// ------------------------- Replace build-in literals ---------------------------
				core::VariablePtr globalSizeVar = *(core.as<core::LambdaExprPtr>()->getParameterList().end()-2);
				core::VariablePtr localSizeVar = *(core.as<core::LambdaExprPtr>()->getParameterList().end()-1);
				core::VariablePtr numGroupVar =
						static_pointer_cast<const core::DeclarationStmt>(
						static_pointer_cast<const core::CompoundStmt>(kernel->getBody())->getStatements()[0])->getVariable();
				BuildInReplacer replacer(manager, globalSizeVar, localSizeVar, numGroupVar);
				core = static_pointer_cast<const core::Statement>(core->substitute(manager, replacer));
				//update the core. remove two params at the end
				core::ParametersPtr newParams = builder.parameters(varList);
				core = static_pointer_cast<const core::Statement>(core::transform::replaceAll(
					manager, core, paramList, newParams, core::transform::globalReplacement));

				// ------------------------- Replace IR convert version to convert builtin --------------
				//   decl ref<vector<uint<1>,4>> v34 = ( var(fun(vector<real<4>,4> v46, type<uint<1>> v47){
				//	 decl ref<vector<uint<1>,4>> v48 = ( var(undefined(type<vector<uint<1>,4>>)));
				//	 for(decl uint<8> v49 = 0 .. CAST<uint<8>>(4) : 1) { ((v48&[v49]) := CAST<uint<1>>((v46[v49]))); };
				//	 return ( *v48);
				//}(( *v33), type<uint<1>>)));
				// ==> IR: decl ref<vector<uint<1>,4>> v34 = ref.var(convert(ref.deref(v33)))

				utils::map::PointerMap<core::NodePtr, core::NodePtr> nodeMap;
				// TODO: improve pattern
				insieme::core::pattern::TreePattern convertPattern = irp::callExpr(tr::any, insieme::core::pattern::aT(irp::forStmt()),
					tr::var("expr") << irp::literal(irp::genericType("type", *tr::any, *tr::any), tr::any)); // list 2 arguments

				visitDepthFirst(core, [&](const core::CallExprPtr& call) {
					auto&& match = convertPattern.matchPointer(call);
					if (match) {
						core::ExpressionPtr exp = static_pointer_cast<const core::Expression>(match->getVarBinding("expr").getValue());
						core::CallExprPtr convert =  builder.callExpr(call->getType(), ext.convertBuiltin, exp, builder.getTypeLiteral(call->getType()));
						nodeMap.insert(std::make_pair(call, convert));
					}
				});
				core = static_pointer_cast<const core::Statement>(
					core::transform::replaceAll(manager, core, nodeMap, core::transform::globalReplacement));

				//LOG(INFO) << "Replace Vector -> Errors: " << core::check(core, core::checks::getFullCheck());
				//std::cout << "Core After: " << core::printer::PrettyPrinter(core) << std::endl;

				// ------------------ Create resulting lambda expression -------------------

				// build parameter list
				vector<core::VariablePtr> params = vector<core::VariablePtr>(core.as<core::LambdaExprPtr>()->getParameterList().begin(), 
						core.as<core::LambdaExprPtr>()->getParameterList().end());

				for_each(params, [&](core::VariablePtr& cur) {
					core::VariablePtr res = builder.variable(extensions.getType(varMap[cur], cur->getType()), cur->getId());
					if(core::annotations::hasAttachedName(cur)) {
                        			core::annotations::attachName(res, core::annotations::getAttachedName(cur));
					}
					cur = res;
				});

				vector<core::TypePtr> paramTypes = ::transform(params, [](const core::VariablePtr& cur) { return cur->getType(); });

				core::FunctionTypePtr kernelType = builder.functionType(paramTypes, basic.getUnit());
				core::LambdaExprPtr newKernel = builder.lambdaExpr(kernelType, params, core.as<core::LambdaExprPtr>()->getBody());


				if (core::annotations::hasAttachedName(kernel->getLambda())) {
					auto name = core::annotations::getAttachedName(kernel->getLambda());
		                    	core::annotations::attachName(newKernel->getLambda(),name);
                		}
				res = builder.callExpr(kernelType, extensions.kernelWrapper, toVector<core::ExpressionPtr>(newKernel));

				// dump the kernel if outFilePath is set
				if(outFilePath.size() > 0) {
					std::ofstream out(outFilePath.c_str());
					assert_true(out.is_open()) << "Cannot open file to write binary dump of kernel";

					core::dump::binary::dumpIR(out, res);

					out.close();
				}

				LOG(INFO) << "New Kernel: " << core::printer::PrettyPrinter(res);
				LOG(INFO) << "Errors After Kernel preprocess: " << core::checks::check(newKernel, core::checks::getFullCheck());
				auto err = core::checks::check(newKernel, core::checks::getFullCheck());;
				assert_true(err.empty()) << "YEAHHH... You broke the OpenCL Backend!!!";
				return res;
			}
		};


	}


	core::NodePtr KernelPreprocessor::process(const backend::Converter& converter, const core::NodePtr& code) {
		return process(converter.getNodeManager(), code);
	}

	core::NodePtr KernelPreprocessor::process(core::NodeManager& manager, const core::NodePtr& code) {
		// the converter does the magic
		TypeWrapper wrapper(manager, outFilePath);
		core::NodePtr kernel = wrapper.map(code);
		return kernel;
	}


} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
