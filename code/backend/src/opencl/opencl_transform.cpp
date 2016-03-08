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

#include "insieme/backend/opencl/opencl_transform.h"
#include "insieme/backend/opencl/opencl_entities.h"
#include "insieme/backend/opencl/opencl_backend.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_analysis.h"
#include "insieme/backend/opencl/opencl_extension.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/inline.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/ir_builder.h"

#include "insieme/backend/runtime/runtime_extension.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace transform {
	
	using namespace insieme::annotations::opencl;
	
	namespace {
		core::LiteralPtr toKernelLiteral(core::NodeManager& manager, const core::LambdaExprPtr& oclExpr) {
			core::IRBuilder builder(manager);
			// obtain the kernel backend which transforms oclExpr into a stringLit
			KernelBackendPtr backend = KernelBackend::getDefault();
			TargetCodePtr target = backend->convert(oclExpr);

			std::string str;
			// in case it is CCode we print it differently -- this shall be the general case tho!
			if (auto ccode = std::dynamic_pointer_cast<c_ast::CCode>(target)) {
				std::stringstream ss;
				for (const c_ast::CodeFragmentPtr& cur : ccode->getFragments()) { ss << *cur; }
				str = ss.str();
			} else {
				str = toString(*target);
			}
			// dump the target code as string and put into IR lit
			return builder.stringLit(str);
		}

		core::ExpressionPtr wrapLazy(core::NodeManager& manager, const core::ExpressionPtr& expr) {
			if (expr->getNodeType() == core::NT_LambdaExpr) return expr.as<core::LambdaExprPtr>();
			// in this case we take use of materialization to wrap it
			core::IRBuilder builder(manager);
			return builder.wrapLazy(expr);
		}
		
		NDRangePtr makeDefaultNDRange(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			NDRangePtr ndrange = std::make_shared<NDRange>();
			ndrange->setWorkDim(builder.uintLit(1));
			ndrange->addGlobalWorkSize(builder.uintLit(1));
			ndrange->addLocalWorkSize(builder.uintLit(1));
			return ndrange;
		}
	}
	
	core::CallExprPtr outline(core::NodeManager& manager, const core::StatementPtr& stmt) {
		// check whether it is allowed
		assert_true(opencl::analysis::isOutlineAble(manager, stmt)) << "cannot outline given code: " << dumpColor(stmt);
		// invoke the standard outline procedure
		core::CallExprPtr callExpr = core::transform::outline(manager, stmt);
		core::LambdaExprPtr lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
		// migrate the annotations from stmt to lambda
		core::transform::utils::migrateAnnotations(stmt, lambdaExpr);
		return callExpr;
	}

	core::CallExprPtr buildRegisterKernel(core::NodeManager& manager, unsigned int& id, const core::LambdaExprPtr& oclExpr) {
		core::IRBuilder builder(manager);
		// grab a reference to the runtime & opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// most important step here, transform the oclExpr into a literal
		core::LiteralPtr literal = toKernelLiteral(manager, oclExpr);
		// generate a new unique id for this kernel
		id = KernelTable::getNextUnique();
		// use the default IRBuilder to generate the callExpr
		return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getRegisterKernel(),
								builder.uintLit(id), lang::buildPtrFromArray(literal));
	}

	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const core::ExpressionPtr& ndrange,
										 const core::ExpressionList& requirements, const core::ExpressionList& optionals) {
		core::IRBuilder builder(manager);
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// this call instructs the irt to execute the kernel @id
		return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getExecuteKernel(), builder.uintLit(id), ndrange,
								core::encoder::toIR<core::ExpressionList, core::encoder::DirectExprListConverter>(manager, requirements),
								builder.pack(optionals));
	}

	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const StepContext& sc, const CallContext& cc) {
		// a valid ndrange must be present, the rest is optional
		assert_true(cc.getNDRange()) << "NDRange must not be null!";
		// put together all requirements
		core::ExpressionList requirements;
		if (!cc.getOverrideRequirements().empty()) {
			// a step has produced custom requirement functions, use them instead
			requirements = cc.getOverrideRequirements();
			assert_true(requirements.size() == sc.getDefaultRequirements().size())
				<< "number of overridden requirements must match default ones!";

			// @TODO: implement analysis to assure the signature of overrides is correct
			//		  e.g. analysis::isValidRequirement()
		} else {
			// fallback are the default non-modified requirements, transform into IR and push
			for (const auto& requirement : sc.getDefaultRequirements())
				requirements.push_back(toIR(manager, requirement));
		}
		// put together all optionals
		core::ExpressionList optionals;
		if (!cc.getOptionals().empty()) {
			core::IRBuilder builder(manager);
			// each optional is represented as tuple (size,expr)
			for (const auto& optional : cc.getOptionals()) {
				// in case the optional is a function or a closure -- which must be in the form of
				// (...) -> 'a -- the opconverter will take care to evaluate it in an eager manner
				auto unit = manager.getLangBasic().getUnit();
				auto type = unit;
				switch (optional->getNodeType()) {
				case core::NT_Literal:
				case core::NT_Variable:
						type = optional->getType();
						break;
				case core::NT_CallExpr:
					{
						auto callExpr = optional.as<core::CallExprPtr>();
						// use returnType deduction for this purpose
						type = core::types::deduceReturnType(callExpr->getFunctionExpr()->getType().as<core::FunctionTypePtr>(),
							core::extractTypes(callExpr->getArguments()));
						break;
					}
				case core::NT_BindExpr:
					{
						auto bindExpr = optional.as<core::BindExprPtr>();
						assert_true(bindExpr->getParameters().empty())
							<< "optional argument modeled as bindExpr must not expect additional parameters";
						// also use returnType deduction but use bound parameters
						auto callExpr = bindExpr->getCall();
						type = core::types::deduceReturnType(callExpr->getFunctionExpr()->getType().as<core::FunctionTypePtr>(),
							core::extractTypes(callExpr->getArguments()));
						break;
					}
				default:
						// fall through and trigger error handling
						break;
				}
				assert_ne(type, unit) << "failed to deduce type of optional argument";
				// at this point we can finally construct the tuple
				optionals.push_back(builder.callExpr(manager.getLangBasic().getSizeof(),
													 builder.getTypeLiteral(type)));
				optionals.push_back(optional);
			}
		}
		// done .. execute the helper one level deeper
		return buildExecuteKernel(manager, id, toIR(manager, cc.getNDRange()), requirements, optionals);
	}

	core::StatementList buildExecuteGraph(core::NodeManager& manager, unsigned int id, const StepContext& sc) {
		core::StatementList result;

		const auto& graph = sc.getCallGraph();
		assert_true(graph.getNumVertices() > 0) << "execution graph must consist of at least one vertex";
		// right now: simple implementation which sequentially executes all calls
		for (auto it = graph.vertexBegin(); it != graph.vertexEnd(); ++it) {
			auto callExpr = buildExecuteKernel(manager, id, sc, **it);
			// for the sake of debugging -- print IR of generated execute
			LOG(INFO) << "generated execute call: " << dumpColor(callExpr);
			result.push_back(callExpr);
		}

		return result;
	}

	core::LambdaExprPtr toIR(core::NodeManager& manager, const NDRangePtr& ndrange) {
		core::IRBuilder builder(manager);
		// grab a reference to the runtime & opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		auto& runExt = manager.getLangExtension<runtime::RuntimeExtension>();
		
		core::VariableList params;
		params.push_back(core::Variable::get(manager, core::lang::buildRefType(runExt.getWorkItemType())));
		// body is simply a ref_new_init
		core::StatementList body;
		body.push_back(builder.returnStmt(NDRange::encode(manager, ndrange)));
		// and finally ... THE lambda ladies and gentleman
		return builder.lambdaExpr(builder.functionType(core::extractTypes(params), oclExt.getNDRange(), core::FK_PLAIN), params, builder.compoundStmt(body));
	}
	
	core::LambdaExprPtr toIR(core::NodeManager& manager, const VariableRequirementPtr& var) {
		/*
		the requirement is transformed into the following 'lambda'
		fun001 = function (wi: ref<irt_wi>, nd: ref<opencl_ndrange>, arg: ref<uint<4>>) -> opencl_data_requirement {
			return opencl_make_data_requirement(1u, 2u, fun002);
		}
		fun002 = function (wi: ref<irt_wi>, nd: ref<opencl_ndrange>, arg: ref<uint<4>>, dim: ref<uint<4>>) -> opencl_data_range {
			switch (dim) {
			case 0: return opencl_make_data_range(1000u, 0u, 1000u);
			case 1: return opencl_make_data_range(1000u, 0u, 1000u);
			case 2: return opencl_make_data_range(1000u, 0u, 1000u);
			}
		}
		*/
		core::IRBuilder builder(manager);
		// grab a reference to the runtime & opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		auto& runExt = manager.getLangExtension<runtime::RuntimeExtension>();
		// copy over the access mode
		DataRequirementPtr requirement = std::make_shared<DataRequirement>();
		switch (var->getAccessMode()) {
		case VariableRequirement::AccessMode::RO:
				requirement->setAccessMode(DataRequirement::AccessMode::RO);
				break;
		case VariableRequirement::AccessMode::WO:
				requirement->setAccessMode(DataRequirement::AccessMode::WO);
				break;		
		case VariableRequirement::AccessMode::RW:
				requirement->setAccessMode(DataRequirement::AccessMode::RW);
				break;
		}
		requirement->setType(var->getVar()->getType());

		core::VariableList params;
		params.push_back(core::Variable::get(manager, core::lang::buildRefType(core::lang::buildRefType(runExt.getWorkItemType()))));
		params.push_back(core::Variable::get(manager, core::lang::buildRefType(core::lang::buildRefType(oclExt.getNDRange()))));
		params.push_back(core::Variable::get(manager, core::lang::buildRefType(manager.getLangBasic().getUInt4())));
		params.push_back(core::Variable::get(manager, core::lang::buildRefType(manager.getLangBasic().getUInt4())));

		core::StatementList body;
		// encode the range (in this case 1D)
		DataRangePtr range = DataRange::get(manager, var->getSize(), var->getStart(), var->getEnd());
		body.push_back(builder.returnStmt(DataRange::encode(manager, range)));
		// ok, now we can generate the "inner" lambda
		core::LambdaExprPtr rangeExpr = builder.lambdaExpr(oclExt.getDataRange(), params, builder.compoundStmt(body));

		// prepare for the outer lamdba
		body.pop_back();
		params.pop_back();
		// as a VariableRequirement is used for 1D, we set the numRanges hard-coded
		requirement->setNumRanges(builder.uintLit(1));
		// encode the requirement and build the "outer" lambda
		requirement->setRangeExpr(rangeExpr);
		body.push_back(builder.returnStmt(DataRequirement::encode(manager, requirement)));
		// and finally ... THE lambda ladies and gentleman
		return builder.lambdaExpr(oclExt.getDataRequirement(), params, builder.compoundStmt(body));
	}
	
	core::NodePtr FlattenVariableIndirectionStep::process(const Converter& converter, const core::NodePtr& code) {
		core::LambdaExprPtr lambdaExpr = code.isa<LambdaExprPtr>();
		if (!lambdaExpr) return code;
	
		core::IRBuilder builder(manager);
		// used to modify the body and header
		core::NodeMap bodyReplacements;
		core::VariableMap annoReplacements;
		
		core::VariableList parameter;
		for_each(lambdaExpr->getParameterList(), [&](const core::VariablePtr& cur) {
			// default param
			parameter.push_back(cur);
			if (!core::lang::isReference(cur->getType())) return;
			// grab the inner type as this would be our replacements
			auto param = builder.variable(analysis::getElementType(cur->getType()));
			if (!core::lang::isReference(param->getType())) return;
			// ok the element must be a pointer, otherwise we do not need to replace
			if (!core::lang::isPointer(analysis::getElementType(param->getType()))) return;
			
			// excellent .. lets replace all derefs with the variable in question
			bodyReplacements[builder.deref(cur)] = param;
			parameter.back() = param;
			// this is required to fix up the migrated annotations
			annoReplacements[cur] = param;
		});
		// fast-path?
		if (bodyReplacements.empty()) return lambdaExpr;
		// replace all usages in the original body
		core::LambdaExprPtr newLambdaExpr = builder.lambdaExpr(manager.getLangBasic().getUnit(), 
			parameter, core::transform::replaceAllGen(manager, lambdaExpr->getBody(), bodyReplacements));
		// in the end migrate the annotations and fix them up
		core::transform::utils::migrateAnnotations(lambdaExpr, newLambdaExpr);
		return core::transform::replaceVarsGen(manager, newLambdaExpr, annoReplacements);
	}

	core::NodePtr InlineAssignmentsStep::process(const Converter& converter, const core::NodePtr& node) {		
		core::LambdaExprPtr lambdaExpr = node.isa<LambdaExprPtr>();
		if (!lambdaExpr) return node;

		core::IRBuilder builder(manager);
		// auto var = core::pattern::irp::atom(builder.typeVariable("a"));
		auto rhs = core::pattern::irp::variable(core::pattern::any);
		auto lhs = core::pattern::irp::variable(core::pattern::irp::refType(core::pattern::any));
		// now construct the lambda pattern which represents the actual c/cpp_style_assignment
		auto pattern = core::pattern::irp::lambda(
			core::pattern::any, 				// return type
			core::pattern::empty << lhs << rhs,	// arguments
			core::pattern::aT( 					// body
				core::pattern::irp::assignment(),
				core::pattern::irp::returnStmt(core::pattern::any)
				));

		core::NodeMap replacements;
		core::visitDepthFirstOnce(core::NodeAddress(lambdaExpr->getBody()), [&](const core::NodeAddress& addr) {
			const core::NodePtr& target = addr.getAddressedNode();
			// non-calls are not important
			if (target->getNodeType() != core::NT_CallExpr) return;

			auto callExpr = target.as<core::CallExprPtr>();
			// if we call something else than a lambda we are not interested in it as well
			if (auto callee = callExpr->getFunctionExpr().isa<core::LambdaExprPtr>()) {
				// do not introduce a local var to hold the evaluated value -- only inline straight assignments
				if (!addr.isRoot() && addr.getParentAddress().getAddressedNode()->getNodeType() == core::NT_CallExpr) return;
				// if our parent is not a call & the pattern matches the replacement is valid
				if (!pattern.match(callee->getLambda())) return;

				replacements[target] = builder.assign(callExpr->getArgument(0), callExpr->getArgument(1));
			}
		});
		// fast-path
		if (replacements.empty()) return node;

		core::LambdaExprPtr newLambdaExpr= builder.lambdaExpr(manager.getLangBasic().getUnit(),
			lambdaExpr->getParameterList(), core::transform::replaceAllGen(manager, lambdaExpr->getBody(), replacements));
		// migrate the annotations -- but no need to fixup as only hthe body has changed
		core::transform::utils::migrateAnnotations(lambdaExpr, newLambdaExpr);
		return newLambdaExpr;
	}
	
	core::NodePtr CallIntroducerStep::process(const Converter& converter, const core::NodePtr& node) {
		auto callContext = std::make_shared<CallContext>();
		// introduce a single default clEnqueueTask call
		callContext->setNDRange(makeDefaultNDRange(manager));
		context.getCallGraph().addVertex(callContext);
		return node;
	}

	core::NodePtr KernelTypeStep::process(const Converter& converter, const core::NodePtr& code) {
		#if 0
		core::LambdaExprPtr lambdaExpr = code.isa<LambdaExprPtr>();
		if (!lambdaExpr) return code;

		core::IRBuilder builder(manager);
		core::StatementList body;

		for_each(lambdaExpr->getParameterList(), [&](const core::VariablePtr& cur) {
			body.push_back(builder.markerExpr(cur, cur->getId()));
		});

		body.insert(body.end(), lambdaExpr->getBody().begin(), lambdaExpr->getBody().end());
		core::LambdaExprPtr newLambdaExpr = builder.lambdaExpr(manager.getLangBasic().getUnit(), lambdaExpr->getParameterList(), builder.compoundStmt(body));

		LOG(INFO) << "lambda with kernel types: " << dumpColor(newLambdaExpr);
		#endif
		return code;
	}

	core::NodePtr IntegrityCheckStep::process(const Converter& converter, const core::NodePtr& code) {
		LOG(INFO) << "generated kernel code:";
		LOG(INFO) << dumpColor(code);

		core::checks::MessageList messages = core::checks::check(code);
		if (!messages.empty()) {
			std::stringstream ss;
			messages.printTo(ss);
			LOG(ERROR) << "integrity checks for OpenCL Kernel code has failed:";
			LOG(ERROR) << ss.str();
			
			assert_fail() << "see messages above!";
		}
		return code;
	}

	core::LambdaExprPtr toOcl(const Converter& converter, core::NodeManager& manager, unsigned int& id, const core::CallExprPtr& callExpr) {
		core::IRBuilder builder(manager);
		// grab a ptr to the enclosed lamda -- right now no transformations applied
		core::LambdaExprPtr lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
		// context which is usable among all steps
		StepContext context;
		// grab and set the default requirements for all variables
		context.setDefaultRequirements(analysis::getVariableRequirements(manager, callExpr));
		// all modifications applied to the lambda (which will ultimately yield OlcIR) are modeled as preProcessors
		auto processor = makePreProcessorSequence(
			getBasicPreProcessorSequence(),
			makePreProcessor<FlattenVariableIndirectionStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<InlineAssignmentsStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<CallIntroducerStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<KernelTypeStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<IntegrityCheckStep>(boost::ref(manager), boost::ref(context)));

		core::StatementList body;
		// first of all, we need to register the kernel itself
		auto registerCall = buildRegisterKernel(manager, id, processor->process(converter, lambdaExpr).as<LambdaExprPtr>());
		// as the processor has yield a callGraph build it now
		body = buildExecuteGraph(manager, id, context);
		body.push_back(registerCall);
		// now the body is populated with all required IR constructs, build the wrapper
		return builder.lambdaExpr(lambdaExpr->getFunctionType(), lambdaExpr->getParameterList(), builder.compoundStmt(body));
	}
}
}
}
}
