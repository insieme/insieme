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
			OpenCLKernelBackendPtr backend = OpenCLKernelBackend::getDefault();
			TargetCodePtr target = backend->convert(oclExpr);
			// dump the target code as string and put into IR lit
			return builder.stringLit(toString(*target));
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
		// paranoia checks
		assert_true(ndrange->getNodeType() == core::NT_LambdaExpr) << "ndrange must be a lambdaExpr: " << dumpColor(ndrange);
		// @TODO: ndrange must be LambdaExprPtr & requirements is the same
		// @TODO: optionals must be basis types
		core::IRBuilder builder(manager);
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// this call instructs the irt to execute the kernel @id
		return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getExecuteKernel(), builder.uintLit(id), ndrange,
								core::encoder::toIR<core::ExpressionList, core::encoder::DirectExprListConverter>(manager, requirements),
								builder.pack(optionals));
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
	
	namespace {
		class InlineAssignmentsNodeMapping : public core::transform::CachedNodeMapping {
			core::NodeManager& manager;
			core::IRBuilder builder;
			bool includeEffortEstimation;

		public:
			InlineAssignmentsNodeMapping(core::NodeManager& manager) : manager(manager), builder(manager) {}

			const core::NodePtr resolveElement(const core::NodePtr& ptr) override {
				#if 0
				/*
				 * the task of this function is to inline the following code:
				 * LANG_EXT_DERIVED(CStyleAssignment, "(lhs : ref<'a,f,'b>, rhs : 'a) -> 'a { lhs = rhs; return *lhs; }")
				 * LANG_EXT_DERIVED(CxxStyleAssignment, "(lhs : ref<'a,f,'b,'c>, rhs : 'a) -> ref<'a,f,'b,'c> { lhs = rhs; return lhs; }")
				 */
				auto var = core::pattern::irp::atom(builder.typeVariable("a"));
				auto rhs = core::pattern::irp::variable(var);
				auto lhs = core::pattern::irp::variable(core::pattern::irp::refType(var));
				// now construct the lambda pattern which represents the actual c/cpp_style_assignment
				auto pattern = core::pattern::irp::lambda(
					core::pattern::any, 				// return type
					core::pattern::anyList, 			// arguments
					core::pattern::aT( 					// body
						core::pattern::irp::assignment(),
						core::pattern::irp::returnStmt(core::pattern::any)
						));
				
				core::NodeMap replacements;
				// visit all call expressions within the node and try to replace them with inlined versions
				core::visitDepthFirstOncePrunable(lambdaExpr->getBody(), [&](const core::CallExprPtr& callExpr) {
					// simple case, the pattern matches right along
					if (auto lambdaExpr = callExpr->getFunctionExpr().isa<core::LambdaExprPtr>()) {
						if (pattern.match(lambdaExpr)) {
							auto newNode = core::transform::inlineCode(manager, callExpr);
							LOG(INFO) << "OpenCL::InlineAssignmentsStep@pattern:" << std::endl
									  << "original: " << dumpColor(callExpr)
									  << "inlined: " << dumpColor(newNode);
							replacements[callExpr] = newNode;
						}
						
						// let the visitor know that we do not want to visit children of this call
						return true;
					}
					
					return true;
				});
				// fast-path
				if (replacements.empty()) return node;
				auto newNode = builder.lambdaExpr(manager.getLangBasic().getUnit(), 
					lambdaExpr->getParameterList(), core::transform::replaceAllGen(manager, lambdaExpr->getBody(), replacements));
				
				LOG(INFO) << "OpenCL::InlineAssignmentsStep:"
						  << "input: " << dumpColor(node)
						  << "output: " << dumpColor(newNode);
				return newNode;
				#endif
				
				// right now we do an identity mapping
				return ptr;
			}
		};
	}
	
	core::NodePtr InlineAssignmentsStep::process(const Converter& converter, const core::NodePtr& node) {		
		core::LambdaExprPtr lambdaExpr = node.isa<LambdaExprPtr>();
		if (!lambdaExpr) return node;
	
		InlineAssignmentsNodeMapping mapping(manager); 
		return mapping.resolveElement(node);
	}

	core::LambdaExprPtr toOcl(const Converter& converter, core::NodeManager& manager, unsigned int& id, const core::CallExprPtr& callExpr) {
		core::IRBuilder builder(manager);
		// grab a ptr to the enclosed lamda -- right now no transformations applied
		core::LambdaExprPtr lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
		// get the requirements which are encoded as annotations
		const VariableRequirementList& requirements = analysis::getVariableRequirements(manager, callExpr);
		// transform the requirements into functions
		core::ExpressionList rq;
		for (const auto& requirement : requirements)
			rq.push_back(toIR(manager, requirement));
		// transform the NDRange(s) into function(s)
		core::ExpressionPtr nd = toIR(manager, makeDefaultNDRange(manager));
		// empty list for the additional arguments
		core::ExpressionList va;
		
		// all modifications applied to the lambda (which will ultimately yield OlcIR) are modeled as preProcessors
		auto processor = makePreProcessorSequence(
			makePreProcessor<FlattenVariableIndirectionStep>(boost::ref(manager))/*,
			makePreProcessor<InlineAssignmentsStep>(boost::ref(manager))*/);
		
		core::StatementList body;
		// first of all, we need to register the kernel itself
		body.push_back(buildRegisterKernel(manager, id, processor->process(converter, lambdaExpr).as<LambdaExprPtr>()));
		body.push_back(buildExecuteKernel(manager, id, nd, rq, va));
		// now the body is populated with all required IR constructs, build the wrapper
		return builder.lambdaExpr(lambdaExpr->getFunctionType(), lambdaExpr->getParameterList(), builder.compoundStmt(body));
	}
}
}
}
}
