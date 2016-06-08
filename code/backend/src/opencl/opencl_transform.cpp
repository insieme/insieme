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
#include "insieme/backend/opencl/opencl_postprocessor.h"

#include "insieme/annotations/meta_info/meta_infos.h"
#include "insieme/annotations/opencl/opencl_annotations.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/inline.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/ir_builder.h"

#include "insieme/backend/runtime/runtime_extension.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/range.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace transform {

	using namespace insieme::annotations::opencl;
	using namespace insieme::annotations::opencl_ns;

	namespace {
		core::LiteralPtr toKernelLiteral(core::NodeManager& manager, const StepContext& sc, const core::LambdaExprPtr& oclExpr) {
			core::IRBuilder builder(manager);
			// obtain the kernel backend which transforms oclExpr into a stringLit -- each call MUST allocate a fresh new backend!
			KernelBackendPtr backend = KernelBackend::getDefault(sc);
			TargetCodePtr target = backend->convert(oclExpr);

			std::stringstream ss;
			OffloadSupportPost::generateCompat(sc, ss);
			// in case it is CCode we print it differently -- this shall be the general case tho!
			if (auto ccode = std::dynamic_pointer_cast<c_ast::CCode>(target)) {
				for (const c_ast::CodeFragmentPtr& cur : ccode->getFragments()) { ss << *cur; }
			} else {
				ss << toString(*target);
			}
			// dump the target code as string and put into IR lit
			return builder.stringLit(ss.str());
		}

		core::ExpressionPtr identity(const core::ExpressionPtr& expr) {
			return expr;
		}

		template<typename... P>
		core::ExpressionList makeExpressionList(P... args) {
			return toVector(identity(args)...);
		}

		NDRangePtr makeDefaultNDRange(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			NDRangePtr ndrange = std::make_shared<NDRange>();
			ndrange->setWorkDim(builder.uintLit(1));

			core::ExpressionList workSize = makeExpressionList(builder.uintLit(1));
			ndrange->setGlobalWorkSize(workSize);
			ndrange->setLocalWorkSize(workSize);
			return ndrange;
		}

		core::ExpressionPtr toWiExpr(core::NodeManager& manager, const StepContext& sc, const core::VariablePtr& wi, const core::ExpressionPtr& expr) {
			core::IRBuilder builder(manager);
			// the problem is the following:
			// if expr is a literal everything is fine, just return it and we are done.
			// however, if expr contains variables (which must be captured by wi/lwdata_item)
			// we need to adjust the access to use getArg of runtime extension
			core::VariableList free = core::analysis::getFreeVariables(manager.get(expr));
			if (free.empty()) return expr;
			// get the runtime extension
			auto& runExt = manager.getLangExtension<runtime::RuntimeExtension>();
			// transform the lwdata_item type into a type_lit
			core::NodeMap replacements;
			auto lwdType = builder.getTypeLiteral(sc.getDefaultLWDataItemType());
			// shortcut for certain child-nodes we will need
			auto params = sc.getDefaultLambdaExpr()->getParameterList();
			auto paramTypes = sc.getDefaultLambdaExpr()->getType().as<core::FunctionTypePtr>()->getParameterTypes();
			for (const auto& cur : free) {
				// determine the index of 'cur' within the lwdata_item
				auto it = std::find_if(params.begin(), params.end(),
					[&](const core::VariablePtr& var) {
						return *var == *cur;
					});
				// if we were not able to find it ... well then lucky you! :P
				assert_true(it != params.end()) << "failed to resolve variable in lwdata_item: " << dumpColor(cur);
				unsigned index = std::distance(params.begin(), it);
				// now replace the variable with a reference to the lwdata_item one
				auto varType = paramTypes->getElements()[index];
				replacements[builder.deref(cur)] = builder.callExpr(varType, runExt.getGetWorkItemArgument(),
					toVector<core::ExpressionPtr>(builder.deref(wi),
					core::encoder::toIR(manager, index), lwdType, builder.getTypeLiteral(varType)));
			}
			// paranioa check
			assert_true(free.size() == replacements.size()) << "failed to build replacements for at least one free var";
			auto newExpr = core::transform::replaceAllGen(manager, expr, replacements);
			return newExpr;
		}
	}

	core::CallExprPtr outline(core::NodeManager& manager, const core::StatementPtr& stmt, VariableRequirementList& requirements) {
		// check whether it is allowed
		assert_true(opencl::analysis::isOffloadAble(manager, stmt)) << "cannot outline given code: " << dumpColor(stmt);
		// obtain a list of all required variables
		core::VariableList free = opencl::analysis::getFreeVariables(manager, stmt, requirements);
		// parameter for new lambda
		core::VariableList parameter;
		// arguments for the call to the new lambda
		core::ExpressionList args;
		// build all parameters and replacements
		core::IRBuilder builder(manager);
		core::VarExprMap replacements;
		for (const auto& cur : free) {
			// default case, just pass it in -- however a fixed size array must be wrapped
			// example of invalid kernel arg if not ptr: struct __insieme_type_1 { float data[1000];; }
			if (core::lang::isReference(cur->getType()) &&
				!core::lang::isFixedSizedArray(opencl::analysis::getElementType(cur->getType()))) {
				// due to opencl restrictions, outline only allocates new vars
				// if we would generate ref/deref pairs kernel needs to strip anyway!
				auto param = builder.variable(cur->getType());
				replacements[cur] = param;
				parameter.push_back(param);
				// and add it correctly to the call args
				args.push_back(builder.deref(cur));
			} else {
				auto param = builder.variable(core::lang::buildRefType(cur->getType()));
				replacements[cur] = builder.deref(param);
				parameter.push_back(param);
				// and also pass it to the call
				args.push_back(cur);
			}
		}
		// replace everything in the original stmt
		core::StatementPtr body = core::transform::replaceVarsGen(manager, stmt, replacements);
		// replace everything in the supplied requirements
		VariableRequirementList newRequirements;
		for (const auto& cur : requirements) {
			auto var = cur->getVar();
			// find the new variable if present
			auto it = replacements.find(var);
			if (it != replacements.end()) {
				// the variable is now either hidden within a deref or avail right along
				auto expr = it->second;
				if (expr->getNodeType() == core::NT_CallExpr) expr = core::analysis::getArgument(expr, 0);
				var = expr.as<core::VariablePtr>();
			}
			auto ranges = cur->getRanges();
			std::transform(ranges.begin(), ranges.end(), ranges.begin(),
				[&](VariableRangePtr range) { range->replaceVars(replacements); return range; });
			// construct the updated requirement
			newRequirements.push_back(std::make_shared<VariableRequirement>(var, cur->getAccessMode(), ranges));
		}
		requirements = newRequirements;
		// create lambda accepting all free variables as arguments
		auto lambdaExpr = builder.lambdaExpr(manager.getLangBasic().getUnit(), parameter, body);
		// migrate the annotations from stmt to lambda
		core::transform::utils::migrateAnnotations(stmt, lambdaExpr);
		return builder.callExpr(manager.getLangBasic().getUnit(), lambdaExpr, args);
	}

	core::CallExprPtr buildGetWorkDim(core::NodeManager& manager) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(manager.getLangBasic().getUInt4(), oclExt.getWorkDim());
	}

	core::CallExprPtr buildGetGlobalId(core::NodeManager& manager, const core::ExpressionPtr& dim) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(oclExt.getSizeType(), oclExt.getGlobalId(), dim);
	}

	core::CallExprPtr buildGetGlobalSize(core::NodeManager& manager, const core::ExpressionPtr& dim) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(oclExt.getSizeType(), oclExt.getGlobalSize(), dim);
	}

	core::CallExprPtr buildGetLocalId(core::NodeManager& manager, const core::ExpressionPtr& dim) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(oclExt.getSizeType(), oclExt.getLocalId(), dim);
	}

	core::CallExprPtr buildGetLocalSize(core::NodeManager& manager, const core::ExpressionPtr& dim) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(oclExt.getSizeType(), oclExt.getLocalSize(), dim);
	}

	core::CallExprPtr buildGetNumGroups(core::NodeManager& manager, const core::ExpressionPtr& dim) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(oclExt.getSizeType(), oclExt.getNumGroups(), dim);
	}

	core::CallExprPtr buildGetGroupId(core::NodeManager& manager, const core::ExpressionPtr& dim) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		return builder.callExpr(oclExt.getSizeType(), oclExt.getGroupId(), dim);
	}

	core::CallExprPtr buildRegisterKernel(core::NodeManager& manager, unsigned int& id, const StepContext& sc, const core::LambdaExprPtr& oclExpr) {
		core::IRBuilder builder(manager);
		// grab a reference to the opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// most important step here, transform the oclExpr into a literal
		core::LiteralPtr literal = toKernelLiteral(manager, sc, oclExpr);
		// generate a new unique id for this kernel
		id = KernelTable::getNextUnique();
		// use the default IRBuilder to generate the callExpr
		return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getRegisterKernel(),
								builder.uintLit(id), core::lang::buildPtrFromArray(literal),
								core::lang::buildPtrFromArray(builder.stringLit(sc.getKernelName())));
	}

	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const core::ExpressionPtr& ndrange,
										 const core::ExpressionList& requirements, const OptionalList& optionals) {
		core::IRBuilder builder(manager);
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		// encode all optionals into IR in order to generate a var_list
		core::ExpressionList vargs;
		for (const auto& optional : optionals)
			vargs.push_back(Optional::encode(manager, optional));
		// this call instructs the irt to execute the kernel @id
		return builder.callExpr(manager.getLangBasic().getUnit(), oclExt.getExecuteKernel(), builder.uintLit(id), ndrange,
								core::encoder::toIR<core::ExpressionList, core::encoder::DirectExprListConverter>(manager, requirements),
								builder.pack(vargs));
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
				requirements.push_back(toIR(manager, sc, requirement));
		}
		// done .. execute the helper one level deeper
		return buildExecuteKernel(manager, id, toIR(manager, sc, cc.getNDRange()), requirements, cc.getOptionals());
	}

	core::StatementList buildExecuteGraph(core::NodeManager& manager, unsigned int id, const StepContext& sc) {
		core::IRBuilder builder(manager);
		core::StatementList result;

		const auto& graph = sc.getCallGraph();
		// determine all topological sets within the call graph
		auto sets = analysis::getTopologicalSets(graph.asBoostGraph());
		for (const auto& set : sets) {
			if (set.size() == 1) {
				// a single call will not result in parallel jobs -- sync call
				auto callExpr = buildExecuteKernel(manager, id, sc, **(std::begin(set)));
				// for the sake of debugging -- print IR of generated execute
				LOG(INFO) << "generated single execute call: " << dumpColor(callExpr);
				result.push_back(callExpr);
				continue;
			}

			for (const auto& descriptor : set) {
				// build a parallel execution
				auto callExpr = buildExecuteKernel(manager, id, sc, *descriptor);
				// for the sake of debugging -- print IR of generated execute
				LOG(INFO) << "generated parallel execute call: " << dumpColor(callExpr);
				result.push_back(callExpr);
			}
			// and merge to enforce a sync point
			result.push_back(builder.mergeAll());
		}
		return result;
	}

	OptionalPtr buildOptional(core::NodeManager& manager, const core::ExpressionPtr& expr, Optional::Modifier modifier) {
		core::IRBuilder builder(manager);
		// in case the optional is a function or a closure -- which must be in the form of
		// (...) -> 'a -- the opconverter will take care to evaluate it in an eager manner
		auto unit = manager.getLangBasic().getUnit();
		auto type = unit;
		switch (expr->getNodeType()) {
		case core::NT_Literal:
		case core::NT_Variable:
				type = expr->getType();
				break;
		case core::NT_CallExpr:
			{
				auto callExpr = expr.as<core::CallExprPtr>();
				// use returnType deduction for this purpose
				type = core::types::deduceReturnType(callExpr->getFunctionExpr()->getType().as<core::FunctionTypePtr>(),
					core::extractTypes(callExpr->getArgumentList()));
				break;
			}
		case core::NT_BindExpr:
			{
				auto bindExpr = expr.as<core::BindExprPtr>();
				assert_true(bindExpr->getParameters().empty())
					<< "optional argument modeled as bindExpr must not expect additional parameters";
				// also use returnType deduction but use bound parameters
				auto callExpr = bindExpr->getCall();
				type = core::types::deduceReturnType(callExpr->getFunctionExpr()->getType().as<core::FunctionTypePtr>(),
					core::extractTypes(callExpr->getArgumentList()));
				break;
			}
		default:
				// fall through and trigger error handling
				break;
		}
		assert_ne(type, unit) << "failed to deduce type of optional argument";
		// at this point we can finally construct the tuple
		auto result = std::make_shared<Optional>();
		result->setModifier(modifier);
		result->setValue(expr);
		result->setSize(builder.callExpr(manager.getLangBasic().getSizeof(), builder.getTypeLiteral(type)));
		return result;
	}

	core::LambdaExprPtr toIR(core::NodeManager& manager, const StepContext& sc, const NDRangePtr& ndrange) {
		core::IRBuilder builder(manager);
		// grab a reference to the runtime & opencl extension
		auto& oclExt = manager.getLangExtension<OpenCLExtension>();
		auto& runExt = manager.getLangExtension<runtime::RuntimeExtension>();

		core::VariableList params;
		auto wi = builder.variable(core::lang::buildRefType(core::lang::buildRefType(runExt.getWorkItemType())));
		params.push_back(wi);
		// body is simply a ref_new_init
		core::StatementList body;
		body.push_back(builder.returnStmt(toWiExpr(manager, sc, wi, NDRange::encode(manager, ndrange))));
		// and finally ... THE lambda ladies and gentleman
		return builder.lambdaExpr(oclExt.getNDRange(), params, builder.compoundStmt(body));
	}

	core::LambdaExprPtr toIR(core::NodeManager& manager, const StepContext& sc, const VariableRequirementPtr& var) {
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
		auto wi = builder.variable(core::lang::buildRefType(core::lang::buildRefType(runExt.getWorkItemType())));
		params.push_back(wi);
		params.push_back(builder.variable(core::lang::buildRefType(core::lang::buildRefType(oclExt.getNDRange()))));
		params.push_back(builder.variable(core::lang::buildRefType(manager.getLangBasic().getUInt4())));

		auto dim = builder.variable(core::lang::buildRefType(manager.getLangBasic().getUInt4()));
		params.push_back(dim);

		core::StatementList body;
		// build a case for each dimension
		std::vector<std::pair<core::ExpressionPtr, core::StatementPtr>> cases;
		for (const auto& range : var->getRanges()) {
			auto stmt = builder.returnStmt(DataRange::encode(manager, DataRange::get(manager,
				toWiExpr(manager, sc, wi, range->getSize()),
				toWiExpr(manager, sc, wi, range->getStart()),
				toWiExpr(manager, sc, wi, range->getEnd()))));
			auto expr = builder.uintLit(cases.size());
			cases.push_back(std::make_pair(expr, stmt));
		}
		assert_true(cases.size() > 0) << "variable requirement must consist of data ranges";
		body.push_back(builder.switchStmt(builder.deref(dim), cases, cases[0].second));
		// ok, now we can generate the "inner" lambda
		auto rangeExpr = builder.lambdaExpr(oclExt.getDataRange(), params, builder.compoundStmt(body));

		// prepare for the outer lamdba
		body.pop_back();
		params.pop_back();
		requirement->setNumRanges(builder.uintLit(var->getRanges().size()));
		// encode the requirement and build the "outer" lambda
		requirement->setRangeExpr(rangeExpr);
		body.push_back(builder.returnStmt(DataRequirement::encode(manager, requirement)));
		// and finally ... THE lambda ladies and gentleman
		return builder.lambdaExpr(oclExt.getDataRequirement(), params, builder.compoundStmt(body));
	}

	core::NodePtr FixParametersStep::process(const Converter& converter, const core::NodePtr& code) {
		auto lambdaExpr = code.isa<LambdaExprPtr>();
		if (!lambdaExpr) return code;

		// used to hold the trimmed parameters
		core::VariableList parameter;
		auto params = lambdaExpr->getParameterList();
		auto range = utils::make_range(params.begin(), params.end())
			.subrange(0, context.getDefaultRequirements().size());
		// fast-path?
		if (std::distance(params.begin(), params.end()) ==
			std::distance(range.begin(), range.end())) return code;
		// this will implicitly remove all additionally captured vars from the kernel
		// and thus will flatten them to 'unit'
		for_each(range, [&](const core::VariablePtr& cur) {	parameter.push_back(cur); });
		// replace all usages in the original body
		auto newLambdaExpr = builder.lambdaExpr(manager.getLangBasic().getUnit(),
			parameter, lambdaExpr->getBody());
		// in the end migrate the annotations and fix them up
		core::transform::utils::migrateAnnotations(lambdaExpr, newLambdaExpr);
		return Step::process(converter, newLambdaExpr);
	}

	namespace {
		class ForLoopReplacer : public core::transform::CachedNodeMapping {
			core::NodeManager& manager;
			core::IRBuilder builder;
			core::ExpressionList globalWorkSizes;
		public:
			ForLoopReplacer(core::NodeManager& manager) :
				manager(manager), builder(manager)
			{ }

			const core::ExpressionList& getGlobalWorkSizes() { return globalWorkSizes; }

			const core::NodePtr resolveElement(const core::NodePtr& node) {
				// if we have already exhausted all dimensions
				if (globalWorkSizes.size() == 3) return node;
				// do not visit child calls
				if (node->getNodeType() == core::NT_CallExpr) return node;
				// in case we do not face a for-stmt, hop one level deeper
				if (node->getNodeType() != core::NT_ForStmt) return node->substitute(manager, *this);
				// do nothing if it is not independent -- continue our way down
				if (!analysis::isIndependentStmt(node.as<core::StatementPtr>())) return node->substitute(manager, *this);

				core::StatementList stmts;
				auto forStmt = node.as<core::ForStmtPtr>();
				/*
				for( int<4> v87 = 0 .. 1000 : 1) {
					ptr_subscript(*v308, v87) = IMP_add(*ptr_subscript(*v306, v87), *ptr_subscript(*v307, v87));
				}

				-->

				int<4> v00 = num_cast(opencl_get_global_id(), type_lit(int<4>));
				if (v00 >= start && v00 < end && (v00 - start) % step == 0)
					ptr_subscript(*v308, v00) = IMP_add(*ptr_subscript(*v306, v00), *ptr_subscript(*v307, v00));
				*/
				auto decl = forStmt->getDeclaration();
				stmts.push_back(DeclarationStmt::get(manager, decl->getVariable(), builder.numericCast(
					buildGetGlobalId(manager, builder.uintLit(globalWorkSizes.size())), decl->getVariable()->getType())));
				// save the loop end for build an NDRange from it later on
				globalWorkSizes.push_back(forStmt->getEnd());
				// prepare the body -- thus we do a bottom up replacement
				auto body = forStmt->getBody()->substitute(manager, *this);
				auto cond = builder.logicAnd(
					builder.ge(decl->getVariable(), forStmt->getStart()),
					builder.logicAnd(
						builder.lt(decl->getVariable(), forStmt->getEnd()),
						builder.eq(
							builder.mod(
								builder.sub(decl->getVariable(), decl->getInitialization()),
								forStmt->getStep()),
							builder.numericCast(builder.uintLit(0), forStmt->getStep()->getType()))));
				// try to simplify the generated condition if possible
				stmts.push_back(builder.ifStmt(core::transform::simplify(manager, cond), body));
				return builder.compoundStmt(stmts);
			}
		};
	}

	core::NodePtr LoopOptimizerStep::process(const Converter& converter, const core::NodePtr& node) {
		auto lambdaExpr = node.isa<LambdaExprPtr>();
		if (!lambdaExpr) return node;

		assert_true(context.getCallGraph().getNumVertices() == 0) << "execution graph must not exist prior loop optimizer";
		/*
         * note1:
		 * you are not allowed to modify order or type of the original arguments!
		 * however, it is legal and possible to extend it with 'optionals'. this might
		 * help to implement dynamic loop tiling if required.
		 *
		 * note2:
		 * the loop optimizer must generate at least one vertex in the call graph
		 * as the applied transformations influence the NDRange! the CallOptimizer
		 * is later on permitted to may split to into e.g. a z-order graph
		 *
		 * note3:
		 * at this point no kernel types are present, thus allowing to apply loop
		 * transformations which are independent of the target address space. the
		 * TypeOptimizer may further enhance the optimization by local prefetching etc.
		 *
		 * note4:
		 * keep in mind that is you write things like:
		 * 'ndrange->setGlobalWorkSize(makeExpressionList(forStmt->getEnd()));'
		 * that toWiExpr must be able to make all free variables of the expression
		 * to captured args (thus all free vars must be reachable via getArg!!!)
		 */

		auto callContext = std::make_shared<CallContext>();
		context.getCallGraph().addVertex(callContext);

		// replace the loops with OpenCL constructs
		ForLoopReplacer loopReplacer(manager);
		auto body = loopReplacer.map(lambdaExpr->getBody());

		const auto& globalWorkSizes = loopReplacer.getGlobalWorkSizes();
		// introduce a single default clEnqueueTask call if we replace nothing
		if (globalWorkSizes.empty()) {
			callContext->setNDRange(makeDefaultNDRange(manager));
			return node;
		}
		// apply ndranges impicitly computed by optimizer
		auto ndrange = std::make_shared<NDRange>();
		ndrange->setWorkDim(builder.uintLit(globalWorkSizes.size()));
		ndrange->setGlobalWorkSize(globalWorkSizes);
		callContext->setNDRange(ndrange);
		// for the sake of sanity!
		assert_true(context.getCallGraph().getNumVertices() > 0) << "execution graph must exist prior loop optimizer";
		auto newLambdaExpr= builder.lambdaExpr(manager.getLangBasic().getUnit(), lambdaExpr->getParameterList(), body);
		// migrate the annotations -- but no need to fixup as only hthe body has changed
		core::transform::utils::migrateAnnotations(lambdaExpr, newLambdaExpr);
		return Step::process(converter, newLambdaExpr);
	}

	core::NodePtr TypeOptimizerStep::process(const Converter& converter, const core::NodePtr& code) {
		auto lambdaExpr = code.isa<LambdaExprPtr>();
		if (!lambdaExpr) return code;

		/*
		 * note:
		 * per default, all pointers within the kernel region are modeled as __global 'a*.
		 * thus if one wants to restrict the scope to e.g. private or local wrap them with kernel types
		 *
		 * check for additional info: opencl_type_handler.cpp:handleKrnlType
		 */
		return Step::process(converter, lambdaExpr);
	}

	core::NodePtr CallOptimizerStep::process(const Converter& converter, const core::NodePtr& code) {
		// this kernel requires double precision
		context.getExtensions().insert(StepContext::KhrExtension::Fp64);
		// and name it such that we can mark it later as __kernel
		context.setKernelName("__insieme_fun_0");
		return Step::process(converter, code);
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

		// also assure that the callGraph is sane
		assert_true(context.getCallGraph().getNumVertices() > 0) << "execution graph must consist of at least one vertex";
		assert_true(utils::graph::detectCycle(context.getCallGraph().asBoostGraph()).empty()) << "execution graph must be acyclic";
		return code;
	}

	std::vector<core::LambdaExprPtr> toOcl(const Converter& converter, core::NodeManager& manager, const core::NodePtr& code,
										   const core::CallExprPtr& callExpr, const VariableRequirementList& requirements,
										   const DeviceAnnotationPtr& deviceInfo) {
		// even though the interface supports it, only one variant is generated
		core::IRBuilder builder(manager);
		std::vector<core::LambdaExprPtr> variants;

		auto lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();
		// log what we got as input to quickly check if outline worked right
		LOG(INFO) << "trying to generate kernel for: " << dumpColor(lambdaExpr);
		// context which is usable among all steps
		StepContext context;
		context.setDefaultLambdaExpr(lambdaExpr);
		context.setDefaultLWDataItemType(analysis::getLWDataItemType(lambdaExpr));
		context.setDefaultRequirements(requirements);
		// all modifications applied to the lambda (which will ultimately yield OlcIR) are modeled as preProcessors
		auto processor = makePreProcessorSequence(
			getBasicPreProcessorSequence(),
			makePreProcessor<FixParametersStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<LoopOptimizerStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<TypeOptimizerStep>(boost::ref(manager), boost::ref(context)),
			makePreProcessor<CallOptimizerStep>(boost::ref(manager), boost::ref(context)),
			// this is very important as otherwise we have generated a faulty kernel
			makePreProcessor<IntegrityCheckStep>(boost::ref(manager), boost::ref(context)));

		unsigned id;
		core::StatementList body;
		// run the extracted lambda through the pipeline
		auto kernelExpr = processor->process(converter, lambdaExpr).as<LambdaExprPtr>();
		// first of all, we need to register the kernel itself
		auto registerExpr = buildRegisterKernel(manager, id, context, kernelExpr);
		// as the processor has yield a callGraph build it now
		body = buildExecuteGraph(manager, id, context);
		body.push_back(registerExpr);
		// now the body is populated with all required IR constructs, build the variant
		// which encapsultes the whole execution plan
		lambdaExpr = builder.lambdaExpr(lambdaExpr->getFunctionType(), lambdaExpr->getParameterList(),
			builder.compoundStmt(body));
		// very important step:
		// first of all, mark this variant as opencl capable.
		// secondly, we take note which kernel this meta_info is about for future extendability
		info_type meta_data;
		meta_data.opencl = true;
		meta_data.kernel_id = id;
		// this kernel can run on any device
		meta_data.device_type = static_cast<unsigned>(deviceInfo->getDevice()->getType());
		lambdaExpr->attachValue(meta_data);
		// add the generated variant to the result set
		variants.push_back(lambdaExpr);
		return variants;
	}
}
}
}
}
