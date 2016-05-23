/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/backend/preprocessor.h"

#include "insieme/annotations/backend_instantiate.h"

#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/function_manager.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/static_vars.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/attributes.h"

#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/instantiate.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {


	PreProcessorPtr getBasicPreProcessorSequence(BasicPreprocessorFlags options) {
		vector<PreProcessorPtr> steps;
		if(!(options & SKIP_POINTWISE_EXPANSION)) { steps.push_back(makePreProcessor<InlinePointwise>()); }
		steps.push_back(makePreProcessor<CorrectRecVariableUsage>());
		steps.push_back(makePreProcessor<RecursiveLambdaInstantiator>());
		steps.push_back(makePreProcessor<RefCastIntroducer>());
		steps.push_back(makePreProcessor<RefDeclEliminator>());
		return makePreProcessor<PreProcessingSequence>(steps);
	}


	core::NodePtr PreProcessingSequence::process(const Converter& converter, const core::NodePtr& code) {
		auto& manager = converter.getNodeManager();

		// start by copying code to given target manager
		core::NodePtr res = manager.get(code);

		// apply sequence of pre-processing steps
		for_each(preprocessor, [&](const PreProcessorPtr& cur) { res = cur->process(converter, res); });

		// return final result
		return res;
	}


	// ------- concrete pre-processing step implementations ---------

	core::NodePtr NoPreProcessing::process(const Converter& converter, const core::NodePtr& code) {
		// just copy to target manager
		return converter.getNodeManager().get(code);
	}


	// --------------------------------------------------------------------------------------------------------------
	//      PreProcessor InlinePointwise => replaces invocations of pointwise operators with in-lined code
	// --------------------------------------------------------------------------------------------------------------

	class PointwiseReplacer : public core::transform::CachedNodeMapping {
		core::NodeManager& manager;
		const core::lang::BasicGenerator& basic;

	  public:
		PointwiseReplacer(core::NodeManager& manager) : manager(manager), basic(manager.getLangBasic()){};

		const core::NodePtr resolveElement(const core::NodePtr& ptr) {

			// check whether current node is a target of interest
			auto call = ptr.isa<core::CallExprPtr>();
			if(!call) return ptr->substitute(manager, *this);		// not of interest
			auto callArgs = core::transform::extractArgExprsFromCall(call);

			// get the (potential) pointwise operator
			auto pointwiseOp = call->getFunctionExpr();

			// check whether it is a call to the right function
			auto& arrayModule = manager.getLangExtension<core::lang::ArrayExtension>();
			if (!arrayModule.isCallOfArrayPointwise(pointwiseOp)) return ptr->substitute(manager, *this);

			// extract argument types
			core::TypePtr arg0Type = callArgs[0]->getType();
			core::TypePtr arg1Type = callArgs[1]->getType();
			core::TypePtr resType = call->getType();

			// check argument and result types!
			assert_pred1(core::lang::isFixedSizedArray, arg0Type) << "First argument should be a fixed-sized array!";
			assert_pred1(core::lang::isFixedSizedArray, arg1Type) << "Second argument should be a fixed-sized array!";
			assert_pred1(core::lang::isFixedSizedArray, resType) << "Result should be a fixed-sized array!";

			// obtain the vector size
			unsigned size = core::lang::ArrayType(arg0Type).getNumElements();

			// obtain element types
			core::TypePtr arg0ElementType = core::lang::ArrayType(arg0Type).getElementType();
			core::TypePtr arg1ElementType = core::lang::ArrayType(arg1Type).getElementType();

			// extract operator
			core::ExpressionPtr op = pointwiseOp.as<core::CallExprPtr>()->getArgument(0);

			// create new lambda, realizing the point-wise operation
			core::IRBuilder builder(manager);
			core::FunctionTypePtr funType = builder.functionType(toVector<core::TypePtr>(arg0Type, arg1Type), resType);

			core::VariablePtr v1 = builder.variable(builder.refType(arg0Type));
			core::VariablePtr v2 = builder.variable(builder.refType(arg1Type));
			core::VariablePtr res = builder.variable(resType);

			// create vector init expression
			vector<core::ExpressionPtr> elements;

			// unroll the pointwise operation
			core::TypePtr unitType = basic.getUnit();
			core::TypePtr longType = basic.getInt8();
			core::ExpressionPtr arraySubscript = arrayModule.getArraySubscript();
			for(std::size_t i = 0; i < size; i++) {
				core::LiteralPtr index = builder.literal(longType, boost::lexical_cast<std::string>(i));
				core::ExpressionPtr a = builder.callExpr(arraySubscript, builder.deref(v1), index);
				core::ExpressionPtr b = builder.callExpr(arraySubscript, builder.deref(v2), index);
				elements.push_back(builder.callExpr(op, a, b));
			}

			// return result
			core::StatementPtr body = builder.returnStmt(builder.initExprTemp(builder.refType(arg0Type), elements));

			// construct substitute ...
			core::LambdaExprPtr substitute = builder.lambdaExpr(funType, toVector(v1, v2), body);
			return builder.callExpr(resType, substitute, callArgs)->substitute(manager, *this);
		}
	};

	core::NodePtr InlinePointwise::process(const Converter& converter, const core::NodePtr& code) {
		// the converter does the magic
		return PointwiseReplacer(converter.getNodeManager()).map(code);
	}

	core::NodePtr CorrectRecVariableUsage::process(const Converter& converter, const core::NodePtr& code) {
		core::NodeManager& manager = converter.getNodeManager();

		// this pass has been implemented as part of the core manipulation utils
		return core::transform::makeCachedLambdaMapper([&](const core::NodePtr& code) -> core::NodePtr {
			       // only consider lambdas
			       if(code->getNodeType() != core::NT_LambdaExpr) return code;
			       // use core library utility to fix recursive variable usage
			       return core::transform::correctRecursiveLambdaVariableUsage(manager, code.as<core::LambdaExprPtr>());
			   })
		    .map(code);
	}

	core::NodePtr RecursiveLambdaInstantiator::process(const Converter& converter, const core::NodePtr& code) {
		auto skipInstantiation = [&](const core::NodePtr& node) {
			return !annotations::isBackendInstantiate(node) && converter.getFunctionManager().isBuiltIn(node);
		};
		return core::transform::instantiateTypes(code, skipInstantiation);
	}

	namespace cl = core::lang;

	core::NodePtr RefCastIntroducer::process(const Converter& converter, const core::NodePtr& code) {
		auto builtInLimiter = [](const core::NodePtr& cur) {
			if(core::lang::isBuiltIn(cur)) return core::transform::ReplaceAction::Prune;
			return core::transform::ReplaceAction::Process;
		};

		// 1) declaration stmts
		auto retCode = core::transform::transformBottomUpGen(code, [](const core::DeclarationStmtPtr& decl) {
			if(cl::isReference(decl->getVariable()) && cl::isReference(decl->getInitialization())) {
				cl::ReferenceType varT(decl->getVariable()), initT(decl->getInitialization());
				if(!varT.isPlain() && initT.isPlain()) {
					core::DeclarationStmtAddress addr(decl);
					auto replacement = core::transform::replaceAddress(decl->getNodeManager(), addr->getInitialization(),
					                                                   cl::buildRefKindCast(decl->getInitialization(), varT.getKind()));
					return replacement.getRootNode().as<core::DeclarationStmtPtr>();
				}
			}
			return decl;
		}, builtInLimiter);

		// 2) returns
		retCode = core::transform::transformBottomUpGen(retCode, [](const core::ReturnStmtPtr& ret) {
			if(cl::isReference(ret->getReturnType()) && cl::isReference(ret->getReturnExpr())) {
				cl::ReferenceType varT(ret->getReturnType()), initT(ret->getReturnExpr());
				if(!varT.isPlain() && initT.isPlain()) {
					core::ReturnStmtAddress addr(ret);
					auto replacement = core::transform::replaceAddress(ret->getNodeManager(), addr->getReturnExpr(),
					                                                   cl::buildRefKindCast(ret->getReturnExpr(), varT.getKind()));
					return replacement.getRootNode().as<core::ReturnStmtPtr>();
				}
			}
			return ret;
		}, builtInLimiter);

		return retCode;
	}

	core::NodePtr RefDeclEliminator::process(const Converter& converter, const core::NodePtr& code) {
		auto& refExt = code->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		return core::transform::transformBottomUpGen(code, [&refExt](const core::CallExprPtr& call) {
			if(refExt.isCallOfRefDecl(call)) {
				return core::lang::buildRefTemp(core::analysis::getReferencedType(call->getType())).as<core::CallExprPtr>();
			}
			return call;
		}, core::transform::globalReplacement);
	}

} // end namespace backend
} // end namespace insieme
