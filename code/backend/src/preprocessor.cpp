/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/backend/preprocessor.h"

#include <boost/algorithm/string.hpp>


#include "insieme/annotations/backend_instantiate.h"
#include "insieme/annotations/c/include.h"

#include "insieme/backend/function_manager.h"
#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/type_manager.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/annotations/backend_interception_info.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/static_vars.h"

#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/core/transform/instantiate.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace backend {


	PreProcessorPtr getBasicPreProcessorSequence(BasicPreprocessorFlags options) {
		vector<PreProcessorPtr> steps;
		if(!(options & SKIP_POINTWISE_EXPANSION)) { steps.push_back(makePreProcessor<InlinePointwise>()); }
		steps.push_back(makePreProcessor<CorrectRecVariableUsage>());
		steps.push_back(makePreProcessor<RecursiveLambdaInstantiator>());
		steps.push_back(makePreProcessor<RefCastIntroducer>());
		steps.push_back(makePreProcessor<DefaultedMemberCallMarker>());
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

	std::ostream& PreProcessingSequence::printTo(std::ostream& out) const {
		return out << join(", ", preprocessor, [](std::ostream& out, const PreProcessorPtr& pre) { out << *pre; });
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
		core::IRBuilder builder(code->getNodeManager());

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

		// 3) init exprs
		// this pass can be greatly simplified when we introduce declarations in init expressions

		utils::map::PointerMap<core::TagTypeReferencePtr, core::TagTypePtr> ttAssignment;
		visitDepthFirstOnce(retCode, [&](const core::TagTypeDefinitionPtr& td){
			for(const auto& ttb : td->getDefinitions()) {
				const auto& ttRef = ttb->getTag();
				auto tt = builder.normalize(builder.tagType(ttRef, td));
				if(ttAssignment.find(ttRef) != ttAssignment.end() && ttAssignment[ttRef] != tt) {
					assert_not_implemented()
							<< "Case of non-unique tag type references not handled here, could be done but complex, and this code should be obsolete in the future\n"
							<< "Duplicated tag type reference: " << *ttRef;
				}
				ttAssignment[ttRef] = tt;
			}
		});

		retCode = core::transform::transformBottomUpGen(retCode, [&ttAssignment](const core::InitExprPtr& init) {
			core::IRBuilder builder(init->getNodeManager());
			auto initType = core::lang::ReferenceType(init->getType()).getElementType();
			// scalar init cpp references from plain references
			if(init.getInitExprs().size() == 1) {
				auto expr = init.getInitExprs().front();
				auto ttype = init->getType();
				if(cl::isReference(ttype) && cl::isReferenceTo(expr, initType)) {
						cl::ReferenceType varT(ttype), initT(expr);
						if(!varT.isPlain() && initT.isPlain()) {
							return builder.initExpr(init->getMemoryExpr(), cl::buildRefKindCast(expr, varT.getKind()));
						}
				}
			}
			// init tagtypes
			auto tt = initType.isa<core::TagTypePtr>();
			if(auto ttRef = initType.isa<core::TagTypeReferencePtr>()) {
				tt = ttAssignment[ttRef];
			}
			if(tt) {
				core::TypeList targetTypes;
				for(auto field: tt->getRecord()->getFields()->getFields()) {
					targetTypes.push_back(field->getType());
				}
				core::ExpressionList newInits;
				size_t i = 0;
				for(auto expr: init->getInitExprList()) {
					if(targetTypes.size() <= i) {
						newInits.push_back(expr);
						continue;
					}
					auto& ttype = targetTypes[i++];

					if(cl::isReference(ttype) && cl::isReference(expr)) {
						cl::ReferenceType varT(ttype), initT(expr);
						if(!varT.isPlain() && initT.isPlain()) {
							newInits.push_back(cl::buildRefKindCast(expr, varT.getKind()));
							continue;
						}
					}
					newInits.push_back(expr);
				}
				return builder.initExpr(init->getMemoryExpr(), newInits);
			}
			return init;
		}, builtInLimiter);

		return retCode;
	}

	core::NodePtr DefaultedMemberCallMarker::process(const Converter& converter, const core::NodePtr& code) {
		visitDepthFirstOnce(code, [&](const core::CallExprPtr& call) {
			auto fun = call->getFunctionExpr();
			if(core::analysis::isaDefaultMember(fun)) {
				auto obj = core::analysis::getObjectType(fun->getType());
				if(obj.isa<core::TagTypePtr>()) {
					obj.as<core::TagTypePtr>()->getRecord().attachValue<UsedMemberTag>();
				}
			}
		});
		return code;
	}


	void BackendInterceptor::addBackendInterception(const std::string& namePtefix, const std::string& headerToAttach) {
		backendInterceptions.push_back({ namePtefix, headerToAttach });
	}

	core::NodePtr BackendInterceptor::process(const Converter& converter, const core::NodePtr& code) {
		// early exit
		if(backendInterceptions.empty()) return code;

		core::IRBuilder builder(code.getNodeManager());
		auto res = code;

		// first we replace calls to LambdaExprs with Literals
		res = core::transform::transformBottomUpGen(res, [&](const core::CallExprPtr& call) {
			const auto& callee = call->getFunctionExpr();
			const auto& calleeType = callee->getType().as<core::FunctionTypePtr>();
			// only consider calls to members of TagTypes
			if(calleeType->isMember() && callee.isa<core::LambdaExprPtr>()) {
				auto objectType = core::analysis::getObjectType(calleeType);
				if(const auto& tagType = objectType.isa<core::TagTypePtr>()) {
					const auto& typeName = tagType->getTag()->getName()->getValue();

					// we iterate over all mappings
					for(const auto& backendInterception : backendInterceptions) {
						// if this is a call we should modify
						if(boost::starts_with(typeName, backendInterception.first)) {
							// we create a new callee
							auto oldCalleeName = callee.as<core::LambdaExprPtr>()->getReference()->getNameAsString();
							std::string targetName;
							// if the frontend attached backend interception information, we can use that
							if(core::annotations::hasBackendInterceptionInfo(callee)) {
								targetName = core::annotations::getBackendInterceptionInfo(callee).qualifiedName;

								// if this is a call to a default generated assignment operator, we know the name
							} else if(calleeType->isMemberFunction() && core::analysis::isaDefaultMember(callee)) {
								targetName = "operator=";

								// otherwise we don't know what to do. we just use the old name
							} else {
								targetName = oldCalleeName;
							}

							// we create a surrogate literal callee, which will mimic an interception by the frontend
							auto newCallee = builder.literal(callee->getType(), oldCalleeName);
							// we migrate annotations and attach the name as well as the include location
							core::transform::utils::migrateAnnotations(callee, newCallee);
							annotations::c::attachInclude(newCallee, backendInterception.second);
							core::annotations::attachName(newCallee, targetName);

							// and create the new call to return with annotations migrated
							auto res = builder.callExpr(call->getType(), newCallee, call->getArgumentDeclarationList());
							core::transform::utils::migrateAnnotations(call, res);
							return res;
						}
					}
				}
			}
			return call;
		}, core::transform::globalReplacement, false); // Note that we are migrating annotations ourselves. We don't want transformBottomUpGen to do it for us

		// and then we replace TagTypes with GenericTypes
		res = core::transform::transformBottomUpGen(res, [&](const core::TypePtr& type) {
			if(const auto& tagType = type.isa<core::TagTypePtr>()) {
				const auto& typeName = tagType->getTag()->getName()->getValue();

				// we iterate over all mappings
				for(const auto& backendInterception : backendInterceptions) {
					// if this is a call we should modify
					if(boost::starts_with(typeName, backendInterception.first)) {
						// if this type has backend interception info attached, we can proceed
						if(core::annotations::hasBackendInterceptionInfo(tagType)) {
							auto backendInterceptionInfo = core::annotations::getBackendInterceptionInfo(tagType);

							// for each instantiation argument, we create a generic type with a fake header attachment (one we already use anyways), so it will get printed verbatim by the backend
							core::TypeList instantiationArguments = ::transform(backendInterceptionInfo.instantiationArguments, [&builder](const std::string& arg) -> core::TypePtr {
								auto genArg = builder.genericType(arg);
								annotations::c::attachInclude(genArg, "stdint.h");
								return genArg;
							});

							// we create a surrogate type, which will mimic an interception by the frontend
							auto res = builder.genericType(utils::mangle(backendInterceptionInfo.qualifiedName), instantiationArguments);
							// we migrate annotations and attach the name as well as the include location
							core::transform::utils::migrateAnnotations(type, res);
							annotations::c::attachInclude(res, backendInterception.second);
							core::annotations::attachName(res, backendInterceptionInfo.qualifiedName);
							return res.as<core::TypePtr>();
						}
					}
				}
			}
			return type;
		}, core::transform::globalReplacement, false); // Note that we are migrating annotations ourselves. We don't want transformBottomUpGen to do it for us

		return res;
	}

} // end namespace backend
} // end namespace insieme
