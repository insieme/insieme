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

#include "insieme/frontend/utils/conversion_utils.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/state/function_manager.h"

#include "insieme/core/analysis/default_delete_member_semantics.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace frontend {
namespace utils {

	core::ExpressionPtr fixTempMemoryInInitExpression(const core::ExpressionPtr& memLoc, const core::ExpressionPtr& initExpIn) {
		auto& mgr = initExpIn->getNodeManager();
		auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
		// if the init expr is a constructor call
		if(core::analysis::isConstructorCall(initExpIn)) {
			core::CallExprAddress call(initExpIn.as<core::CallExprPtr>());
			assert_ge(call->getNumArguments(), 1) << "Ill-formed constructor call. Missing this argument";
			if(refExt.isCallOfRefTemp(call->getArgument(0))) {
				// we replace the first parameter (which has been created as ref_temp) by the memory space being initialized
				return core::transform::replaceNode(
					       initExpIn->getNodeManager(), call->getArgument(0),
					       core::lang::buildRefCast(memLoc, call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getParameterType(0)))
					.as<core::ExpressionPtr>();
			}
		}
		// if the init expr is an init expr
		core::ExpressionAddress initExp(initExpIn);
		if(refExt.isCallOfRefDeref(initExp)) initExp = core::ExpressionAddress(initExpIn.as<core::CallExprPtr>()->getArgument(0));
		if(auto initInitExpr = initExp.isa<core::InitExprAddress>()) {
			auto memExprAddr = initInitExpr->getMemoryExpr();
			if(refExt.isCallOfRefTemp(memExprAddr)) {
				return core::transform::replaceNode(initExp->getNodeManager(), memExprAddr, memLoc).as<core::ExpressionPtr>();
			}
		}
		return initExpIn;
	}


	core::CallExprPtr buildCxxMethodCall(conversion::Converter& converter, const core::TypePtr& retType, const core::ExpressionPtr& callee,
		                                 const core::ExpressionPtr& thisArgument, clang::CallExpr::arg_const_range argumentRange) {
		auto lambdaParamTypes = callee.getType().as<core::FunctionTypePtr>().getParameterTypeList();

		// the constructor is then simply a call with the mem location and all its arguments
		core::ExpressionList arguments{thisArgument};
		size_t i = 1;
		for(auto arg : argumentRange) {
			arguments.push_back(converter.convertCxxArgExpr(arg, lambdaParamTypes[i++]));
		}

		// return call
		return converter.getIRBuilder().callExpr(retType, callee, arguments);
	}

	core::ExpressionPtr buildEnumConstantExpression(conversion::Converter& converter, const clang::EnumConstantDecl* decl) {
		auto& builder = converter.getIRBuilder();
		const clang::EnumType* enumType = llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(decl->getDeclContext())->getTypeForDecl());

		// determine target integral type
		auto irEnumDef = core::lang::getEnumTypeDefinition(converter.convertType(clang::QualType(enumType, 0)));
		auto enumIntType = core::lang::EnumDefinition(irEnumDef).getIntType();

		// get the init val of the enum constant decl
		core::ExpressionPtr val;
		std::string value = decl->getInitVal().toString(10);
		val = builder.literal(enumIntType, value);
		if(val->getType() != enumIntType) {
			val = builder.numericCast(val, enumIntType);
		}

		return core::lang::buildEnumValue(irEnumDef, val);
	}

	core::ExpressionPtr convertConstructExpr(conversion::Converter& converter, const clang::CXXConstructExpr* constructExpr,
		                                     const core::ExpressionPtr& memLoc) {
		core::TypePtr resType = converter.convertType(constructExpr->getType());

		if(constructExpr->getType()->isArrayType()) {
			if(constructExpr->arguments().begin() != constructExpr->arguments().end()) assert_not_implemented();
			return converter.getIRBuilder().initExpr(memLoc);
		}

		if(VLOG_IS_ON(2)) {
			VLOG(2) << "convertConstructExpr - ResType: \n" << dumpDetailColored(resType);
			auto types = converter.getIRTranslationUnit().getTypes();
			auto t = types.find(resType.as<core::GenericTypePtr>());
			if(t != types.end()) {
				VLOG(2) << " - recType:\n" << *t->second << "\n";
			}
		}

		// try to employ plugins for translation
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			auto retIr = extension->Visit(constructExpr, converter);
			if(retIr) {
				if(retIr.isa<core::CallExprPtr>()) {
					auto call = retIr.as<core::CallExprPtr>();
					auto args = call.getArgumentList();
					args[0] = memLoc;
					return converter.getIRBuilder().callExpr(call.getType(), call.getFunctionExpr(), args);
				}
				return retIr;
			}
		}

		// get constructor lambda
		auto constructorLambda = converter.getFunMan()->lookup(constructExpr->getConstructor());

		// return call
		auto retType = constructorLambda->getType().as<core::FunctionTypePtr>()->getReturnType();
		return utils::buildCxxMethodCall(converter, retType, constructorLambda, memLoc, constructExpr->arguments());
	}

	core::ExpressionPtr convertMaterializingExpr(conversion::Converter& converter, core::ExpressionPtr retIr) {
		auto& builder = converter.getIRBuilder();
		// if we are materializing the rvalue result of a non-built-in function call, do it
		auto subCall = retIr.isa<core::CallExprPtr>();
		if(subCall) {
			// if we are already materialized everything is fine
			if(core::lang::isPlainReference(retIr->getType())) return retIr;
			// otherwise, materialize

			// if call to deref, simply remove it
			auto& refExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefDeref(retIr)) {
				retIr = subCall->getArgument(0);
				return retIr;
			}
			// otherwise, materialize call if not built in
			if(!core::lang::isBuiltIn(subCall->getFunctionExpr())) {
				retIr = builder.callExpr(builder.refType(retIr->getType()), subCall->getFunctionExpr(), subCall->getArgumentDeclarations());
			}
		}
		return retIr;
	}

	core::ExpressionPtr prepareThisExpr(conversion::Converter& converter, core::ExpressionPtr thisArg) {
		if(core::lang::isPointer(thisArg)) thisArg = core::lang::buildPtrToRef(thisArg);
		if(!core::lang::isReference(thisArg)) thisArg = frontend::utils::convertMaterializingExpr(converter, thisArg);
		return thisArg;
	}

	core::StatementPtr addIncrementExprBeforeAllExitPoints(const core::StatementPtr& body, const core::StatementPtr& incrementExpression) {
		core::IRBuilder builder(incrementExpression.getNodeManager());

		core::StatementList newBody;
		if(body) newBody.push_back(body);

		// add the increment expression at the end of the body
		newBody.push_back(incrementExpression);

		if(body) {
			auto& origBody = newBody.front();

			// and also in front of every continue statement
			auto exitPoints = core::analysis::getExitPoints(origBody);

			// sort those points in a reverse order
			std::sort(exitPoints.rbegin(), exitPoints.rend());

			// add increments in front of all continue calls
			for(const auto& cur : exitPoints) {
				if (cur.isa<core::ContinueStmtAddress>()) {
					// insert increment before the continue stmt
					if(cur.isRoot()) {
						origBody = builder.compoundStmt(incrementExpression, cur.as<core::StatementPtr>());
					} else {
						origBody = core::transform::insertBefore(origBody->getNodeManager(), cur.switchRoot(origBody), incrementExpression).as<core::StatementPtr>();
					}
				}
			}
		}

		return stmtutils::aggregateStmts(builder, newBody);
	}

	core::TypePtr getThisType(conversion::Converter& converter, const clang::CXXMethodDecl* methDecl) {
		auto parentType = converter.convertType(converter.getCompiler().getASTContext().getRecordType(methDecl->getParent()));
		return getThisType(methDecl, parentType);
	}

	bool isDefaultClassMember(const clang::CXXMethodDecl* methDecl) {
		auto constDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(methDecl);
		auto dtorDecl = llvm::dyn_cast<clang::CXXDestructorDecl>(methDecl);
		return (constDecl && (constDecl->isDefaultConstructor() || constDecl->isCopyOrMoveConstructor()))
				|| dtorDecl
				|| (methDecl && (methDecl->isCopyAssignmentOperator() || methDecl->isMoveAssignmentOperator()));
	}

	core::analysis::MemberProperties createDefaultCtorFromDefaultCtorWithDefaultParams(conversion::Converter& converter,
	                                                                                   const clang::CXXConstructorDecl* ctorDecl,
	                                                                                   const core::analysis::MemberProperties& defaultCtorWithParams) {
		const auto& otherCtorLit = defaultCtorWithParams.literal;
		core::IRBuilder builder(otherCtorLit.getNodeManager());

		// get necessary stuff
		auto thisType = getThisType(converter, ctorDecl);
		auto ctorType = builder.getDefaultConstructorType(thisType);
		auto thisVariable = builder.variable(builder.refType(thisType));
		const auto& paramTypes = otherCtorLit->getType().as<core::FunctionTypePtr>()->getParameterTypeList();

		// generate the body, which only contiains a call to the other constructor
		core::ExpressionList args;
		// first pass the this parameter
		args.push_back(builder.deref(thisVariable));
		// and then append all the constructor argument default values
		for(unsigned index = 0; index < ctorDecl->getNumParams(); ++index) {
			// if any of the default arguments isn't instantiated, we can't create this ctor here
			if(ctorDecl->getParamDecl(index)->hasUninstantiatedDefaultArg()) {
				return {};
			}
			args.push_back(converter.convertCxxArgExpr(ctorDecl->getParamDecl(index)->getDefaultArg(), paramTypes[index + 1]));
		}
		auto body = builder.callExpr(otherCtorLit, args);

		// build result
		core::analysis::MemberProperties res;
		res.literal = builder.getLiteralForConstructor(ctorType);
		res.lambda = builder.lambdaExpr(ctorType, toVector(thisVariable), builder.compoundStmt(body));
		return res;
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

