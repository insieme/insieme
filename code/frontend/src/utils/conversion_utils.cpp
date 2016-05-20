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

#include "insieme/frontend/utils/conversion_utils.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/state/function_manager.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/reference.h"
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
				// we replace the first parameter (which has been created as ref_temp) by the variable to initialize
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
				// we replace the memory address to be initialized with the variable being declared
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

		VLOG(2) << "convertConstructExpr - ResType: \n" << dumpDetailColored(resType) << " recType:\n"
			    << (*converter.getIRTranslationUnit().getTypes().find(resType.as<core::GenericTypePtr>())).second << "\n";

		// try to employ plugins for translation
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			auto retIr = extension->Visit(constructExpr, converter);
			if(retIr) {
				auto call = retIr.as<core::CallExprPtr>();
				auto args = call.getArgumentList();
				args[0] = memLoc;
				return converter.getIRBuilder().callExpr(call.getType(), call.getFunctionExpr(), args);
			}
		}

		// get constructor lambda
		auto constructorLambda = converter.getFunMan()->lookup(constructExpr->getConstructor());

		// return call
		auto retType = constructorLambda->getType().as<core::FunctionTypePtr>()->getReturnType();
		return utils::buildCxxMethodCall(converter, retType, constructorLambda, memLoc, constructExpr->arguments());
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

