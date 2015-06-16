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

#include "insieme/frontend/extensions/variadic_arguments_extension.h"
#include "insieme/frontend/expr_converter.h"

#include "insieme/core/lang/varargs_extension.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/annotations/c/include.h"

#include <boost/algorithm/string/predicate.hpp>

namespace insieme {
namespace frontend {
namespace extensions {

using namespace insieme;

core::ExpressionPtr VariadicArgumentsExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact) {
	const clang::VAArgExpr*  vaargexpr = (llvm::isa<clang::VAArgExpr>(expr)) ? llvm::cast<clang::VAArgExpr>(expr) : nullptr;
	if(vaargexpr)
	{
		core::IRBuilder builder = convFact.getIRBuilder();
		const auto& builderExt = convFact.getNodeManager().getLangExtension<core::lang::VarArgsExtension>();

		core::ExpressionPtr firstArg = convFact.convertExpr(vaargexpr->getSubExpr());
		core::ExpressionList args;
		args.push_back(firstArg);
		const clang::QualType varTy = vaargexpr->getType();
		core::TypePtr&& irType = convFact.convertType( varTy );
		args.push_back(builder.getTypeLiteral(irType));

		return builder.callExpr(irType, builderExt.getVaarg(), args);
	}

	return nullptr;
}

core::ExpressionPtr  VariadicArgumentsExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
												   insieme::frontend::conversion::Converter& convFact) {
	const clang::CallExpr* callexpr = (llvm::isa<clang::CallExpr>(expr)) ? llvm::cast<clang::CallExpr>(expr) : nullptr;

	if(callexpr && callexpr->getDirectCallee()) {
		core::ExpressionPtr funExpr;
		core::CallExprPtr callExpr;
		//if the call expression is wrapped by a marker expression
		if(irExpr.isa<core::MarkerExprPtr>()) {
			//extract the call expression
			callExpr = irExpr.as<core::MarkerExprPtr>()->getSubExpression().as<core::CallExprPtr>();
		} else {
			callExpr = irExpr.as<core::CallExprPtr>();
		}
		funExpr = callExpr->getFunctionExpr();

		core::IRBuilder builder = convFact.getIRBuilder();

		if(callexpr->getDirectCallee()->isVariadic()) {
			// dividing common arguments from variadic ones

			auto type = funExpr->getType().as<core::FunctionTypePtr>();
			auto parameterTypes = type->getParameterTypeList();

			auto args = callExpr->getArguments();
			core::ExpressionList varArgs(args.begin() + parameterTypes.size() -1, args.end());

			// building new arguments list with packed variadic arguments

			core::ExpressionList newArgs(args.begin(), args.begin() + parameterTypes.size() -1);
			newArgs.push_back(builder.callExpr(builder.getLangBasic().getVarList(), builder.getLangBasic().getVarlistPack(), builder.tupleExpr(varArgs)));

			auto retCall = builder.callExpr(funExpr->getType().as<core::FunctionTypePtr>()->getReturnType(), funExpr, newArgs);

			//if the original expression was wrapped inside a marker expression
			if(irExpr.isa<core::MarkerExprPtr>()) {
				//again pack it into a marker expression
				auto marker = irExpr.as<core::MarkerExprPtr>();
				auto ret = builder.markerExpr(marker->getID(), retCall);
				//and also migrate all the annotations
				core::transform::utils::migrateAnnotations(marker, ret);
				return ret;
			}
			return retCall;
		}
	}

	return irExpr;
}

core::TypePtr  VariadicArgumentsExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& convFact) {
	if(const clang::RecordType * tt = llvm::dyn_cast<clang::RecordType>(type)) {
		if(tt->getDecl()->getNameAsString().find("va_list") != std::string::npos) {
			auto irType = convFact.getNodeManager().getLangExtension<core::lang::VarArgsExtension>().getValist();
			convFact.addToTypeCache(type, irType);
			return irType;
		}
	}

	return nullptr;
}

core::TypePtr  VariadicArgumentsExtension::PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
											 insieme::frontend::conversion::Converter& convFact) {

	 // build the right function type for variadic functions, we have to extend the parameter list with an extra TYPE
	if(const clang::FunctionProtoType * funType = llvm::dyn_cast<clang::FunctionProtoType>(type.getTypePtr())) {
		if(funType->isVariadic()) {

			core::IRBuilder builder = convFact.getIRBuilder();

			auto irFunType = irType.as<core::FunctionTypePtr>();
			assert_true(irFunType) << "Type is not a FuntionType";

			auto parameterTypes = irFunType->getParameterTypeList();

			// if we've already handled it
			if (builder.getLangBasic().isVarList(parameterTypes.back()))
				return irType;

			// append VarList argument

			parameterTypes.push_back(builder.getLangBasic().getVarList());
			auto newFuncType = builder.functionType(parameterTypes, irFunType->getReturnType(), irFunType->getKind());
			convFact.addToTypeCache(type, newFuncType);

			return newFuncType;
		}
	}

	return irType;
}

insieme::core::ExpressionPtr VariadicArgumentsExtension::FuncDeclPostVisit(const clang::FunctionDecl* decl, core::ExpressionPtr expr, insieme::frontend::conversion::Converter& convFact, bool symbolic) {
	if(!symbolic) {
		if(decl->isVariadic())
		{
			core::IRBuilder builder = convFact.getIRBuilder();

			core::ExpressionPtr symb = convFact.convertFunctionDecl(decl);
			assert(symb.isa<core::LiteralPtr>());

			core::ExpressionPtr fe = convFact.getIRTranslationUnit()[symb.as<core::LiteralPtr>()];
			if(!fe) {
				// this is an intercepted function
				return nullptr;
			}

			core::VariableList params = fe.as<core::LambdaExprPtr>()->getParameterList();
			core::VariablePtr var = builder.variable(builder.getLangBasic().getVarList());

			// if we've already handled it
			if (!params.empty() && params.back().as<core::VariablePtr>()->getType() == var->getType())
				return nullptr;

			params.push_back(var);

			auto body = fe.as<core::LambdaExprPtr>()->getBody();
			auto funcTy = fe->getType();
			auto lambda = builder.lambdaExpr(funcTy.as<core::FunctionTypePtr>(), params, body.as<core::CompoundStmtPtr>());

			assert_true(lambda);
			assert(lambda->getType() == symb->getType());

			convFact.getIRTranslationUnit().replaceFunction(symb.as<core::LiteralPtr>(), lambda);
		}
	} 
	return nullptr;
}

insieme::core::ProgramPtr  VariadicArgumentsExtension::IRVisit(insieme::core::ProgramPtr& prog){

	core::IRBuilder builder (prog->getNodeManager());
	core::NodeManager& mgr = prog->getNodeManager();

	core::TypePtr vaListTy = mgr.getLangExtension<core::lang::VarArgsExtension>().getValist();
	core::TypePtr vectorVaList = builder.vectorType( vaListTy, core::ConcreteIntTypeParam::get(mgr, 1));
	core::TypePtr arrayVaList = builder.refType(builder.arrayType( vaListTy));

	// fix types, change usage
	core::NodeMap replacements;
	replacements [ arrayVaList ]  = vaListTy;
	replacements [ vectorVaList ] = vaListTy;
	prog = core::transform::replaceAllGen (mgr, prog, replacements, false);

	// collect and remove wrongly typed vector inits, happens when using var. args in c++
	auto vectorInitCleanup = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node)-> core::NodePtr{
				if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
					core::IRBuilder builder (node->getNodeManager());
					const auto& gen = node->getNodeManager().getLangBasic();

					if (core::analysis::isCallOf(call, gen.getVectorInitUniform())){
						if (call->getType() == vaListTy)
							return call[0];
					}
				}

				return node;
			});
	prog = vectorInitCleanup.map(prog);

	// cleanup the not needed casts
	auto castRemover = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node)-> core::NodePtr{
				if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
					core::IRBuilder builder (node->getNodeManager());
					const auto& gen = node->getNodeManager().getLangBasic();

					if (core::analysis::isCallOf(call, gen.getRefVectorToRefArray())){
						if (call[0]->getType() == builder.refType(vaListTy))
							return builder.deref(call[0]);
					}
				}

				return node;
			});
	prog = castRemover.map(prog);

	// rename all __builtin_va functions (just remove the __builtin prefix for this ones)
	auto literalRenamer = core::transform::makeCachedLambdaMapper([&](const core::NodePtr& node)-> core::NodePtr{
				if (core::LiteralPtr lit = node.isa<core::LiteralPtr>()){
					if (boost::starts_with(lit->getStringValue(), "__builtin_va_" )){
						core::IRBuilder builder (lit->getNodeManager());
						core::LiteralPtr res = builder.literal(lit->getStringValue().substr(10), lit->getType());
						insieme::annotations::c::attachInclude(res, "stdarg.h");
						return res;
					}
				}

				return node;
			});
	prog = literalRenamer.map(prog);

	return prog;
}

} // extensions
} // frontend
} // insieme
