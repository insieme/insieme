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

#pragma once

#include "insieme/frontend/extensions/frontend_plugin.h"

#include "insieme/frontend/expr_converter.h"

using namespace insieme;

class VariadicArgumentsPlugin : public insieme::frontend::extensions::FrontendPlugin
{ 
    bool isStdargBuiltin(const std::string& name)
    {
            return (name.find("__builtin_va") != std::string::npos) ? true : false;
    }       

    virtual core::ExpressionPtr Visit(const clang::Expr* expr,
                                                       insieme::frontend::conversion::Converter& convFact) 
    { 
        const clang::VAArgExpr* vaargexpr = (llvm::isa<clang::VAArgExpr>(expr)) ? llvm::cast<clang::VAArgExpr>(expr) : nullptr;
        if(vaargexpr)
        {
                VLOG(2) << "VISIT EXPR";
            core::IRBuilder builder = convFact.getIRBuilder();

            core::ExpressionPtr firstArg = convFact.convertExpr(vaargexpr->getSubExpr());
            core::ExpressionList args;
            args.push_back(firstArg);
            const clang::QualType varTy = vaargexpr->getType();
            core::TypePtr&& irType = convFact.convertType( varTy.getTypePtr() );

            auto type = builder.functionType(
                    toVector<core::TypePtr>(builder.refType(builder.arrayType(builder.genericType("va_list")))),// builder.typeVariable("a")), 
                    irType);
            auto lit = builder.literal("va_arg", type.as<core::FunctionTypePtr>());

            return builder.callExpr(lit, args);
        }

        return nullptr;
    }
    virtual core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                                       insieme::frontend::conversion::Converter& convFact) 
    { 
        const clang::CallExpr* callexpr = (llvm::isa<clang::CallExpr>(expr)) ? llvm::cast<clang::CallExpr>(expr) : nullptr;

        if(callexpr && callexpr->getDirectCallee() && callexpr->getDirectCallee()->isVariadic())
        {
                VLOG(1) << "POST VISIT EXPR";
            core::IRBuilder builder = convFact.getIRBuilder();

            auto funExpr = irExpr.as<core::CallExprPtr>()->getFunctionExpr();

            // dividing common arguments from variadic ones 
            
            auto type = funExpr->getType().as<core::FunctionTypePtr>();
            auto parameterTypes = type->getParameterTypeList();    

            auto args = irExpr.as<core::CallExprPtr>()->getArguments();
            core::ExpressionList varArgs(args.begin() + parameterTypes.size() -1, args.end());

            // building new arguments list with packed variadic arguments

            core::ExpressionList newArgs(args.begin(), args.begin() + parameterTypes.size() -1);
            newArgs.push_back(builder.callExpr(builder.getLangBasic().getVarList(), builder.getLangBasic().getVarlistPack(), builder.tupleExpr(varArgs))); 

            return  builder.callExpr(funExpr->getType().as<core::FunctionTypePtr>()->getReturnType(), funExpr, newArgs);
        }

        return irExpr;
    }

    virtual bool Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact)
    {
        if(const clang::FunctionDecl *fd = llvm::dyn_cast<clang::FunctionDecl>(decl))
        {
                VLOG(1) << "VISIT DECL";
            core::IRBuilder builder = convFact.getIRBuilder();
            core::LiteralPtr lit;

            // handling builtins 

            if(fd->getNameAsString().find("va_start") != string::npos)
            {
                auto newType = builder.functionType(toVector<core::TypePtr>(builder.refType(builder.arrayType(builder.genericType("va_list"))), builder.getLangBasic().getVarList()), builder.getLangBasic().getUnit());
                lit = builder.literal("va_start", newType.as<core::FunctionTypePtr>());
            }
            else if(fd->getNameAsString().find("va_end") != string::npos)
            {
                auto newType = builder.functionType(toVector<core::TypePtr>(builder.refType(builder.arrayType(builder.genericType("va_list")))), builder.getLangBasic().getUnit());
                lit = builder.literal("va_end", newType.as<core::FunctionTypePtr>());
            }
            else if(fd->getNameAsString().find("va_copy") != string::npos)
            {
                auto newType = builder.functionType(toVector<core::TypePtr>(builder.refType(builder.arrayType(builder.genericType("va_list"))), builder.refType(builder.arrayType(builder.genericType("va_list")))), builder.getLangBasic().getUnit());
                lit = builder.literal("va_copy", newType.as<core::FunctionTypePtr>());
            }
            //else if(fd->getNameAsString().find("va_arg") != string::npos)
            //{
            //        LOG(INFO) << "DUMP ";
            //        fd->dump() ;
            //    auto newType = builder.functionType(toVector<core::TypePtr>(builder.refType(builder.arrayType(builder.genericType("va_list"))), builder.genericType("va_arg_type", insieme::core::TypeList(builder.typeVariable("a")), insieme::core::IntParamList()), builder.getLangBasic().getInt4()));
            //    lit = builder.literal("va_arg", newType.as<core::FunctionTypePtr>());
            //}
            else {
                    // Do not handle anything else
                    return false;
            }

            convFact.addToLambdaCache(fd, lit);

            return true;
        }

        return false;
    }

    virtual core::TypePtr Visit(const clang::Type* type, insieme::frontend::conversion::Converter& convFact)
    {
        if(const clang::RecordType * tt = llvm::dyn_cast<clang::RecordType>(type)) {
            if(tt->getDecl()->getNameAsString().find("va_list") != std::string::npos) {
                VLOG(1) << "VISIT TYPE";
                auto irType = convFact.getIRBuilder().genericType("va_list");
                convFact.addToTypeCache(type, irType);
                return irType;
            }
        }

        return nullptr;
    }

    virtual core::TypePtr PostVisit(const clang::Type* type, const insieme::core::TypePtr& irType,
                                                 insieme::frontend::conversion::Converter& convFact) 
    {
        if(const clang::FunctionProtoType * funType = llvm::dyn_cast<clang::FunctionProtoType>(type)) {
            if(funType->isVariadic()) {
                VLOG(1) << "POST VISIT TYPE";

                core::IRBuilder builder = convFact.getIRBuilder();
            
                auto irFunType = irType.as<core::FunctionTypePtr>();
                assert(irFunType && "Type is not a FuntionType");

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

    virtual void PostVisit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact)
    {

		if (const clang::FunctionDecl* fd = llvm::dyn_cast<clang::FunctionDecl>(decl)){
			if(fd->isVariadic())
			{
					VLOG(1) << "POST VISIT DECL";
				core::IRBuilder builder = convFact.getIRBuilder();

				core::ExpressionPtr symb = convFact.convertFunctionDecl(fd);
				assert(symb.isa<core::LiteralPtr>());

				core::ExpressionPtr fe = convFact.getIRTranslationUnit()[symb.as<core::LiteralPtr>()];
				if(!fe) {
					// this is an intercepted function
					return;
				}

				core::VariableList params = fe.as<core::LambdaExprPtr>()->getParameterList();
				core::VariablePtr var = builder.variable(builder.getLangBasic().getVarList());

				// if we've already handled it
				if (!params.empty() && params.back().as<core::VariablePtr>()->getType() == var->getType())
					return;

				params.push_back(var);

				auto body = fe.as<core::LambdaExprPtr>()->getBody();
				auto funcTy = fe->getType();
				auto lambda = builder.lambdaExpr(funcTy.as<core::FunctionTypePtr>(), params, body.as<core::CompoundStmtPtr>());

				assert(lambda);
				assert(lambda->getType() == symb->getType());

				convFact.getIRTranslationUnit().replaceFunction(symb.as<core::LiteralPtr>(), lambda);
			}
		}
	}

};
