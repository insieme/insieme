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
    virtual core::ExpressionPtr Visit(const clang::Expr* expr, frontend::conversion::Converter& convFact)
    {
        if(llvm::isa<clang::CallExpr>(expr))
        {
            core::IRBuilder builder = convFact.getIRBuilder();
            const clang::CallExpr* callexpr = llvm::cast<clang::CallExpr>(expr);
            if(!callexpr->getDirectCallee())
                return nullptr;
            //find the correct name for the builtin
            std::string name = callexpr->getDirectCallee()->getNameAsString();
            bool found=false;
            if(name.find("va_start") != std::string::npos)
            {
                name = "va_start";
                found=true;
            }
            if(name.find("va_end") != std::string::npos)
            {
                name = "va_end";
                found=true;
            }
            if(name.find("va_arg") != std::string::npos)
            {
                name = "va_arg";
                found=true;
            }
            if(name.find("va_copy") != std::string::npos)
            {
                name = "va_copy";
                found=true;
            }
            if(!found)
                return nullptr;

            //convert it with the standard expression visitor
            core::ExpressionPtr ex = convFact.getExprConverter()->VisitCallExpr(callexpr);
            assert(ex.isa<core::CallExprPtr>() && "this is no call expression.");

            auto funExpr = ex.as<core::CallExprPtr>()->getFunctionExpr();
            auto args = ex.as<core::CallExprPtr>()->getArguments();
            vector<core::ExpressionPtr> newArgs(args);

            //if(core::analysis::isCallOf(args[0], gen.getRefVectorToRefArray()))
            //newArgs[0] = args[0].as<core::CallExprPtr>()->getArguments();

            auto type = funExpr->getType().as<core::FunctionTypePtr>();
            auto typeAddr = core::FunctionTypeAddress(type);
            auto paramTypeAddr = typeAddr->getParameterType(0);
            auto newFunType = insieme::core::transform::replaceNode(builder.getNodeManager(), paramTypeAddr, builder.refType(builder.genericType("va_list")));
            auto lit = builder.literal(name,newFunType.as<core::FunctionTypePtr>());

            auto argument = args[0];
            auto argumentAddress = core::ExpressionAddress(argument);
            auto argumentTypeAddress = argumentAddress->getType();
            auto newArg = insieme::core::transform::replaceNode(builder.getNodeManager(), argumentTypeAddress, builder.refType(builder.genericType("va_list")));

            newArgs[0] = newArg.as<core::ExpressionPtr>();

            ex = builder.callExpr(type->getReturnType(), lit, newArgs);
            return ex;
        }
        return nullptr;
    }

    virtual bool Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact)
    {
        if(const clang::TypeDecl * d = llvm::dyn_cast<clang::TypeDecl>(decl))
        {
            if(d->getNameAsString().find("va_list") != std::string::npos)
            {
                convFact.addToTypeCache(d->getTypeForDecl(), convFact.getIRBuilder().genericType("va_list"));
                return true;
            }
        }
        return false;
    }
};
