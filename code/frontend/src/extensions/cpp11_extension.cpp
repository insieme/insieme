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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
	#include "clang/AST/StmtVisitor.h"
	#include <clang/AST/Expr.h>
	#include <clang/AST/DeclCXX.h>
	#include <clang/AST/ExprCXX.h>
	#include <clang/AST/CXXInheritance.h>

	#include <clang/Basic/FileManager.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/extensions/cpp11_extension.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/ir++_extension.h"

#include "insieme/core/ir_class_info.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/encoder/lists.h"

using namespace insieme::frontend;

namespace insieme {
namespace frontend {
namespace extensions {

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 expressions

/**
 *			Cxx11 default init expression
 */
insieme::core::ExpressionPtr Cpp11Extension::VisitCXXDefaultInitExpr (const clang::CXXDefaultInitExpr* initExpr,
												insieme::frontend::conversion::Converter& convFact) {
    return convFact.convertExpr(initExpr->getExpr());
}


/**
 *			Cxx11 null pointer
 */
insieme::core::ExpressionPtr Cpp11Extension::VisitCXXNullPtrLiteralExpr	(const clang::CXXNullPtrLiteralExpr* nullPtrExpr,
												 insieme::frontend::conversion::Converter& convFact){
	auto builder = convFact.getIRBuilder();
	insieme::core::ExpressionPtr retIr;
	insieme::core::TypePtr type = convFact.convertType(nullPtrExpr->getType());
	assert_ne(type->getNodeType(), insieme::core::NT_ArrayType) << "C pointer type must of type array<'a,1>";
	return (retIr = builder.refReinterpret(convFact.getNodeManager().getLangBasic().getRefNull(), type));
}

/**
 *  			Cxx11 lambda expression
 */
insieme::core::ExpressionPtr Cpp11Extension::VisitLambdaExpr (const clang::LambdaExpr* lambdaExpr, insieme::frontend::conversion::Converter& convFact) {
	//auto builder = convFact.getIRBuilder();
	auto& mgr = convFact.getNodeManager();
	insieme::core::ExpressionPtr retIr;

	// convert the enclosing class
	const clang::CXXRecordDecl* decl = llvm::cast<clang::CXXRecordDecl>(lambdaExpr->getLambdaClass());
	insieme::core::TypePtr lambdaClassIR = convFact.convertType(decl->getTypeForDecl()->getCanonicalTypeInternal());

	// convert the captures
	auto captureIt  = lambdaExpr->capture_init_begin();
	auto captureEnd = lambdaExpr->capture_init_end();
	std::vector<insieme::core::ExpressionPtr> captures;
	for (;captureIt != captureEnd; ++captureIt){
		captures.push_back(convFact.convertExpr(*captureIt));
	}



	core::ExpressionPtr symb = convFact.getCallableExpression(lambdaExpr->getCallOperator ());
	frontend_assert(symb.isa<core::LiteralPtr>()) << "no literal?";
	lambdaMap.insert({lambdaExpr->getCallOperator(), lambdaExpr});


	insieme::core::ExpressionPtr init = insieme::core::encoder::toIR<ExpressionList, core::encoder::DirectExprListConverter>(mgr, captures);
	return retIr = convFact.getInitExpr (lambdaClassIR, init);
;
}


/**
 *  			Cxx11 size of pack expression
 */
insieme::core::ExpressionPtr Cpp11Extension::VisitSizeOfPackExpr(const clang::SizeOfPackExpr* sizeOfPackExpr, insieme::frontend::conversion::Converter& convFact) {
	//sizeOf... returns size_t --> use unsigned int
	core::ExpressionPtr retExpr = convFact.getIRBuilder().uintLit(sizeOfPackExpr->getPackLength());
	return retExpr;
}


/**
 *  			Cxx11 init list expression
 */
insieme::core::ExpressionPtr Cpp11Extension::VisitInitListExpr(const clang::CXXStdInitializerListExpr* initList, insieme::frontend::conversion::Converter& convFact) {
    //get the sub expression of the std init list expression
    auto expr = initList->getSubExpr();
    auto builder = convFact.getIRBuilder();
    auto& mgr = convFact.getNodeManager();
    auto& ext = mgr.getLangExtension<insieme::core::lang::IRppExtensions>();

    //we must have a materialize below the cxxstdinitlistexpr
    if(llvm::dyn_cast<clang::MaterializeTemporaryExpr>(expr)) {
        //get the materialize
        auto materialize = llvm::cast<clang::MaterializeTemporaryExpr>(expr);
        //if we have an non builtin element type there might be a cxxbindtemporaryexpr below
        //but at least we know that a initlistexpr is somewhere. either directly after the materializeexpr
        //or after the cxxbindtemporary
        const clang::InitListExpr* innerInitList;
        //if we have a bind temporary, extract the inner init list expression
        //else get it directly from the materialize expression
        if(llvm::dyn_cast<clang::CXXBindTemporaryExpr>(materialize->GetTemporaryExpr())) {
            auto bindExpr = llvm::cast<clang::CXXBindTemporaryExpr>(materialize->GetTemporaryExpr());
            innerInitList = llvm::cast<clang::InitListExpr>(bindExpr->getSubExpr());
        } else {
            innerInitList = llvm::cast<clang::InitListExpr>(materialize->GetTemporaryExpr());
        }
        //perfect, convert the inner list, create a literal out of it and return
        auto retExpr = convFact.convertExpr(innerInitList);
        return builder.callExpr(ext.getStdInitListExpr(), retExpr, builder.getTypeLiteral(convFact.convertType(initList->getType())));
    }
    assert_fail() << "failed to convert a CXXStdInitializerListExpr";
    return nullptr;
}


//////////////////////////////////////////////////////////////////////////////////////
//               C++11 types

/**
 * auto type
 */
insieme::core::TypePtr Cpp11Extension::VisitAutoType(const clang::AutoType* autoTy, insieme::frontend::conversion::Converter& convFact) {
	return convFact.convertType(autoTy->getDeducedType());
}

/**
 * decltype(E) is the type ("declared type") of the name or expression E and can be used in declarations.
 */
insieme::core::TypePtr Cpp11Extension::VisitDecltypeType(const clang::DecltypeType* declTy, insieme::frontend::conversion::Converter& convFact) {
	insieme::core::TypePtr retTy;
	assert_true(declTy->getUnderlyingExpr());
	retTy = convFact.convertExpr(declTy->getUnderlyingExpr ())->getType();
	return retTy;
}

insieme::core::TypePtr Cpp11Extension::VisitRValueReferenceType(const clang::RValueReferenceType* rvalref, insieme::frontend::conversion::Converter& convFact) {
    core::TypePtr innerTy = convFact.convertType(rvalref->getPointeeType());
    bool isConst = rvalref->getPointeeType().isConstQualified();
	core::TypePtr ret;
    if(isConst)
        ret = core::analysis::getConstRValCppRef(innerTy);
    else
        ret = core::analysis::getRValCppRef(innerTy);
	return ret;
}


///////////////////////////////////////////////////////////////////////////////////////
//  Decls post visit
core::ExpressionPtr Cpp11Extension::FuncDeclPostVisit(const clang::FunctionDecl* decl, core::ExpressionPtr expr, frontend::conversion::Converter& convFact, bool symbolic) {
	if(!symbolic) {
		if (const clang::CXXMethodDecl* method= llvm::dyn_cast<clang::CXXMethodDecl>(decl)){
		// we need to substitute any captured usage name by the reference to the local copy
		// 		- retrieve the operator() (.. )  func
		// 		- create this->_mX access for captured vars
		// 		- substitute every usage by member access

			// if is the declaration of a lambda that has being processed before
			auto fit = lambdaMap.find(method);
			if (fit != lambdaMap.end()){
				auto builder = convFact.getIRBuilder();

				// retrieve temporal implementation
				//core::ExpressionPtr symb = expr.as<core::LiteralPtr>();
				core::ExpressionPtr symb = convFact.getCallableExpression(method);
				assert(symb.isa<core::LiteralPtr>());
				core::ExpressionPtr irFunc = convFact.lookupFunctionImpl(symb);
				assert(irFunc.isa<core::LambdaExprPtr>());
				insieme::core::ExpressionPtr thisExpr = irFunc.as<core::LambdaExprPtr>()->getParameterList()[0];

				// build replacements for captured vars
				insieme::core::NodeMap replacements;
				clang::LambdaExpr::capture_iterator cap_it = fit->second->capture_begin();
				clang::LambdaExpr::capture_iterator cap_end= fit->second->capture_end();
				unsigned id(0);
				for (;cap_it != cap_end; ++cap_it){
					auto var = convFact.lookUpVariable(cap_it->getCapturedVar());
                    if(llvm::dyn_cast<clang::ParmVarDecl>(cap_it->getCapturedVar())) {
                        var = convFact.lookUpVariableInWrapRefMap(var);
                        if(core::analysis::isCppRef(var->getType()))
                            var = core::analysis::unwrapCppRef(var);
                    }



					core::StringValuePtr ident = builder.stringValue("__m"+insieme::utils::numeric_cast<std::string>(id));
					core::ExpressionPtr access;
					//now we have to create the access to the lambda struct
					//if thisExpr is not a struct type use the fallback method
					//otherwise call the builder accessMember method to create the call
					auto typeCheck = [](const core::TypePtr ty)->bool {
					    if(ty.isa<core::RefTypePtr>())
                            if(core::analysis::getReferencedType(ty).isa<core::NamedCompositeTypePtr>())
                                return true;
                        if(ty.isa<core::NamedCompositeTypePtr>())
                            return true;
                        return false;
					};
					if(!typeCheck(thisExpr->getType())) {
                        access =  builder.callExpr (var->getType(),
                                                    builder.getLangBasic().getCompositeRefElem(), thisExpr,
                                                    builder.getIdentifierLiteral(ident),
                                                    builder.getTypeLiteral(var->getType()));

					} else {
                        access = builder.accessMember(thisExpr, ident);
					}
					//check if we have a cpp ref or a ref<cpp ref> internally
					//if true we have to replace the uses of the old
					//variable (e.g., *v1) with (e.g., *RefCppToIR(*v_new))
                    if(core::analysis::isCppRef(access->getType()) ||
                        (core::analysis::isRefType(access->getType()) &&
                         core::analysis::isCppRef(core::analysis::getReferencedType(access->getType())))) {
                        access = core::analysis::unwrapCppRef(access);
                    }
					replacements[var] = access;
					id++;
				}

				// update implementation
				irFunc = insieme::core::transform::replaceAllGen(builder.getNodeManager(), irFunc, replacements, false );
				convFact.getIRTranslationUnit().replaceFunction(symb.as<insieme::core::LiteralPtr>(), irFunc.as<core::LambdaExprPtr>());

				// clean map
				lambdaMap.erase(method);
			}
		}
	}
	return nullptr;
}


stmtutils::StmtWrapper Cpp11Extension::VisitCXXForRangeStmt(const clang::CXXForRangeStmt* frStmt, frontend::conversion::Converter& convFact) {
	auto builder = convFact.getIRBuilder();

	const clang::DeclStmt* rangeStmt 		= frStmt->getRangeStmt ();
	const clang::DeclStmt* beginStmt 		= frStmt->getBeginEndStmt ();
	const clang::Expr* cond				= frStmt->getCond ();
	const clang::Expr* inc 				= frStmt->getInc ();
	const clang::DeclStmt* loopVarStmt	= frStmt->getLoopVarStmt ();
	const clang::Stmt* body 				= frStmt->getBody ();

	core::StatementPtr range = convFact.convertStmt(rangeStmt);
	core::StatementPtr begin = convFact.convertStmt(beginStmt);
	core::ExpressionPtr condIr = convFact.convertExpr(cond);
	core::ExpressionPtr incIr = convFact.convertExpr(inc);
	core::StatementPtr loopVarStmtIr = convFact.convertStmt(loopVarStmt);
	core::StatementPtr  bodyIr = convFact.convertStmt(body);

	StatementList stmts;
	stmts.push_back(loopVarStmtIr);
	stmts.push_back(bodyIr);
	stmts.push_back(incIr);
	core::CompoundStmtPtr fullBody = builder.compoundStmt(stmts);
	core::StatementPtr whileStmt = builder.whileStmt(condIr, fullBody.as<core::StatementPtr>());


	stmtutils::StmtWrapper res;
	assert(begin.isa<core::CompoundStmtPtr>());
	res.push_back(range);
	res.push_back(begin.as<core::CompoundStmtPtr>()[0]);
	res.push_back(begin.as<core::CompoundStmtPtr>()[1]);
	res.push_back(whileStmt);
	return res;
}

FrontendExtension::flagHandler Cpp11Extension::registerFlag(insieme::driver::cmd::detail::OptionParser& optParser) {
    //create lambda
    auto lambda = [&](const ConversionJob& job) {
        return (job.getStandard() == ConversionSetup::Standard::Cxx11);
    };
    return lambda;
}

} //namespace extension
} //namespace frontend
} //namespace insieme
