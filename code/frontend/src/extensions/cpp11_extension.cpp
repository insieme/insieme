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
 *			Cxx11 null pointer
 */
insieme::core::ExpressionPtr Cpp11Plugin::VisitCXXNullPtrLiteralExpr	(const clang::CXXNullPtrLiteralExpr* nullPtrExpr,
												 insieme::frontend::conversion::Converter& convFact){
	auto builder = convFact.getIRBuilder();
	insieme::core::ExpressionPtr retIr;
	insieme::core::TypePtr type = convFact.convertType(GET_TYPE_PTR(nullPtrExpr));
	assert(type->getNodeType() != insieme::core::NT_ArrayType && "C pointer type must of type array<'a,1>");
	return (retIr = builder.refReinterpret(convFact.getNodeManager().getLangBasic().getRefNull(), type));
}

/**
 *  			Cxx11 lambda expression
 */
insieme::core::ExpressionPtr Cpp11Plugin::VisitLambdaExpr (const clang::LambdaExpr* lambdaExpr, insieme::frontend::conversion::Converter& convFact) {
	//auto builder = convFact.getIRBuilder();
	auto& mgr = convFact.getNodeManager();
	insieme::core::ExpressionPtr retIr;

	// convert the enclosing class
	const clang::CXXRecordDecl* decl = llvm::cast<clang::CXXRecordDecl>(lambdaExpr->getLambdaClass());
	insieme::core::TypePtr lambdaClassIR = convFact.convertType(decl->getTypeForDecl());

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


	insieme::core::ExpressionPtr init = insieme::core::encoder::toIR(mgr, captures);
	return retIr = convFact.getInitExpr (lambdaClassIR, init);
;
}

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 types

/**
 * auto type
 */
insieme::core::TypePtr Cpp11Plugin::VisitAutoType(const clang::AutoType* autoTy, insieme::frontend::conversion::Converter& convFact) {
	return convFact.convertType(autoTy->getDeducedType().getTypePtr());
}

/**
 * decltype(E) is the type ("declared type") of the name or expression E and can be used in declarations.
 */
insieme::core::TypePtr Cpp11Plugin::VisitDecltypeType(const clang::DecltypeType* declTy, insieme::frontend::conversion::Converter& convFact) {
	insieme::core::TypePtr retTy;
	assert(declTy->getUnderlyingExpr());
	retTy = convFact.convertExpr(declTy->getUnderlyingExpr ())->getType();
	return retTy;
}


///////////////////////////////////////////////////////////////////////////////////////
//  Decls post visit

void Cpp11Plugin::PostVisit(const clang::Decl* decl,  insieme::frontend::conversion::Converter& convFact) {
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

				core::StringValuePtr ident = builder.stringValue("__m"+insieme::utils::numeric_cast<std::string>(id));
				core::ExpressionPtr access =  builder.callExpr (var->getType(),
														  builder.getLangBasic().getCompositeRefElem(), thisExpr,
														  builder.getIdentifierLiteral(ident), builder.getTypeLiteral(var->getType()));
				replacements[var] = access;
				id++;
			}

			// update implementation
			irFunc = insieme::core::transform::replaceAllGen(builder.getNodeManager(), irFunc, replacements, false );
			convFact.getIRTranslationUnit().replaceFunction(symb.as<insieme::core::LiteralPtr>(), irFunc.as<core::LambdaExprPtr>());

			// update the meta info of the class
			core::TypePtr classType = thisExpr->getType().as<core::RefTypePtr>().getElementType();
			classType = convFact.lookupTypeDetails(classType);
			core::ClassMetaInfo classInfo = core::getMetaInfo(classType);
			const vector<core::MemberFunction>& old = classInfo.getMemberFunctions();
			assert(old.size() ==1);
			vector<core::MemberFunction> newFuncs;
			newFuncs.push_back ( core::MemberFunction(old[0].getName(), irFunc, old[0].isVirtual(), old[0].isConst()) );
			classInfo.setMemberFunctions(newFuncs);
			core::setMetaInfo(classType, classInfo);

			// clean map
			lambdaMap.erase(method);
		}
	}
}


} //namespace plugin
} //namespace frontnt
} //namespace extensions
