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
	auto builder = convFact.getIRBuilder();
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


	// we need to substitute any captured usage name by the reference to the local copy
	// 		- retrieve the operator() (.. )  func
	// 		- create this->_mX access for captured vars
	// 		- substitute every usage by member access
	const insieme::core::ClassMetaInfo&  metainfo = insieme::core::getMetaInfo(lambdaClassIR);
	std::vector<insieme::core::MemberFunctionPtr> functionals = metainfo.getMemberFunctionOverloads("operator()");

	for (auto cur : functionals){
		if(cur->isVirtual()){
			continue;
		}
		// in the meta information we only store a symbol, the actual implementation is stored in the translation unit
		insieme::core::ExpressionPtr symb = cur->getImplementation();
		assert(symb);
		insieme::core::LambdaExprPtr membFunction;
		if  (symb.isa<insieme::core::LiteralPtr>()){
			 membFunction = convFact.getIRTranslationUnit()[symb.as<insieme::core::LiteralPtr>()];
		}
		else if (symb.isa<insieme::core::LambdaExprPtr>()){
			membFunction = symb.as<insieme::core::LambdaExprPtr>();
			assert(false);
		}
		else {
			assert(false && "not a func, not a literal, u tell me what is this" );
		}
		insieme::core::ExpressionPtr thisExpr = membFunction->getParameterList()[0];

		// for each capture, prepare a substitute
		insieme::core::NodeMap replacements;
		unsigned id(0);
		for (auto capExpr : captures){

			insieme::core::VariableList vars;
			visitDepthFirstOnce(capExpr, [this, &vars] (const insieme::core::VariablePtr& var){ vars.push_back(var);});
			assert(vars.size() ==1 && "more than one variable in expression?");
			insieme::core::VariablePtr var = vars[0];

			// build anonymous member access
			insieme::core::StringValuePtr ident = builder.stringValue("__m"+insieme::utils::numeric_cast<std::string>(id));
			insieme::core::ExpressionPtr access =  builder.callExpr (var->getType(),
													  builder.getLangBasic().getCompositeRefElem(), thisExpr,
													  builder.getIdentifierLiteral(ident), builder.getTypeLiteral(var->getType()));
			replacements[var] = access;
			id++;
		}

		// replace variables usage and update function implementation in the TU
		membFunction = insieme::core::transform::replaceAllGen(builder.getNodeManager(), membFunction, replacements, false );

		if  (symb.isa<insieme::core::LiteralPtr>()){
			convFact.getIRTranslationUnit().replaceFunction(symb.as<insieme::core::LiteralPtr>(), membFunction);
		}
		//core::setMetaInfo(lambdaClassIR, metainfo);
	}
	// restore new meta info

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
