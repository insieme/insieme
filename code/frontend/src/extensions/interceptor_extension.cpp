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

#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/core/transform/manipulation_utils.h"

namespace insieme {
namespace frontend {
namespace extensions {

	insieme::core::ExpressionPtr InterceptorPlugin::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact) {
		if(const clang::DeclRefExpr* declRefExpr = llvm::dyn_cast<clang::DeclRefExpr>(expr) ) {
			if( const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(declRefExpr->getDecl()) ) {
				if(declRefExpr->hasExplicitTemplateArgs() && convFact.getInterceptor().isIntercepted(funcDecl)) {
					VLOG(2) << "interceptorplugin\n";
					//get a callable expression
					return convFact.getInterceptor().intercept(funcDecl, convFact, true);
				}
			}
		}
		return nullptr;
	}
        
    bool InterceptorPlugin::Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact) {
		if(const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
			// check whether function should be intercected
			if( convFact.getInterceptor().isIntercepted(funcDecl) ) {
				auto irExpr = convFact.getInterceptor().intercept(funcDecl, convFact);
				convFact.addToLambdaCache(funcDecl, irExpr);
				VLOG(2) << "interceptorplugin" << irExpr;
				return true;
			}
		}
    	return false;
	}
    
    core::TypePtr InterceptorPlugin::Visit(const clang::Type* type, insieme::frontend::conversion::Converter& convFact) {
		if(convFact.getInterceptor().isIntercepted(type)) {
			VLOG(2) << "interceptorplugin\n";
			VLOG(2) << type << " isIntercepted";
			auto res = convFact.getInterceptor().intercept(type, convFact);
			//convFact.addToTypeCache(type, res);
			return res;
		}
		return nullptr;
	}
	
	void InterceptorPlugin::PostVisit(const clang::Decl* decl, insieme::frontend::conversion::Converter& convFact) {
		if(const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl) ) {
			
			//for Converter::lookUpVariable
			if( varDecl->hasGlobalStorage()
				&& convFact.getInterceptor().isIntercepted(varDecl->getQualifiedNameAsString())) {
				
				//we expect globals to be literals -- get the "standard IR"which we need to change
				core::LiteralPtr globalLit = convFact.lookUpVariable(varDecl).as<core::LiteralPtr>();
				assert(globalLit);
				VLOG(2) << globalLit;
					
				auto globals = convFact.getIRTranslationUnit().getGlobals();

				//varDecl in the cache has "name" we need "qualifiedName" 
				auto name = varDecl->getQualifiedNameAsString();
				auto replacement = convFact.getIRBuilder().literal(name,globalLit->getType());
				
				//migrate possible annotations
				core::transform::utils::migrateAnnotations(globalLit, replacement);

				//standard way only add nonstaticlocal and nonexternal to the globals
				if( !varDecl->isStaticLocal() && !varDecl->hasExternalStorage() ) {
					auto git = std::find_if(globals.begin(), globals.end(), 
							[&](const insieme::frontend::tu::IRTranslationUnit::Global& cur)->bool { 
								return *globalLit == *cur.first; 
							});
					assert(git != globals.end() && "only remove the intercepted globals which were added in the standard way");
					if (varDecl->isStaticDataMember()) {
						//remove varDecl from TU -- as they are declared by the intercepted party
						if( git != globals.end() ) { 
							globals.erase(git); 
							VLOG(2) << "removed from TU.globals";
						} 
					} else {
						// replace in TU the "wrong" literal with the "simple" name with the qualified name
						if( git != globals.end() ) { 
							git->first = replacement; 
							VLOG(2) << "replaced in TU.globals";
						} 
					}
				}

				//replace the current var with the changed one
				convFact.addToVarDeclMap(varDecl,replacement);
				VLOG(2) << "changed from " << globalLit << " to " << replacement;
				VLOG(2) << convFact.lookUpVariable(varDecl);
			}
		}
	}
}
}
}
