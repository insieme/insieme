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

#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/frontend/utils/name_manager.h"

#include <boost/program_options.hpp>

namespace insieme {
namespace frontend {
namespace extensions {

	boost::optional<std::string> InterceptorExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// interceptor needs to be the first extension in the extension list
		if(setup.getExtensions().begin()->get() != this) { return boost::optional<std::string>("InterceptorExtension should be the first Extension"); }

		// prerequisites are met - no prerequisite is missing
		return boost::optional<std::string>();
	}

	insieme::core::ExpressionPtr InterceptorExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		//if(const clang::DeclRefExpr* declRefExpr = llvm::dyn_cast<clang::DeclRefExpr>(expr)) {
		//	const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(declRefExpr->getDecl());
		//	std::string name;

		//	// if the decl is shadowed (brought into the current namespace with "using")
		//	// we need to (a) find the original decl and (b) get a canonical name
		//	if(const auto* shadowDecl = llvm::dyn_cast<clang::UsingShadowDecl>(declRefExpr->getFoundDecl())) {
		//		name = shadowDecl->getCanonicalDecl()->getQualifiedNameAsString();
		//		funcDecl = llvm::dyn_cast<clang::FunctionDecl>(shadowDecl->getTargetDecl());
		//	}

		//	if(funcDecl) {
		//		if(name.empty()) { name = funcDecl->getQualifiedNameAsString(); }
		//		if(getInterceptor().isIntercepted(name)) {
		//			VLOG(2) << "interceptorextension\n";
		//			// returns a callable expression
		//			return getInterceptor().intercept(funcDecl, converter, declRefExpr->hasExplicitTemplateArgs(), name);
		//		}
		//	}

		//	if(const clang::EnumConstantDecl* enumConstant = llvm::dyn_cast<clang::EnumConstantDecl>(declRefExpr->getDecl())) {
		//		const clang::EnumType* enumType =
		//		    llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(enumConstant->getDeclContext())->getTypeForDecl());
		//		if(getInterceptor().isIntercepted(enumType->getCanonicalTypeInternal())) { return getInterceptor().intercept(enumConstant, converter); }
		//	}
		//}
		return nullptr;
	}

	core::ExpressionPtr InterceptorExtension::FuncDeclVisit(const clang::FunctionDecl* funcDecl, insieme::frontend::conversion::Converter& converter,
	                                                        bool symbolic) {
		//// check whether function should be intercected
		//if(getInterceptor().isIntercepted(funcDecl)) {
		//	auto irExpr = getInterceptor().intercept(funcDecl, converter);
		//	VLOG(2) << "interceptorextension" << irExpr;
		//	return irExpr;
		//}
		return nullptr;
	}

	core::TypePtr InterceptorExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		//if(getInterceptor().isIntercepted(type)) {
		//	VLOG(2) << "interceptorextension\n";
		//	auto res = getInterceptor().intercept(type, converter);
		//	// converter.addToTypeCache(type, res);
		//	return res;
		//}
		return nullptr;
	}

	core::ExpressionPtr InterceptorExtension::ValueDeclPostVisit(const clang::ValueDecl* decl, core::ExpressionPtr expr,
	                                                             insieme::frontend::conversion::Converter& converter) {
		//if(const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl)) {
		//	if(getInterceptor().isIntercepted(varDecl->getQualifiedNameAsString())) {
		//		if(varDecl->hasGlobalStorage()) {
		//			// we expect globals to be literals -- get the "standard IR"which we need to change
		//			core::LiteralPtr globalLit = converter.lookUpVariable(varDecl).as<core::LiteralPtr>();
		//			assert_true(globalLit);
		//			VLOG(2) << globalLit;

		//			auto globals = converter.getIRTranslationUnit().getGlobals();

		//			// varDecl in the cache has "name" we need "qualifiedName"
		//			auto name = varDecl->getQualifiedNameAsString();
		//			auto replacement = converter.getIRBuilder().literal(name, globalLit->getType());

		//			// migrate possible annotations
		//			core::transform::utils::migrateAnnotations(globalLit, replacement);

		//			// standard way only add nonstaticlocal and nonexternal to the globals
		//			if(!varDecl->isStaticLocal() && !varDecl->hasExternalStorage()) {
		//				auto git = std::find_if(globals.begin(), globals.end(),
		//				                        [&](const insieme::frontend::tu::IRTranslationUnit::Global& cur) -> bool { return *globalLit == *cur.first; });
		//				assert(git != globals.end() && "only remove the intercepted globals which were added in the standard way");
		//				if(varDecl->isStaticDataMember()) {
		//					// remove varDecl from TU -- as they are declared by the intercepted party
		//					if(git != globals.end()) {
		//						globals.erase(git);
		//						VLOG(2) << "removed from TU.globals";
		//					}
		//				} else {
		//					// replace in TU the "wrong" literal with the "simple" name with the qualified name
		//					if(git != globals.end()) {
		//						git->first = replacement;
		//						VLOG(2) << "replaced in TU.globals";
		//					}
		//				}
		//			}

		//			// replace the current var with the changed one
		//			converter.addToVarDeclMap(varDecl, replacement);
		//			VLOG(2) << "changed from " << globalLit << " to " << replacement;
		//			VLOG(2) << converter.lookUpVariable(varDecl);
		//		}
		//	}
		//}
		return nullptr;
	}

	core::TypePtr InterceptorExtension::TypeDeclVisit(const clang::TypeDecl* decl, insieme::frontend::conversion::Converter& converter) {
		//if(llvm::isa<clang::TypedefDecl>(decl)) {
		//	if(getInterceptor().isIntercepted(decl->getQualifiedNameAsString())) {
		//		auto innerType = converter.convertType(decl->getTypeForDecl()->getCanonicalTypeInternal());

		//		core::IRBuilder builder(innerType->getNodeManager());

		//		if(!innerType.isa<core::GenericTypePtr>()) { return nullptr; }

		//		// if is a typedef which ends pointing to an annonymous struct, lets save the effort and
		//		// return a generic opaque type
		//		auto tmp = converter.getIRTranslationUnit()[innerType.as<core::GenericTypePtr>()];
		//		core::StructTypePtr structTy = tmp.isa<core::StructTypePtr>();
		//		// if (structTy && structTy->getName()->getValue().substr(0,5) == "_anon"){
		//		if(structTy && structTy->getName()->getValue() == "") {
		//			auto name = decl->getQualifiedNameAsString();
		//			core::GenericTypePtr gen = builder.genericType(name);
		//			converter.getHeaderTagger().addHeaderForDecl(gen, decl);
		//			return structTy;
		//		}
		//		return nullptr;
		//	}
		//}
		return nullptr;
	}

	/**
	 * This post visitor is needed to check if we have a ctor that contains a default argument
	 * that accesses or uses private structs or elements. We have to remove this default arguments
	 * otherwise our backend code will contain a call to a private element -> compiler error
	 * Example:
	 *  class A { private: struct X{}; public: A(X x=X()) {} };
	 *  int main() { A a; }
	 */
	core::ExpressionPtr InterceptorExtension::PostVisit(const clang::Expr* expr, const core::ExpressionPtr& irExpr, conversion::Converter& converter) {
		//if(const clang::CXXConstructExpr* call = llvm::dyn_cast<clang::CXXConstructExpr>(expr)) {
		//	// only do this for intercepted types and only if we have an IR ctor call
		//	if(!interceptor.isIntercepted(call->getConstructor())) { return irExpr; }
		//	if(!core::analysis::isConstructorCall(irExpr)) { return irExpr; }
		//	// check for default arguments
		//	unsigned defaultArgs = 0;
		//	for(unsigned i = 0; i < call->getNumArgs(); ++i) {
		//		// count the number of default args. they have to be at the end...
		//		if(llvm::dyn_cast<clang::CXXDefaultArgExpr>(call->getArg(i)->IgnoreImplicit())) { defaultArgs++; }
		//	}
		//	// if no default args -> early exit
		//	if(!defaultArgs) { return irExpr; }
		//	// else create a new call
		//	// first check if the ir call is a call expression
		//	assert(irExpr.isa<core::CallExprPtr>() && "the constructor call has to be a call expression.");
		//	core::CallExprPtr callExpr = irExpr.as<core::CallExprPtr>();
		//	// no literal means not intercepted. return.
		//	if(!callExpr->getFunctionExpr().isa<core::LiteralPtr>()) { return irExpr; }
		//	// create new argument list
		//	core::ExpressionList newArgs;
		//	core::TypeList argTypes;
		//	for(unsigned i = 0; i < callExpr->getArguments().size() - defaultArgs; ++i) {
		//		// migrate the annotations and store the node in the list
		//		core::ExpressionPtr newA = callExpr->getArgument(i);
		//		core::TypePtr newT = callExpr->getArgument(i)->getType();
		//		core::transform::utils::migrateAnnotations(callExpr->getArgument(i), newA);
		//		core::transform::utils::migrateAnnotations(callExpr->getArgument(i)->getType(), newT);
		//		newArgs.push_back(newA);
		//		argTypes.push_back(newT);
		//	}
		//	// extract old function type and return type
		//	core::FunctionTypePtr funType = callExpr->getFunctionExpr()->getType().as<core::FunctionTypePtr>();
		//	core::transform::utils::migrateAnnotations(callExpr->getFunctionExpr()->getType(), funType);
		//	core::TypePtr retType = funType->getReturnType();
		//	core::transform::utils::migrateAnnotations(funType->getReturnType(), retType);
		//	// create new function expression
		//	core::LiteralPtr literal = callExpr->getFunctionExpr().as<core::LiteralPtr>();
		//	core::transform::utils::migrateAnnotations(callExpr->getFunctionExpr(), literal);
		//	core::ExpressionPtr newFunExpr =
		//	    converter.getIRBuilder().literal(literal->getStringValue(), converter.getIRBuilder().functionType(argTypes, retType, core::FK_CONSTRUCTOR));
		//	core::transform::utils::migrateAnnotations(callExpr->getFunctionExpr(), newFunExpr);

		//	auto ir = converter.getIRBuilder().callExpr(retType, newFunExpr, newArgs);
		//	core::transform::utils::migrateAnnotations(irExpr, ir);
		//	return ir;
		//}
		return irExpr;
	}

	FrontendExtension::flagHandler InterceptorExtension::registerFlag(boost::program_options::options_description& options) {
		// create lambda
		auto lambda = [&](const ConversionJob& job) {
			// check if the default activated plugins have been deactivated manually
			if(job.hasOption(frontend::ConversionJob::NoDefaultExtensions)) { return false; }
			this->setInterceptor(job.getInterceptedNameSpacePatterns());
			return true;
		};
		return lambda;
	}
} // extensions
} // frontend
} // insieme
