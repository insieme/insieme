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

#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/decl_converter.h"

#include <boost/program_options.hpp>

namespace insieme {
namespace frontend {
namespace extensions {

	InterceptorExtension::InterceptorExtension() {
	}

	boost::optional<std::string> InterceptorExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// interceptor needs to be the first extension in the extension list
		if(setup.getExtensions().begin()->get() != this) {
			return boost::optional<std::string>("InterceptorExtension should be the first Extension");
		}

		// prerequisites are met - no prerequisite is missing
		return boost::optional<std::string>();
	}

	core::ExpressionPtr InterceptorExtension::FuncDeclVisit(const clang::FunctionDecl* funcDecl, insieme::frontend::conversion::Converter& converter,
	                                                        bool symbolic) {
		std::cout << "INTERCEPTOR F: " << funcDecl->getNameAsString() << " : " << converter.getHeaderTagger()->isIntercepted(funcDecl) << "\n";
		return nullptr;
	}

	core::TypePtr InterceptorExtension::TypeDeclVisit(const clang::TypeDecl* decl, insieme::frontend::conversion::Converter& converter) {
		std::cout << "INTERCEPTOR T: " << decl->getNameAsString() << " : " << converter.getHeaderTagger()->isIntercepted(decl) << "\n";
		return nullptr;
	}

	insieme::core::ExpressionPtr InterceptorExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		std::cout << "INTERCEPTOR E\n";
		auto dr = llvm::dyn_cast<clang::DeclRefExpr>(expr);
		if(dr) {
			auto decl = dr->getDecl();
			std::cout << " -> dr\n";
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				std::cout << " -> inter\n";
				auto funDecl = llvm::dyn_cast<clang::FunctionDecl>(decl);
				if(funDecl) {
					std::cout << " -> fun\n";
					auto lit = converter.getIRBuilder().literal(utils::buildNameForFunction(funDecl), converter.convertType(expr->getType()));
					converter.applyHeaderTagging(lit, decl);
					VLOG(2) << "Interceptor: intercepted clang fun\n" << dumpClang(decl) << " -> converted to literal: " << *lit << "\n";
					return lit;
				}
			}
		}
		auto construct = llvm::dyn_cast<clang::CXXConstructExpr>(expr);
		if(construct) {
			auto decl = construct->getConstructor();
			std::cout << " -> construct\n";
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				std::cout << " -> inter\n";
				auto funDecl = llvm::dyn_cast<clang::FunctionDecl>(decl);
				if(funDecl) {
					std::cout << " -> fun\n";
					return converter.getDeclConverter()->convertFunctionDecl(decl, utils::buildNameForFunction(decl), true);
				}
			}
		}
		auto memberCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(expr);
		if(memberCall) {
			auto decl = memberCall->getCalleeDecl();
			std::cout << " -> memberCall\n";
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				std::cout << " -> inter\n";
				auto funDecl = llvm::dyn_cast<clang::FunctionDecl>(decl);
				if(funDecl) {
					std::cout << " -> fun\n";
					return converter.getDeclConverter()->convertFunctionDecl(funDecl, utils::buildNameForFunction(funDecl), true);
				}
			}
		}

		return nullptr;
	}


	core::TypePtr InterceptorExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		std::cout << "INTERCEPTOR QT\n";
		auto tt = llvm::dyn_cast<clang::TagType>(type->getCanonicalTypeUnqualified());
		if(tt) {
			auto decl = tt->getDecl();
			std::cout << " -> " << decl->getNameAsString() << " : " << converter.getHeaderTagger()->isIntercepted(decl) << "\n";
			if(converter.getHeaderTagger()->isIntercepted(decl)) {
				auto genType = converter.getIRBuilder().genericType(utils::getNameForTagDecl(converter, decl).first);
				converter.applyHeaderTagging(genType, decl);
				VLOG(2) << "Interceptor: intercepted clang type\n" << dumpClang(decl) << " -> converted to generic type: " << *genType << "\n";
				return genType;
			}
		}
		return nullptr;
	}

	FrontendExtension::flagHandler InterceptorExtension::registerFlag(boost::program_options::options_description& options) {
		// create lambda
		auto lambda = [&](const ConversionJob& job) {
			// check if the default activated plugins have been deactivated manually
			if(job.hasOption(frontend::ConversionJob::NoDefaultExtensions)) { return false; }
			return true;
		};
		return lambda;
	}

} // extensions
} // frontend
} // insieme
