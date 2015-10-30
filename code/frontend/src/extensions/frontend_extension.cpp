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

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/pragma/handler.h"

namespace insieme {
namespace frontend {
namespace extensions {

	// ############ PRAGMA HANDLING ############ //
	PragmaHandler::PragmaHandler(const std::string& pragmaNamespace, const std::string& keyword, insieme::frontend::pragma::node const& re,
	                             pragmaHandlerFunction lambda)
	    : f(lambda), name(pragmaNamespace), keyw(keyword), tok(re.copy()){};

	PragmaHandler::PragmaHandler(PragmaHandler& pragma) : f(pragma.f), name(pragma.name), keyw(pragma.keyw), tok(pragma.tok->copy()) {}

	PragmaHandler::PragmaHandler(const PragmaHandler& pragma) : f(pragma.f), name(pragma.name), keyw(pragma.keyw), tok(pragma.tok->copy()) {}

	PragmaHandler::~PragmaHandler() {
		delete tok;
	}

	insieme::frontend::pragma::node* PragmaHandler::getToken() {
		return tok;
	};

	// ############ DRIVER STAGE ############ //
	FrontendExtension::flagHandler FrontendExtension::registerFlag(boost::program_options::options_description& options) {
		return [&](const ConversionJob& job) {
			// check if the default activated plugins have been deactivated manually
			if(job.hasOption(frontend::ConversionJob::NoDefaultExtensions)) { return false; }
			return true;
		};
	}

	boost::optional<std::string> FrontendExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
		// prerequisites are met - no prerequisite is missing
		return boost::optional<std::string>();
	}

	// ############ PRE CLANG STAGE ############ //
	const FrontendExtension::macroMap& FrontendExtension::getMacroList() const {
		return macros;
	}

	const FrontendExtension::headerVec& FrontendExtension::getInjectedHeaderList() const {
		return injectedHeaders;
	}

	const FrontendExtension::includeDirVec& FrontendExtension::getKidnappedHeaderList() const {
		return kidnappedHeaders;
	}

	const FrontendExtension::includeDirVec& FrontendExtension::getIncludeDirList() const {
		return includeDirs;
	}

	// ############ CLANG STAGE ############ //
	insieme::core::ExpressionPtr FrontendExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

	insieme::core::TypePtr FrontendExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

	stmtutils::StmtWrapper FrontendExtension::Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter) {
		return stmtutils::StmtWrapper();
	}

	insieme::core::NodePtr FrontendExtension::Visit(const clang::Decl* decl, insieme::frontend::conversion::Converter& converter, bool symbolic) {
		if(llvm::isa<clang::FunctionDecl>(decl)) { return this->FuncDeclVisit(llvm::cast<clang::FunctionDecl>(decl), converter, symbolic); }
		if(llvm::isa<clang::ValueDecl>(decl)) { return this->ValueDeclVisit(llvm::cast<clang::ValueDecl>(decl), converter); }
		if(llvm::isa<clang::TypeDecl>(decl)) { return this->TypeDeclVisit(llvm::cast<clang::TypeDecl>(decl), converter); }
		return nullptr;
	}

	insieme::core::TypePtr FrontendExtension::TypeDeclVisit(const clang::TypeDecl* decl, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

	insieme::core::ExpressionPtr FrontendExtension::FuncDeclVisit(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& converter,
	                                                              bool symbolic) {
		return nullptr;
	}

	insieme::core::ExpressionPtr FrontendExtension::ValueDeclVisit(const clang::ValueDecl* decl, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}


	insieme::core::ExpressionPtr FrontendExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
	                                                          insieme::frontend::conversion::Converter& converter) {
		return irExpr;
	}

	insieme::core::TypePtr FrontendExtension::PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
	                                                    insieme::frontend::conversion::Converter& converter) {
		return irType;
	}

	stmtutils::StmtWrapper FrontendExtension::PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
	                                                    insieme::frontend::conversion::Converter& converter) {
		return irStmt;
	}

	insieme::core::TypePtr FrontendExtension::TypeDeclPostVisit(const clang::TypeDecl* decl, core::TypePtr type,
	                                                            insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

	insieme::core::ExpressionPtr FrontendExtension::FuncDeclPostVisit(const clang::FunctionDecl* decl, core::ExpressionPtr expr,
	                                                                  insieme::frontend::conversion::Converter& converter, bool symbolic) {
		return nullptr;
	}

	insieme::core::ExpressionPtr FrontendExtension::ValueDeclPostVisit(const clang::ValueDecl* decl, core::ExpressionPtr expr,
	                                                                   insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

	insieme::core::NodePtr FrontendExtension::PostVisit(const clang::Decl* decl, core::NodePtr ir, insieme::frontend::conversion::Converter& converter,
	                                                    bool symbolic) {
		if(llvm::isa<clang::FunctionDecl>(decl) && ir.isa<core::ExpressionPtr>()) {
			return this->FuncDeclPostVisit(llvm::cast<clang::FunctionDecl>(decl), ir.as<core::ExpressionPtr>(), converter, symbolic);
		}
		if(llvm::isa<clang::ValueDecl>(decl) && ir.isa<core::ExpressionPtr>()) {
			return this->ValueDeclPostVisit(llvm::cast<clang::ValueDecl>(decl), ir.as<core::ExpressionPtr>(), converter);
		}
		if(llvm::isa<clang::TypeDecl>(decl) && ir.isa<core::TypePtr>()) {
			return this->TypeDeclPostVisit(llvm::cast<clang::TypeDecl>(decl), ir.as<core::TypePtr>(), converter);
		}
		return nullptr;
	}

	// ############ POST CLANG STAGE ############ //
	insieme::core::ProgramPtr FrontendExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		return prog;
	}

	core::tu::IRTranslationUnit FrontendExtension::IRVisit(core::tu::IRTranslationUnit& tu) {
		return tu;
	}

	// ############ PRAGMA HANDLING ############ //
	const FrontendExtension::pragmaHandlerVec& FrontendExtension::getPragmaHandlers() const {
		return pragmaHandlers;
	}


} // extensions
} // frontend
} // insieme
