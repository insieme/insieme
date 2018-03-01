/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <functional>

#include <boost/format.hpp>

#include "insieme/frontend/converter.h"

#include "insieme/frontend/analysis/prunable_decl_visitor.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/type_converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/dump/text_dump.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/annotations/c/include.h"
#include "insieme/annotations/c/tag.h"
#include "insieme/common/env_vars.h"

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
	// ----------- conversion -----------
	core::tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup) {
		// just delegate operation to converter
		TranslationUnit tu(manager, unit, setup);
		conversion::Converter c(manager, tu, setup);
		// add them and fire the conversion
		return c.convert();
	}
}
}

namespace insieme {
namespace frontend {
namespace conversion {

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CONVERTER
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	Converter::Converter(core::NodeManager& mgr, const TranslationUnit& tu, const ConversionSetup& setup)
		: translationUnit(tu), convSetup(setup), pragmaMap(translationUnit.pragmas_begin(), translationUnit.pragmas_end()), mgr(mgr), builder(mgr),
		  feIR(mgr, getCompiler().isCXX()), irTranslationUnit(mgr) {

		if(getenv(INSIEME_DUMP_PRAGMALIST)) {
			for(auto p : translationUnit.getPragmaList()) {
				if(p->isStatement()) {
					std::cout << "Pragma on statement:\n";
					p->getStatement()->dump();
					p->dump(std::cout, getSourceManager());
					std::cout << "\n";
				}
				if(p->isDecl()) {
					std::cout << "Pragma on decl:\n";
					p->getDecl()->dump();
					p->dump(std::cout, getSourceManager());
					std::cout << "\n";
				}
			}
		}

		varManPtr = std::make_shared<state::VariableManager>(*this);
		funManPtr = std::make_shared<state::FunctionManager>();
		recordManPtr = std::make_shared<state::RecordManager>();
		headerTaggerPtr = std::make_shared<utils::HeaderTagger>(setup.getSystemHeadersDirectories(),
		                                                        setup.getInterceptedHeaderDirs(), setup.getInterceptionWhitelist(),
		                                                        setup.getIncludeDirectories(), getCompiler().getSourceManager());

		declConvPtr = std::make_shared<DeclConverter>(*this);
		if(translationUnit.isCxx()) {
			typeConvPtr = std::make_shared<CXXTypeConverter>(*this);
			exprConvPtr = std::make_shared<CXXExprConverter>(*this);
			stmtConvPtr = std::make_shared<CXXStmtConverter>(*this);
		} else {
			typeConvPtr = std::make_shared<CTypeConverter>(*this);
			exprConvPtr = std::make_shared<CExprConverter>(*this);
			stmtConvPtr = std::make_shared<CStmtConverter>(*this);
		}
		// tag the translation unit with as C++ if case
		irTranslationUnit.setCXX(translationUnit.isCxx());
		assert_true(irTranslationUnit.isEmpty()) << "the ir translation unit is not empty, should be before we start";
	}

	void Converter::printDiagnosis(const clang::SourceLocation& loc) {
		clang::Preprocessor& pp = getPreprocessor();
		// print warnings and errors:
		while(!warnings.empty()) {
			if(!getConversionSetup().hasOption(ConversionSetup::NoWarnings)) {
				if(getSourceManager().isLoadedSourceLocation(loc)) {
					std::cerr << "\n\nloaded location:\n";
					std::cerr << "\t" << *warnings.begin() << std::endl;
				} else {
					std::cerr << "\n\n";
					utils::clangPreprocessorDiag(pp, loc, clang::DiagnosticsEngine::Level::Warning, *warnings.begin());
				}
			}
			warnings.erase(warnings.begin());
		}
	}

	core::tu::IRTranslationUnit Converter::convert() {
		assert_true(getCompiler().getASTContext().getTranslationUnitDecl());

		insieme::utils::setAssertExtraInfoPrinter([&]() {
			std::cerr << " ==> last Trackable location: " << getLastTrackableLocation() << "\n";

			// print translationStack
			std::cerr << "\n\nTranslationStack:\n\n";
			bool printDumps = getenv(INSIEME_TRANSLATION_STACK_DUMP);
			int index = 0;
			while(!translationStack.empty()) {
				const auto& entry = translationStack.top();
				std::cerr << "  " << index++ << ": " << entry.getEntryTitle() << std::endl;
				if(printDumps) {
					std::cerr << std::endl;
					entry.dumpClangNode();
					std::cerr << "\n\n" << std::endl;
				}
				translationStack.pop();
			}

			// if the user didn't request to print entry dumps, tell him how he can do so
			if(!printDumps) {
				std::cerr << "\n\nTo print the TranslationStack with entry dumps, set the environment variable " << INSIEME_TRANSLATION_STACK_DUMP << std::endl;
			}
		});

		// collect all type definitions
		auto declContext = clang::TranslationUnitDecl::castToDeclContext(getCompiler().getASTContext().getTranslationUnitDecl());
		declConvPtr->VisitDeclContext(declContext);

		//std::cout << " ==================================== " << std::endl;
		//std::cout << getIRTranslationUnit() << std::endl;
		//std::cout << " ==================================== " << std::endl;

		insieme::utils::clearAssertExtraInfoPrinter();

		// that's all
		return irTranslationUnit;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						SOURCE LOCATIONS
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	void Converter::trackSourceLocation(const clang::Decl* decl) {
		if(const clang::ClassTemplateSpecializationDecl* spet = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(decl)) {
			lastTrackableLocation.push(spet->getPointOfInstantiation());
		} else {
			lastTrackableLocation.push(decl->getLocation());
		}
	}

	void Converter::trackSourceLocation(const clang::Stmt* stmt) {
		lastTrackableLocation.push(stmt->getLocStart());
	}

	void Converter::untrackSourceLocation() {
		assert_false(lastTrackableLocation.empty());
		lastTrackableLocation.pop();
	}

	std::string Converter::getLastTrackableLocation() const {
		if(!lastTrackableLocation.empty()) {
			return utils::location(lastTrackableLocation.top(), getSourceManager());
		} else {
			return "ERROR: unable to identify last input code location ";
		}
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ANNOTATIONS
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	core::ExpressionPtr Converter::attachFuncAnnotations(const core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl) {

		// ----------------------------------- Add annotations to this function -------------------------------------------
		pragma::handlePragmas(core::NodeList({node.as<core::NodePtr>()}), funcDecl, *this);

		// -------------------------------------------------- C NAME ------------------------------------------------------
		// check for overloaded operator "function" (normal function has kind OO_None)
		if(funcDecl->isOverloadedOperator()) {
			clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
			// in order to get a real name we have to use the getOperatorSpelling method
			// otherwise we get names like operator30 and this is for instance not
			// suitable when using STL containers (e.g., map needs < operator)...
			string operatorAsString = clang::getOperatorSpelling(operatorKind);
			core::annotations::attachName(node, ("operator" + operatorAsString));
		} else if(!funcDecl->getNameAsString().empty()) {
			// annotate with the C name of the function
			core::annotations::attachName(node, utils::buildNameForFunction(funcDecl, *this));
		}
		if(core::annotations::hasAttachedName(node)) { VLOG(2) << "attachedName: " << core::annotations::getAttachedName(node); }

		// ---------------------------------------- SourceLocation Annotation ---------------------------------------------
		// for each entry function being converted we register the location where it was originally defined in the C program
		std::pair<SourceLocation, SourceLocation> loc{funcDecl->getLocStart(), funcDecl->getLocEnd()};
		fe::pragma::PragmaStmtMap::DeclMap::const_iterator fit = pragmaMap.getDeclarationMap().find(funcDecl);

		if(fit != pragmaMap.getDeclarationMap().end()) {
			// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
			loc.first = fit->second->getStartLocation();
		}

		utils::attachLocationFromClang(node, getSourceManager(), loc.first, loc.second);

		return node;
	}

	void Converter::applyHeaderTagging(const core::NodePtr& node, const clang::Decl* decl) const {
		VLOG(2) << "Apply header tagging on " << dumpDetailColored(node) << " with decl\n" << dumpClang(decl);
		if(annotations::c::hasIncludeAttached(node)) {
			VLOG(2) << " -> Annotation already attached";
			return;
		}
		if(headerTaggerPtr->isIntercepted(decl)) {
			VLOG(2) << "-> Is intercepted!";
 			headerTaggerPtr->addHeaderForDecl(node, decl);
			if(annotations::c::hasIncludeAttached(node)) {
				// attach name for backend
				if(auto tagDecl = llvm::dyn_cast<clang::TagDecl>(decl)) {
					string name = insieme::utils::demangle(utils::getNameForTagDecl(*this, tagDecl, true).first);
					// we only attach the tag if we are in C or the type comes from an extern C context
					if(!getTranslationUnit().isCxx() || tagDecl->isExternCContext()) {
						if(tagDecl->isStruct()) insieme::annotations::c::attachCTag(node, "struct");
						else if(tagDecl->isUnion()) insieme::annotations::c::attachCTag(node, "union");
						else if(tagDecl->isEnum()) insieme::annotations::c::attachCTag(node, "enum");
					}
					core::annotations::attachName(node, name);
				}
				else if(auto funDecl = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
					string name = insieme::utils::demangle(utils::buildNameForFunction(funDecl, *this, true));
					core::annotations::attachName(node, name);
				}
				else if(auto varDecl = llvm::dyn_cast<clang::VarDecl>(decl)) {
					core::annotations::attachName(node, utils::stripLeadingGlobalNamespace(varDecl->getQualifiedNameAsString()));
				}
				else if(auto enumConstantDecl = llvm::dyn_cast<clang::EnumConstantDecl>(decl)) {
					core::annotations::attachName(node, utils::stripLeadingGlobalNamespace(enumConstantDecl->getQualifiedNameAsString()));
				}
				VLOG(2) << "-> annotated with: " << annotations::c::getAttachedInclude(node);
			} else {
				VLOG(2) << "-> NOT annotated!";
			}
		}
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						BASIC CONVERSIONS
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	core::ExpressionPtr Converter::convertExpr(const clang::Expr* expr) const {
		assert_true(expr) << "Calling convertExpr with a NULL pointer";
		return exprConvPtr->Visit(const_cast<Expr*>(expr));
	}

	core::ExpressionPtr Converter::convertInitExpr(const clang::Expr* expr) const {
		assert_true(expr) << "Calling convertInitExpr with a NULL pointer";
		return exprConvPtr->convertInitExpr(expr);
	}

	core::ExpressionPtr Converter::convertCxxArgExpr(const clang::Expr* expr, const core::TypePtr& targetType) const {
		assert_true(expr) << "Calling convertCxxArgExpr with a NULL pointer";
		return exprConvPtr->convertCxxArgExpr(expr, targetType);
	}

	core::StatementPtr Converter::convertStmt(const clang::Stmt* stmt) const {
		assert_true(stmt) << "Calling convertStmt with a NULL pointer";
		return stmtutils::aggregateStmts(builder, stmtConvPtr->Visit(const_cast<Stmt*>(stmt)));
	}

	stmtutils::StmtWrapper Converter::convertStmtToWrapper(const clang::Stmt* stmt) const {
		assert_true(stmt) << "Calling convertStmtToWrapper with a NULL pointer";
		return stmtConvPtr->Visit(const_cast<Stmt*>(stmt));
	}

	core::TypePtr Converter::convertType(const clang::QualType& type) const {
		return typeConvPtr->convert(type);
	}

	core::TypePtr Converter::convertVarType(const clang::QualType& type) const {
		return typeConvPtr->convertVarType(type);
	}

	core::TypePtr Converter::convertExprType(const clang::Expr* expr) const {
		return exprConvPtr->convertExprType(expr);
	}


	Converter::TranslationStackEntryInserter::TranslationStackEntryInserter(Converter& converter, const clang::Type* clangType) : converter(converter), entry(converter, clangType) {
		converter.translationStack.push(entry);
	}

	Converter::TranslationStackEntryInserter::TranslationStackEntryInserter(Converter& converter, const clang::Stmt* clangStmt) : converter(converter), entry(converter, clangStmt) {
		converter.translationStack.push(entry);
	}

	Converter::TranslationStackEntryInserter::~TranslationStackEntryInserter() {
		assert_true(converter.translationStack.top() == entry) << "Invalid translation stack state.\nexpected "
				<< converter.translationStack.top().getEntryTitle() << "\nactual " << entry.getEntryTitle() << "\n";
		converter.translationStack.pop();
	}


	Converter::TranslationStackEntry::TranslationStackEntry(Converter& converter, const clang::Type* clangType) : clangType(clangType) {
		auto name = ::format("%p", clangType);
		if(auto tt = llvm::dyn_cast<clang::TagType>(clangType)) {
			if(auto namedDecl = llvm::dyn_cast<clang::NamedDecl>(tt->getDecl())) {
				name = namedDecl->getNameAsString();
			}
		}
		entryTitle = "Converting Type " + name;
	}

	Converter::TranslationStackEntry::TranslationStackEntry(Converter& converter, const clang::Stmt* clangStmt) : clangStmt(clangStmt) {
		entryTitle = std::string("Converting ") + (llvm::dyn_cast<clang::Expr>(clangStmt) ? "Expr" : "Stmt") + " at "
				+ frontend::utils::getLocationAsString(clangStmt->getLocStart(), converter.getSourceManager());
	}

	bool Converter::TranslationStackEntry::operator==(const TranslationStackEntry& other) const {
		return entryTitle == other.entryTitle && clangType == other.clangType && clangStmt == other.clangStmt;
	}

	std::string Converter::TranslationStackEntry::getEntryTitle() const {
		return entryTitle;
	}

	void Converter::TranslationStackEntry::dumpClangNode() const {
		if(clangType) {
			clangType->dump();
		} else if(clangStmt) {
			clangStmt->dumpColor();
		}
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
