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

#pragma once

#include <set>
#include <memory>
#include <functional>

#include "insieme/frontend/clang_forward.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/translation_unit.h"
#include "insieme/frontend/utils/frontend_ir.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/core/ir_program.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace frontend {
	/**
	* This function converts a clang translation unit into an IR translation unit.
	*
	* @param manager the manager to be used for managing the resulting IR nodes
	* @param unit the translation unit to be processed
	* @param setup the setup for the conversion process to be respected
	* @return the resulting translation unit
	*/
	core::tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup = ConversionSetup());

	// Forward Declarations
	namespace state {
		class VariableManager;
		class FunctionManager;
		class RecordManager;
	}
	namespace utils {
		class HeaderTagger;
	}
} // end namespace frontend
} // end namespace insieme

namespace insieme {
namespace frontend {
namespace conversion {

	namespace {
		static inline bool genericEmpty(const insieme::core::NodePtr& t) {
			return !t;
		}
		static inline bool genericEmpty(const insieme::frontend::stmtutils::StmtWrapper& w) {
			return w.size() == 0;
		}
	} // end anonymous namespace

	// Forward Declaration
	class DeclConverter;

	namespace state = insieme::frontend::state;

	// ------------------------------------ Converter ---------------------------
	/**
	 * The main unit orchestrating the clang AST => IR Translation Unit conversion processes.
	 */
	class Converter : boost::noncopyable {
		/// List of warnings up to this point
		///
		std::set<std::string> warnings;

		/// Converts Clang declarations to corresponding IR, also storing the necessary state
		///
		std::shared_ptr<DeclConverter> declConvPtr;

		/// Converts a Clang statement into an IR statement
		///
		class StmtConverter;
		class CStmtConverter;
		class CXXStmtConverter;
		std::shared_ptr<StmtConverter> stmtConvPtr;

		///  Converts a Clang types into an IR types
		///
		class TypeConverter;
		class CTypeConverter;
		class CXXTypeConverter;
		std::shared_ptr<TypeConverter> typeConvPtr;

		/// Converts a Clang expression into an IR expression
		///
		class ExprConverter;
		class CExprConverter;
		class CXXExprConverter;
		std::shared_ptr<ExprConverter> exprConvPtr;

		const TranslationUnit& translationUnit;
		const ConversionSetup& convSetup;

		/// A map which associates a statement of the clang AST to a pragma (if any)
		///
		pragma::PragmaStmtMap pragmaMap;

		/// A state object which manages mappings from clang variables to IR variables
		///
		std::shared_ptr<state::VariableManager> varManPtr;

		/// A state object which manages mappings from clang functions to IR LiteralPtrs
		///
		std::shared_ptr<state::FunctionManager> funManPtr;

		/// A state object which manages mappings from clang record types to IR GenericTypes
		///
		std::shared_ptr<state::RecordManager> recordManPtr;

		/// An object which handles attaching tags to literals generated from symbols defined in the standard library
		///
		std::shared_ptr<utils::HeaderTagger> headerTaggerPtr;

		/**
		 * IR building and managing tools
		 */
		core::NodeManager& mgr;
		const core::IRBuilder builder;
		const frontend::ir::FrontendIr feIR;
		core::tu::IRTranslationUnit irTranslationUnit;

		/// A set holding all the ClassTemplateSpecializationDecls we already asked clang to instantiate
		///
		std::set<clang::ClassTemplateSpecializationDecl*> instantiatedDecls;

		/**
		 * Attach annotations to a C function of the input translation unit.
		 *
		 * returns the a MarkerExprPtr if a marker node has to be added and the passed node
		 */
		core::ExpressionPtr attachFuncAnnotations(const core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl);

		/**
		 *  keeps track of the last point a source location to the input code could be found
		 */
		std::stack<clang::SourceLocation> lastTrackableLocation;

	  public:
		Converter(core::NodeManager& mgr, const TranslationUnit& translationUnit, const ConversionSetup& setup = ConversionSetup());

		// should only be run once
		core::tu::IRTranslationUnit convert();

		// Getters & Setters
		const core::IRBuilder& getIRBuilder() const { return builder; }
		core::NodeManager& getNodeManager() const { return mgr; }

		const TranslationUnit& getTranslationUnit() const { return translationUnit; }
		const ConversionSetup& getConversionSetup() const { return convSetup; }
		const frontend::ir::FrontendIr& getFrontendIR() const { return feIR; }

		const ClangCompiler& getCompiler() const { return translationUnit.getCompiler(); }
		clang::Preprocessor& getPreprocessor() const { return getCompiler().getPreprocessor(); }
		clang::SourceManager& getSourceManager() const { return getCompiler().getSourceManager(); }

		core::tu::IRTranslationUnit& getIRTranslationUnit() { return irTranslationUnit; }
		const core::tu::IRTranslationUnit& getIRTranslationUnit() const { return irTranslationUnit; }

		std::set<clang::ClassTemplateSpecializationDecl*>& getInstantiatedDecls() { return instantiatedDecls; }

		std::shared_ptr<DeclConverter> getDeclConverter() const { return declConvPtr; }
		std::shared_ptr<ExprConverter> getExprConverter() const { return exprConvPtr; }
		std::shared_ptr<StmtConverter> getStmtConverter() const { return stmtConvPtr; }
		std::shared_ptr<TypeConverter> getTypeConverter() const { return typeConvPtr; }

		std::shared_ptr<state::VariableManager> getVarMan() const { return varManPtr; }
		std::shared_ptr<state::FunctionManager> getFunMan() const { return funManPtr; }
		std::shared_ptr<state::RecordManager> getRecordMan() const { return recordManPtr; }
		std::shared_ptr<utils::HeaderTagger> getHeaderTagger() const { return headerTaggerPtr; }

		const pragma::PragmaStmtMap& getPragmaMap() const {	return pragmaMap; }

		/**
		 * Entry point for converting clang statements into IR statements
		 * @param stmt is a clang statement of the AST
		 * @return the corresponding IR statement
		 */
		core::StatementPtr convertStmt(const clang::Stmt* stmt) const;

		/**
		 * Entry point for converting clang statements into an IR statement wrapper
		 * @param stmt is a clang statement of the AST
		 * @return the corresponding IR statement(s) in a wrapper
		 */
		stmtutils::StmtWrapper convertStmtToWrapper(const clang::Stmt* stmt) const;

		/**
		 * Entry point for converting clang expressions to IR expressions
		 * @param expr is a clang expression of the AST
		 * @return the corresponding IR expression
		 */
		core::ExpressionPtr convertExpr(const clang::Expr* expr) const;

		/**
		 * Entry point for converting clang initialization expressions to IR expressions
		 * - the difference is relevant in the case of string literals
		 * @param expr is a clang expression of the AST
		 * @return the corresponding IR initialization expression
		 */
		core::ExpressionPtr convertInitExpr(const clang::Expr* expr) const;

		/**
		 * Entry point for converting clang expressions used in C++ arguments or return values to IR expressions
		 * - in these cases, we need to skip implicitly generated move and copy constructor calls
		 * - we also need to cast reference types to the correct reference kind if required
		 * @param expr is a clang expression of the AST
		 * @param targetType is the IR type we require (optional, no casting if not provided)
		 * @return the corresponding IR expression
		 */
		core::ExpressionPtr convertCxxArgExpr(const clang::Expr* expr, const core::TypePtr& targetType = nullptr) const;

		/**
		 * Entry point for converting clang types into an IR types
		 * @param type is a clang type
		 * @return the corresponding IR type
		 */
		core::TypePtr convertType(const clang::QualType& type) const;

		/**
		 * Entry point for converting clang types into IR types with memory locations for variables
		 * @param type is a clang type
		 * @return the corresponding IR type
		 */
		core::TypePtr convertVarType(const clang::QualType& type) const;

		/**
		 * Entry point for converting clang expression types into IR types (with correct lvalue/rvalue handling)
		 * @param expr is a clang expression
		 * @return the corresponding IR type
		 */
		core::TypePtr convertExprType(const clang::Expr* expr) const;


		/**
		 * Tag symbols from std libs with the appropriate header information
		 * @param node the node to (potentially) tag
		 * @param decl the clang decl this node is derived from
		 */
		void applyHeaderTagging(const core::NodePtr& node, const clang::Decl* decl) const;

		/**
		 * Print diagnosis errors, warnings stored during translation
		 * sometimes we can not retrieve a location to attach the error to, we'll store it
		 * and it will be printed as soon as a location can be used
		 * @param loc: the location this warning will be attached to
		 */
		void printDiagnosis(const clang::SourceLocation& loc);

		/**
		 *  Keeps track of the last point a source location to the Declaration
		 *  this might be different depending of what we are dealing with.
		 *  Template spetialization might have 2 locations, template and instantiation location, both of those
		 *  are not the location retrieved by the getLocation method in Decl
		 */
		void trackSourceLocation(const clang::Decl* decl);

		/**
		 *  Keeps track of the last point a source location to the Statement
		 *  this might be different depending of what we are dealing with
		 */
		void trackSourceLocation(const clang::Stmt* stmt);

		/**
		 * Unstacks last tracked location
		 */
		void untrackSourceLocation();

		/**
		 *  Returns readable location of the last registered source location
		 */
		std::string getLastTrackableLocation() const;

		/**
		 *  Returns the filename and path of the translation unit
		 */
		const boost::filesystem::path& getTUFileName() const {
			return getTranslationUnit().getFileName();
		}

	  public:
		/**
		 *  Apply extension pre- and postprocessing steps to translation
		 */
		template<typename IRType, typename ClangType, typename AppliedFunction>
		IRType applyExtensions(ClangType input, const AppliedFunction& fun) {
			IRType ret;

			// call frontend extension visitors
			for(auto extension : getConversionSetup().getExtensions()) {
				ret = extension->Visit(input, *this);
				if(!genericEmpty(ret)) { break; }
			}

			if(genericEmpty(ret)) {
				ret = fun(input);
			}

			// call frontend extension post visitors
			for(auto extension : getConversionSetup().getExtensions()) {
				ret = extension->PostVisit(input, ret, *this);
			}
			return ret;
		}

	};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
