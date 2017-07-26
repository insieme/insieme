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

#include "insieme/frontend/converter.h"
#include "insieme/frontend/clang.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace analysis {
	struct MemberProperties;
}
}

namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//							DECL CONVERTER -- converts declarations from clang to the IR translation unit
	//
	// Unlike other converters, this one modifies state:
	// it generates entries in the Variable manager and ir translation unit
	//---------------------------------------------------------------------------------------------------------------------
	class DeclConverter : public clang::DeclVisitor<DeclConverter> {
	private:
		Converter& converter;
		const core::IRBuilder& builder;

		/// Whether we are currently in an 'extern "C"' block
		///
		bool inExternC = false;

	public:
		DeclConverter(Converter& converter);

		/// Type expressing the conversion result of a clang Variable decl
		///
		using ConvertedVarDecl = std::pair<core::VariablePtr, core::ExpressionPtr>;

		// Converters -----------------------------------------------------------------------------------------------------

		/// Converts a variable declaration into an IR variable.
		/// @param varDecl is a clang VarDecl which represent a definition for the variable
		/// @return Converted variable
		ConvertedVarDecl convertVarDecl(const clang::VarDecl* varDecl) const;

		/// Converts a function declaration into IR.
		/// @param funcDecl is a clang FunctionDecl which represent a definition for the function
		/// @param name the name to use for the lambda. If empty, one will be generated
		/// @param genLiteral whether to generate a full lambda expr or just a literal
		/// @return Converted lambda
		core::ExpressionPtr convertFunctionDecl(const clang::FunctionDecl* funcDecl, string name = "", bool genLiteral = false) const;


		/// Converts a method declaration into an IR MemberFunction.
		/// @param methDecl is a clang CXXMethodDecl which represent a definition for the method
		/// @param parents parents of the class this is a method of
		/// @param fields fields of the class this is a method of
		/// @param declOnly only add method to function table, don't convert body
		/// @return Converted member function
		core::analysis::MemberProperties convertMethodDecl(const clang::CXXMethodDecl* methDecl, const core::ParentsPtr& parents, const core::FieldsPtr& fields,
		                                                   bool declOnly = false, bool addDefaultedAndDeletedAnnotations = true) const;

		// Visitors -------------------------------------------------------------------------------------------------------

		void VisitDeclContext(const clang::DeclContext* context);

		void VisitRecordDecl(const clang::RecordDecl* typeDecl);
		void VisitTypedefNameDecl(const clang::TypedefNameDecl* typedefDecl);
		void VisitVarDecl(const clang::VarDecl* var);
		void VisitLinkageSpec(const clang::LinkageSpecDecl* link);
		void VisitFunctionDecl(const clang::FunctionDecl* funcDecl);
		void VisitFunctionTemplateDecl(const clang::FunctionTemplateDecl* funcTempDecl);
		void VisitNamespaceDecl(const clang::NamespaceDecl* namespaceDecl);

	};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
