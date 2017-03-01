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
 *
 */
#pragma once

#include "insieme/frontend/clang.h"

namespace insieme {
namespace frontend {

// forward decl
namespace conversion {
	class Converter;
}

namespace utils {

	using namespace llvm;

	/**
	 * Remove all symbols which are not allowed in a C identifier from the given string
	 */
	std::string removeSymbols(std::string str);

	/**
	* Create a string representation of a clang SourceLocation
	*/
	std::string getLocationAsString(const clang::SourceLocation sl, const clang::SourceManager& sm, bool mangled = true);

	/**
	 * Create a name for an anonymous object (encodes location)
	 */
	std::string createNameForAnon(const std::string& prefix, const clang::Decl* decl, const clang::SourceManager& sm);

	/**
	 * we build a complete name for the class,
	 * qualified name does not have the specific types of the specialization
	 * the record provides the qualified name, the type the specialization for the type
	 * we merge both strings in a safe string for the output
	 */
	std::string getNameForRecord(const clang::NamedDecl* decl, const clang::Type* type, const clang::SourceManager& sm);

	/**
	 * build a string suffix encoding the template parameters provided
	 * @param tempArgs template argument list
	 * @param astContext AstContext used for type name lookup
	 * @param cStyleName whether to build a name suitable for usage in a C program or for the IR
	 * @return encoded string value
	 */
	std::string buildNameSuffixForTemplate(const clang::TemplateArgumentList& tempArgs, clang::ASTContext& astContext, bool cStyleName = false);

	/**
	 * build a string to identify a function
	 * the produced string will be output-compatible, this means that we can use this name
	 * to name functions and they will not have qualification issues.
	 * @param funcDecl the function decl to name
	 * @param cStyleName whether to build a name suitable for usage in a C program or for the IR
	 * @return unique string value
	 */
	std::string buildNameForFunction(const clang::FunctionDecl* funcDecl, const conversion::Converter& converter, bool cStyleName = false);

	std::string getNameForGlobal(const clang::VarDecl* varDecl, const clang::SourceManager& sm);

	/**
	 * Get name for enumeration, either from typedef or generated for anonymous
	 * @param tagType clang TagType pointer
	 * @return name for enumeration
	 */
	std::string getNameForEnum(const clang::EnumDecl* enumDecl, const clang::SourceManager& sm);

	/**
	 * Get name for field (named or anonymous)
	 * @param fieldDecl the field declaration given by clang
	 * @return name for the field
	 */
	std::string getNameForField(const clang::FieldDecl* fieldDecl, const clang::SourceManager& sm);

	/**
	 * Get name for tag decl (named or anonymous)
	 * @param tagDecl the Tag declaration given by clang
	 * @param cStyleName whether to build a name suitable for usage in a C program or for the IR
	 * @return pair(name,bool) with bool indicating whether it is externally visible
	 */
	std::pair<std::string,bool> getNameForTagDecl(const conversion::Converter& converter, const clang::TagDecl* tagDecl, bool cStyleName = false);

	/**
	 * Remove leading :: qualifier from name
	 */
	std::string stripLeadingGlobalNamespace(const std::string& name);

	/**
	 * Get full name for DependentNameType
	 */
	std::string getNameForDependentNameType(const clang::DependentNameType* depName);

} // End utils namespace
} // End frontend namespace
} // End insieme namespace
