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

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"

namespace insieme {
namespace frontend {
namespace extensions {

	class InterceptorExtension : public insieme::frontend::extensions::FrontendExtension {
	  public:
		using SpecializationMap = std::map<const clang::Type*, core::TypePtr>;

	  private:
		SpecializationMap templateSpecializationMapping;

	  public:
		InterceptorExtension();

		virtual FrontendExtension::flagHandler registerFlag(boost::program_options::options_description& options) override;
		virtual boost::optional<std::string> isPrerequisiteMissing(ConversionSetup& setup) const override;

		// Extension Hooks

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;
		virtual core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

		virtual bool FuncDeclVisit(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& converter) override;
		virtual bool VarDeclVisit(const clang::VarDecl* decl, insieme::frontend::conversion::Converter& converter) override;

	};

} // extensions
} // frontend
} // insieme
