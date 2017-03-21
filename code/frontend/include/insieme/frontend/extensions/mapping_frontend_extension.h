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

#include <string>
#include <map>

#include "insieme/frontend/clang.h"
#include "insieme/core/ir_node.h"


namespace insieme {
namespace frontend {
namespace extensions {

	/**
	 * An Extension which allows to more easily build custom extensions which perform simple type and expression mappings based on regex patterns.
	 */
	class MappingFrontendExtension : public FrontendExtension {
	  public:
		/**
		 * This extension will perform a pre-visit on expressions.
		 * If a subclass has to do the same, it has to call this method here too, in order for the mapping to still work.
		 */
		virtual core::ExpressionPtr Visit(const clang::Expr* expr, conversion::Converter& converter) override;

		/**
		 * This Extension will perform a pre-visit on types.
		 * If a subclass has to do the same, it has to call this method here too, in order for the mapping to still work.
		 */
		virtual core::TypePtr Visit(const clang::QualType& typeIn, conversion::Converter& converter) override;


	  protected:
		/**
		 * This is the actual interface for type mappings.
		 * A subclass can return a map containing the rules on how to map types. the keys in the map are interpreted as regex strings and matched
		 * against the fully qualified name of the declaration of the corresponding type. If the regex matches, then the corresponding value is used
		 * to build a new type. This type construction supports certain special syntax patterns to extract different elements of the translated type.
		 *
		 * For concrete examples on the supported patterns, see the implementation of MappingFrontendExtension::mapType or more concrete, have
		 * a look at the unit test which tests all this functionality at:
		 * @see insieme/code/frontend/test/extensions/mapping_extension_test.cc
		 * @see insieme/code/frontend/test/inputs/conversion/mapping_types.cpp
		 */
		virtual std::map<std::string, std::string> getTypeMappings();

		/**
		 * This is a callback hook which can be used to customize the actual type parsing for the type matching mechanism in this extension.
		 * The default implementation will simply return <tt>builder.parseType(code)</tt>. This should be sufficient for most cases. However,
		 * if a subclass has to customize the type parsing (e.g. by adding additional symbols to the parser loaded from a core extension), this
		 * callback can be used.
		 */
		virtual core::TypePtr parseTypeForTypeMapping(const core::IRBuilder& builder, const std::string& code);


	  private:
		/// Members used for type mapping
		std::map<std::string, insieme::core::TypePtr> typeIrMap;

		using CodeExtractor = std::function<insieme::core::TypePtr(const clang::RecordDecl* recordDecl, const insieme::core::TypePtr& irType)>;
		std::map<insieme::core::TypePtr, CodeExtractor> placeholderReplacer;

		/// performs the actual type mapping
		core::TypePtr mapType(const clang::Type* type, conversion::Converter& converter);
	};

}
}
}
