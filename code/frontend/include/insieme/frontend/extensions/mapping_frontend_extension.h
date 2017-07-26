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

#include "insieme/frontend/extensions/frontend_extension.h"

#include <string>
#include <regex>
#include <map>
#include <vector>

#include "insieme/frontend/clang.h"
#include "insieme/core/ir_node.h"


namespace insieme {
namespace frontend {
namespace extensions {

	namespace detail {
		/// Entry in the internal type map
		struct TypeMappingEntry;

		/// Entry in the expression map
		class FilterMapper;
	}

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
		 * A subclass can return a map containing the rules on how to map types. The keys in the map are interpreted as regex strings and matched
		 * against the fully qualified name of the declaration of the corresponding type. If the regex matches, then the corresponding value is used
		 * to build a new type. This type construction supports certain special syntax patterns to extract different elements of the translated type.
		 *
		 * For concrete examples on the supported patterns, see the implementation of MappingFrontendExtension::mapType or more concrete, have
		 * a look at the unit test which tests all this functionality at:
		 * @see insieme/code/frontend/test/extensions/mapping_extension_test.cc
		 * @see insieme/code/frontend/test/inputs/conversion/mapping_types.cpp
		 *
		 * Note that this function will only be called once (unless you return an empty map, which is the default implementation).
		 */
		virtual std::map<std::string, std::string> getTypeMappings();

		/**
		 * This is a callback hook which can be used to customize the actual type parsing for the type matching mechanism in this extension.
		 * The default implementation will simply return <tt>builder.parseType(code)</tt>. This should be sufficient for most cases. However,
		 * if a subclass has to customize the type parsing (e.g. by adding additional symbols to the parser loaded from a core extension), this
		 * callback can be used.
		 */
		virtual core::TypePtr parseTypeForTypeMapping(const core::IRBuilder& builder, const std::string& code);

		/**
		 * This is the actual interface for expr mappings.
		 * A subclass can return a vector containing the rules on how to map expressions.
		 * The returned vector contains detail::FilterMapper objects, which allow filtering clang objects based upon regex (and even more) and also
		 * returns an object which can be called to do the actual mapping. detail::FilterMapper provides two conversion constructors supporting
		 * simple name-based regex matching as well as the additional matching on the number of parameters in the declaration to match.
		 *
		 * For concrete examples on the supported patterns, and how to use this mapping facility have a look at the unit test which tests all this
		 * functionality at:
		 * @see insieme/code/frontend/test/extensions/mapping_extension_test.cc
		 * @see insieme/code/frontend/test/inputs/conversion/mapping_types.cpp
		 *
		 * Note that this function will only be called once (unless you return an empty vecor, which is the default implementation).
		 * Also note, that the returned list will always be iterated from beginning to end and thus this allows to specify priorities when matching.
		 */
		virtual std::vector<detail::FilterMapper> getExprMappings();


	  private:
		/// Members used for type mapping
		std::vector<detail::TypeMappingEntry> typeIrMappings;

		using CodeExtractor = std::function<insieme::core::TypePtr(const clang::RecordDecl* recordDecl, const insieme::core::TypePtr& irType)>;
		std::map<insieme::core::TypePtr, CodeExtractor> placeholderReplacer;


		/// Members used for expr mapping
		std::vector<detail::FilterMapper> exprMappings;


		/// performs the actual type mapping
		core::TypePtr mapType(const clang::Type* type, conversion::Converter& converter);

		/// performs the actual expr mapping
		core::ExpressionPtr mapExpr(const clang::Expr* expr, conversion::Converter& converter);
	};


	namespace detail {
		/// Entry in the internal type map
		struct TypeMappingEntry {
			const std::regex pattern;
			const insieme::core::TypePtr irType;
		};


		/// An object passed to the call CallMapper for a matching FilterMapper (see below). Used to represent the clang node and it's arguments.
		struct ClangExpressionInfo {

			const clang::Expr* sourceExpression;

			const unsigned numArgs;

			const std::vector<const clang::Expr*> args;

			const clang::QualType clangType;

			const clang::Expr* implicitObjectArgument;

			const bool isMemberCall;

			const bool isOperatorCall;

			const bool isConstructorCall;

			const clang::SourceLocation locStart;

			insieme::frontend::conversion::Converter& converter;

			static ClangExpressionInfo getClangExpressionInfo(const clang::Expr* expr, conversion::Converter& converter);

		private:
			ClangExpressionInfo(const clang::Expr* sourceExpression, const unsigned numArgs, const std::vector<const clang::Expr*> args,
			                    const clang::QualType clangType, const clang::Expr* implicitObjectArgument,
			                    const bool isMemberCall, const bool isOperatorCall, const bool isConstructorCall, const clang::SourceLocation locStart,
			                    conversion::Converter& converter) :
			                    	sourceExpression(sourceExpression), numArgs(numArgs), args(args), clangType(clangType), implicitObjectArgument(implicitObjectArgument),
			                    	isMemberCall(isMemberCall), isOperatorCall(isOperatorCall),
			                    	isConstructorCall(isConstructorCall), converter(converter) { }
		};

		/// The type used to perform expr mapping. Can be expressed by a class with an overloaded call operator
		using CallMapper = std::function<insieme::core::ExpressionPtr(const ClangExpressionInfo&)>;

		/// A filter matching the qualified name of the passed FunctionDecl against a regex
		class RegexCallFilter {

		  protected:
			const std::string patternString;

			const std::regex pattern;

		  public:
			RegexCallFilter(const std::string& patternString) : patternString(patternString), pattern(std::regex(patternString)) { }

			virtual bool matches(const clang::FunctionDecl* funDecl) const;

			virtual const std::string getFilterRepresentation() const;
		};

		/// A filter matching the qualified name of the passed FunctionDecl against a regex and also comparing the number of parameters
		class NumParamRegexCallFilter : public RegexCallFilter {

			const unsigned numParams;

		  public:
			NumParamRegexCallFilter(const std::string& patternString, const unsigned numParams) : RegexCallFilter(patternString), numParams(numParams) { }

			virtual bool matches(const clang::FunctionDecl* funDecl) const override;

			virtual const std::string getFilterRepresentation() const override;
		};

		/// The mapper which combines a filter and the corresponding CallMapper
		class FilterMapper {

			const std::shared_ptr<RegexCallFilter> filter;

			const CallMapper mapper;

		  public:
			/// Constructs a FilterMapper with the given regexp
			FilterMapper(const char* filterString, const CallMapper& mapper)
					: filter(std::make_shared<RegexCallFilter>(filterString)), mapper(mapper) { }

			/// Constructs a FilterMapper with the given regexp and the number of parameters
			FilterMapper(const char* filterString, const unsigned numParams, const CallMapper& mapper)
					: filter(std::make_shared<NumParamRegexCallFilter>(filterString, numParams)), mapper(mapper) { }

			/// Checks whether this filter matches the given FunctionDecl
			bool matches(const clang::FunctionDecl* funDecl) const {
				return filter->matches(funDecl);
			};

			/// Returns the representation of the used pattern
			const std::string getFilterRepresentation() const {
				return filter->getFilterRepresentation();
			}

			/// Returns the CallMapper used for this Filter
			const CallMapper& getCallMapper() const {
				return mapper;
			}
		};
	}

}
}
}
