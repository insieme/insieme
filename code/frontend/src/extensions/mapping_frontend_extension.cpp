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

#include "insieme/frontend/extensions/mapping_frontend_extension.h"

#include <regex>

#include "insieme/frontend/converter.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/container_utils.h"


namespace insieme {
namespace frontend {
namespace extensions {

	/// Pre-visit expressions
	core::ExpressionPtr MappingFrontendExtension::Visit(const clang::Expr* expr, conversion::Converter& converter) {
		return mapExpr(expr, converter);
	}

	/// Pre-visit types
	core::TypePtr MappingFrontendExtension::Visit(const clang::QualType& typeIn, conversion::Converter& converter) {
		const clang::Type* type = typeIn->getUnqualifiedDesugaredType();

		// Apply the type mapping specification table to record types
		if(auto mappedType = mapType(type, converter)) {
			return mappedType;
		}

		return nullptr;
	}

	/// The default type mappings are empty
	std::map<std::string, std::string> MappingFrontendExtension::getTypeMappings() {
		return {};
	}

	/// The default is to simply parse the type using the builder
	core::TypePtr MappingFrontendExtension::parseTypeForTypeMapping(const core::IRBuilder& builder, const std::string& code) {
		return builder.parseType(code);
	}

	/// The default expr mappings are empty
	std::vector<detail::FilterMapper> MappingFrontendExtension::getExprMappings() {
		return {};
	}

	//----------- Implementation -----------------------------------------------------------------------------------------------------------------------#

	/// Set this flag to true to get some debug output during type and expr mapping
	static bool debug = false;

	/// Maximum number of template arguments we can map
	const unsigned MAX_MAPPED_TEMPLATE_ARGS = 8;

	namespace {

		/// Extract type argument #id from a C++ template instantiation declared by recordDecl
		core::TypePtr extractTemplateTypeArgument(const clang::RecordDecl* recordDecl, int id, conversion::Converter& converter) {
			if(auto specializedDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(recordDecl)) {
				int i = 0;
				for(auto a : specializedDecl->getTemplateArgs().asArray()) {
					if(a.getKind() == clang::TemplateArgument::Type) {
						if(i == id) {
							return converter.convertType(a.getAsType());
						}
						i++;
					}
				}
			}
			return {};
		}
		/// Extract type arguments at #id from a C++ template type argument pack in a recordDecl
		core::TypeList extractTemplateTypeArgumentPack(const clang::RecordDecl* recordDecl, int id, conversion::Converter& converter) {
			core::TypeList ret;
			if(auto specializedDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(recordDecl)) {
				int i = 0;
				for(auto a : specializedDecl->getTemplateArgs().asArray()) {
					if(a.getKind() == clang::TemplateArgument::Pack) {
						if(i == id) {
							for(auto inner : a.getPackAsArray()) {
								if(inner.getKind() == clang::TemplateArgument::Type) {
									ret.push_back(converter.convertType(inner.getAsType()));
								}
							}
							break;
						}
						i++;
					}
				}
			}
			return ret;
		}
		/// Extract type argument #id from a C++ template instantiation declared by the type enclosing recordDecl
		core::TypeList extractEnclosingTemplateTypeArgumentPack(const clang::RecordDecl* recordDecl, int id, conversion::Converter& converter) {
			if(auto cxxRecordDecl = llvm::dyn_cast<clang::CXXRecordDecl>(recordDecl)) {
				auto enclosingRecordDecl = llvm::dyn_cast<clang::CXXRecordDecl>(cxxRecordDecl->getDeclContext());
				assert_true(enclosingRecordDecl) << "Enclosing context is not a CXXRecordDecl";
				return extractTemplateTypeArgumentPack(enclosingRecordDecl, id, converter);
			}
			return {};
		}
	}

	core::TypePtr MappingFrontendExtension::mapType(const clang::Type* type, conversion::Converter& converter) {
		// initialization once
		if(typeIrMappings.empty()) {
			auto& builder = converter.getIRBuilder();

			// generate type to ir map from string-based type map
			for(auto stringMapping : getTypeMappings()) {
				auto ir = parseTypeForTypeMapping(builder, stringMapping.second);
				typeIrMappings.push_back({ std::regex(stringMapping.first), ir });
			}

			// generate the template placeholder replacers to be applied on mapped IR
			for(unsigned i = 0; i < MAX_MAPPED_TEMPLATE_ARGS; ++i) {
				std::string name = ::format("TEMPLATE_T_%u", i);
				// single argument
				auto singleType = parseTypeForTypeMapping(builder, name);
				placeholderReplacer[singleType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
					return extractTemplateTypeArgument(recordDecl, i, converter);
				};

				// variadic argument
				name = ::format("('%s...)", name);
				auto variadicType = parseTypeForTypeMapping(builder, name);
				placeholderReplacer[variadicType] = [&converter, &builder, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
					return builder.tupleType(extractTemplateTypeArgumentPack(recordDecl, i, converter));
				};

				// type extraction from tuple types
				name = ::format("TUPLE_TYPE_%u", i);
				auto tupleTypeExtractorType = parseTypeForTypeMapping(builder, name);
				placeholderReplacer[tupleTypeExtractorType] = [i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
					return irType.as<core::GenericTypePtr>()->getTypeParameter(0).as<core::TupleTypePtr>()->getElement(i);
				};

				// variadic argument from enclosing type
				name = ::format("('ENCLOSING_TEMPLATE_T_%u...)", i);
				auto enclosingSingleType = parseTypeForTypeMapping(builder, name);
				placeholderReplacer[enclosingSingleType] = [&converter, &builder, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
					return builder.tupleType(extractEnclosingTemplateTypeArgumentPack(recordDecl, i, converter));
				};
			}
		}

		// perform the actual mapping
		core::TypePtr ret;
		// we are only interested in Record Types for now
		if(auto recordType = llvm::dyn_cast<clang::RecordType>(type)) {
			auto recordDecl = recordType->getDecl();
			auto name = recordDecl->getQualifiedNameAsString();
			if(debug) std::cout << "Trying to match type: " << name << std::endl;

			// replace if we have a map entry matching this name
			for(const auto& mapping : typeIrMappings) {
				if(std::regex_match(name, mapping.pattern)) {
					ret = mapping.irType;
					core::IRBuilder builder(ret->getNodeManager());
					// replace all placeholders in generated IR type
					ret = core::transform::transformBottomUpGen(ret, [&](const core::TypePtr& typeIn) -> core::TypePtr {
						auto typeForMatching = typeIn;
						if(auto genTy = typeIn.isa<core::GenericTypePtr>()) {
							typeForMatching = builder.genericType(genTy->getName()->getValue());
						}
						if(::containsKey(placeholderReplacer, typeForMatching)) {
							return placeholderReplacer[typeForMatching](recordDecl, typeIn);
						}
						return typeIn;
					});
					if(debug) std::cout << "  Found match: " << *ret << std::endl;
					break;
				}
			}
		}
		return ret;
	}

	core::ExpressionPtr MappingFrontendExtension::mapExpr(const clang::Expr* expr, conversion::Converter& converter) {
		// initialization once
		if(exprMappings.empty()) {
			exprMappings = getExprMappings();
		}

		// perform the actual mapping
		// we handle certain calls specially, which we differentiate by their callee's name
		const clang::Decl* decl = nullptr;

		// the entries in our expression mappings apply to calls and constructor calls
		if(auto call = llvm::dyn_cast<clang::CallExpr>(expr)) {
			decl = call->getCalleeDecl();
		}
		if(auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(expr)) {
			decl = constructExpr->getConstructor();
		}

		// if we found a decl, we get it's fully qualified name and do a lookup in our map
		if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
			auto name = funDecl->getQualifiedNameAsString();
			if(debug) {
				std::cout << "Trying to match expr: " << name << "  from line: "
						<< insieme::frontend::utils::location(funDecl->getLocStart(), funDecl->getASTContext().getSourceManager()) << std::endl;
			}

			for(const auto& mapping : exprMappings) {
				if(mapping.matches(funDecl)) {
					if(debug) std::cout << "  Found match: " << mapping.getFilterRepresentation() << std::endl;
					// perform the actual mapping
					return mapping.getCallMapper()(detail::ClangExpressionInfo::getClangExpressionInfo(expr, converter));
				}
			}
		}

		return nullptr;
	}


	namespace detail {
		ClangExpressionInfo ClangExpressionInfo::getClangExpressionInfo(const clang::Expr* expr, conversion::Converter& converter) {
			// check preconditions
			auto callExpr = llvm::dyn_cast<clang::CallExpr>(expr);
			auto memberCallExpr = llvm::dyn_cast<clang::CXXMemberCallExpr>(expr);
			auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(expr);
			assert_true(callExpr || constructExpr) << "Passed expr must either be a CallExpr or CXXConstructExpr";

			// fill fields
			auto numArgs = callExpr ? callExpr->getNumArgs() : constructExpr->getNumArgs();
			std::vector<const clang::Expr*> args;
			for(auto arg : (callExpr ? callExpr->arguments() : constructExpr->arguments())) {
				args.push_back(arg);
			}
			auto clangType = expr->getType();
			auto implicitObjectArgument = memberCallExpr ? memberCallExpr->getImplicitObjectArgument() : nullptr;
			auto isMemberCall = llvm::isa<clang::CXXMemberCallExpr>(expr);
			auto isOperatorCall = llvm::isa<clang::CXXOperatorCallExpr>(expr);
			auto isConstructorCall = llvm::isa<clang::CXXConstructExpr>(expr);
			auto locStart = expr->getLocStart();

			return {expr, numArgs, args, clangType, implicitObjectArgument, isMemberCall, isOperatorCall, isConstructorCall, locStart, converter};
		}

		bool RegexCallFilter::matches(const clang::FunctionDecl* funDecl) const {
			return std::regex_match(funDecl->getQualifiedNameAsString(), pattern);
		}

		const std::string RegexCallFilter::getFilterRepresentation() const {
			return patternString;
		}

		bool NumParamRegexCallFilter::matches(const clang::FunctionDecl* funDecl) const {
			unsigned clangNumParams = funDecl->getNumParams();
			auto primaryTemplate = funDecl->getPrimaryTemplate();
			if(primaryTemplate) {
				clangNumParams = primaryTemplate->getTemplatedDecl()->getNumParams();
			}
			return clangNumParams == numParams && std::regex_match(funDecl->getQualifiedNameAsString(), pattern);
		}

		const std::string NumParamRegexCallFilter::getFilterRepresentation() const {
			return patternString + " [" + toString(numParams) + " params version]";
		}
	}

} // extensions
} // frontend
} // insieme
