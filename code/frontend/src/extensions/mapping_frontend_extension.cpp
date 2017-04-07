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
		return nullptr;
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

	//----------- Implementation -----------------------------------------------------------------------------------------------------------------------#

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
				placeholderReplacer[tupleTypeExtractorType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
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
					break;
				}
			}
		}
		return ret;
	}

} // extensions
} // frontend
} // insieme
