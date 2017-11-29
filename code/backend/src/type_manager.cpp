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

#include "insieme/backend/type_manager.h"

#include <set>
#include <sstream>

#include <boost/lexical_cast.hpp>
#include <boost/type_traits/remove_const.hpp>

#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/function_manager.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/channel.h"
#include "insieme/core/lang/const_extension.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/reference.h"

#include "insieme/annotations/c/include.h"
#include "insieme/annotations/c/tag.h"
#include "insieme/annotations/c/declaration.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace backend {


	namespace detail {

		template <typename Type>
		struct info_trait;

		template <>
		struct info_trait<core::Type> {
			typedef TypeInfo type;
		};
		template <>
		struct info_trait<core::GenericType> {
			typedef TypeInfo type;
		};
		template <>
		struct info_trait<core::TagType> {
			typedef TagTypeInfo type;
		};
		template <>
		struct info_trait<core::TupleType> {
			typedef TagTypeInfo type;
		};
		template <>
		struct info_trait<core::FunctionType> {
			typedef FunctionTypeInfo type;
		};

		class TypeInfoStore {
			const Converter& converter;

			TypeIncludeTable includeTable;

			TypeHandlerList typeHandlers;

			utils::map::PointerMap<core::TypePtr, std::vector<const TypeInfo*>> typeInfos; // < may contain duplicates

			std::set<const TypeInfo*> allInfos;

			// a set of type infos currently under definition
			std::set<c_ast::CodeFragmentPtr> inDefinition;

		  public:
			TypeInfoStore(const Converter& converter, const TypeIncludeTable& includeTable, const TypeHandlerList& typeHandlers)
			    : converter(converter), includeTable(includeTable), typeHandlers(typeHandlers), typeInfos(), allInfos() {}

			~TypeInfoStore() {
				// free all stored type information instances
				for_each(allInfos, [](const TypeInfo* cur) { delete cur; });
			}

			TypeIncludeTable& getTypeIncludeTable() {
				return includeTable;
			}

			void addTypeHandler(const TypeHandler& handler) {
				typeHandlers.push_back(handler);
			}

			void addTypeHandler(const TypeHandlerList& handler) {
				typeHandlers.insert(typeHandlers.end(), handler.begin(), handler.end());
			}

			/**
			 * Obtains the type information stored for the given function type within this container. If not
			 * present, the information will be automatically, recursively resolved.
			 *
			 * @param type the type for which the requested information should be obtained
			 */
			template <typename T, typename result_type = const typename info_trait<typename boost::remove_const<typename T::element_type>::type>::type*>
			result_type resolveType(ConversionContext& context, const T& type) {
				// lookup type information using internal mechanism
				const TypeInfo* info = resolveInternal(context, type);
				assert_true(info);
				assert_true(dynamic_cast<result_type>(info));
				return static_cast<result_type>(info);
			}

			const TypeInfo* resolveCVectorType(ConversionContext& context, const core::TypePtr& elementType, const c_ast::ExpressionPtr& size);

			const c_ast::CodeFragmentPtr getDefinitionOf(const c_ast::TypePtr& type) const {
				auto pos = std::find_if(allInfos.begin(), allInfos.end(), [&](const TypeInfo* cur) { return *cur->rValueType == *type; });
				if(pos != allInfos.end()) { return (*pos)->definition; }
				return c_ast::CodeFragmentPtr();
			}

		  private:
			// --------------- Internal resolution utilities -----------------

			const TypeInfo* addInfo(const core::TypePtr& type, const TypeInfo* info);
			const TypeInfo* getInfo(const core::TypePtr& type) const;
			void remInfo(const core::TypePtr& type);

			const TypeInfo* resolveInternal(ConversionContext& context, const core::TypePtr& type);
			const TypeInfo* resolveTypeInternal(ConversionContext& context, const core::TypePtr& type);

			const TypeInfo* resolveTypeVariable(ConversionContext& context, const core::TypeVariablePtr& ptr);
			const TypeInfo* resolveNumericType(ConversionContext& context, const core::NumericTypePtr& ptr);
			const TypeInfo* resolveGenericType(ConversionContext& context, const core::GenericTypePtr& ptr);

			const TagTypeInfo* resolveTagType(ConversionContext& context, const core::TagTypePtr& ptr);

			const TagTypeInfo* resolveTupleType(ConversionContext& context, const core::TupleTypePtr& ptr);

			const ArrayTypeInfo* resolveArrayType(ConversionContext& context, const core::GenericTypePtr& ptr);
			const ArrayTypeInfo* resolveFixedSizedArrayType(ConversionContext& context, const core::GenericTypePtr& ptr);
			const ArrayTypeInfo* resolveVariableSizedArrayType(ConversionContext& context, const core::GenericTypePtr& ptr);
			const ArrayTypeInfo* resolveUnknownSizedArrayType(ConversionContext& context, const core::GenericTypePtr& ptr);

			const ChannelTypeInfo* resolveChannelType(ConversionContext& context, const core::GenericTypePtr& ptr);

			const FunctionTypeInfo* resolveFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr);
			const FunctionTypeInfo* resolvePlainFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr);
			const FunctionTypeInfo* resolveMemberFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr);
			const FunctionTypeInfo* resolveThickFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr);

			const RefTypeInfo* resolveRefType(ConversionContext& context, const core::GenericTypePtr& ptr);
		};
	}

	TypeManager::TypeManager(const Converter& converter)
	    : converter(converter), store(new detail::TypeInfoStore(converter, getBasicTypeIncludeTable(), TypeHandlerList())) {}

	TypeManager::TypeManager(const Converter& converter, const TypeIncludeTable& includeTable, const TypeHandlerList& handlers)
	    : converter(converter), store(new detail::TypeInfoStore(converter, includeTable, handlers)) {}

	TypeManager::~TypeManager() {
		delete store;
	}


	const TypeInfo& TypeManager::getTypeInfo(ConversionContext& context, const core::TypePtr& type) {
		// take value from store
		return *(store->resolveType(context, type));
	}

	const TagTypeInfo& TypeManager::getTypeInfo(ConversionContext& context, const core::TagTypePtr& type) {
		// take value from store
		return *(store->resolveType(context, type));
	}

	const TagTypeInfo& TypeManager::getTypeInfo(ConversionContext& context, const core::TupleTypePtr& type) {
		// take value from store
		return *(store->resolveType(context, type));
	}

	const FunctionTypeInfo& TypeManager::getTypeInfo(ConversionContext& context, const core::FunctionTypePtr& type) {
		// just take value from store
		return *(store->resolveType(context, type));
	}

	const RefTypeInfo& TypeManager::getRefTypeInfo(ConversionContext& context, const core::GenericTypePtr& type) {
		return *(static_cast<const RefTypeInfo*>(store->resolveType(context, type)));
	}

	const ArrayTypeInfo& TypeManager::getArrayTypeInfo(ConversionContext& context, const core::GenericTypePtr& type) {
		return *(static_cast<const ArrayTypeInfo*>(store->resolveType(context, type)));
	}

	const ChannelTypeInfo& TypeManager::getChannelTypeInfo(ConversionContext& context, const core::GenericTypePtr& type) {
		return *(static_cast<const ChannelTypeInfo*>(store->resolveType(context, type)));
	}

	const TypeInfo& TypeManager::getCVectorTypeInfo(ConversionContext& context, const core::TypePtr& elementType, const c_ast::ExpressionPtr& size) {
		// take value from store
		return *(store->resolveCVectorType(context, elementType, size));
	}


	const c_ast::CodeFragmentPtr TypeManager::getDefinitionOf(const c_ast::TypePtr& type) {
		// as usual, ask store ...
		return store->getDefinitionOf(type);
	}

	const c_ast::TypePtr TypeManager::getTemplateArgumentType(ConversionContext& context, const core::TypePtr& type) {
		// correctly handle intercepted template template arguments (do not resolve, only use name)
		auto genType = type.isa<core::GenericTypePtr>();
		if(genType && core::analysis::isGeneric(genType) && annotations::c::hasIncludeAttached(genType)) {
			std::string templateParamName = insieme::utils::demangle(genType->getName()->getValue());
			if(core::annotations::hasAttachedName(genType)) {
				templateParamName = core::annotations::getAttachedName(genType);
			}
			return converter.getCNodeManager()->create<c_ast::NamedType>(converter.getCNodeManager()->create<c_ast::Identifier>(templateParamName));
		} else {
			return getTypeInfo(context, type).rValueType;
		}
	}

	TypeIncludeTable& TypeManager::getTypeIncludeTable() {
		return store->getTypeIncludeTable();
	}

	void TypeManager::addTypeHandler(const TypeHandler& handler) {
		store->addTypeHandler(toVector(handler));
	}

	void TypeManager::addTypeHandler(const TypeHandlerList& list) {
		store->addTypeHandler(list);
	}

	namespace type_info_utils {

		const TypeInfo* headerAnnotatedTypeHandler(const Converter& converter, const core::TypePtr& type,
		                                           std::function<void(std::string&, const core::TypePtr&)> nameModifier) {
			if(annotations::c::hasIncludeAttached(type) && core::annotations::hasAttachedName(type)) {
				const string& header = annotations::c::getAttachedInclude(type);
				string name = core::annotations::getAttachedName(type);
				// modify name. E.g., if it is a struct or union type, add the relevant keyword to the name
				nameModifier(name, type);
				return type_info_utils::createInfo(converter.getFragmentManager(), name, header);
			}
			return nullptr;
		}

		c_ast::ExpressionPtr NoOp(const c_ast::SharedCNodeManager&, const c_ast::ExpressionPtr& node) {
			return node;
		}
	}

	TypeInfo::TypeInfo() {}

	namespace detail {

		string getName(const Converter& converter, const core::TypePtr& type) {
			// for generic types it is clear
			if(type->getNodeType() == core::NT_GenericType) { return static_pointer_cast<const core::GenericType>(type)->getFamilyName(); }

			// for other types, use name resolution
			return converter.getNameManager().getName(type);
		}

		// --------------------- Type Specific Wrapper --------------------

		const TypeInfo* TypeInfoStore::addInfo(const core::TypePtr& type, const TypeInfo* info) {

			// register type information
			typeInfos[type].push_back(info);
			allInfos.insert(info);
			return info;
		}

		const TypeInfo* TypeInfoStore::getInfo(const core::TypePtr& type) const {
			auto pos = typeInfos.find(type);
			return (pos != typeInfos.end()) ? pos->second.back() : nullptr;
		}

		void TypeInfoStore::remInfo(const core::TypePtr& type) {
			// check if there are any infos
			auto pos = typeInfos.find(type);
			if (pos == typeInfos.end()) return;

			// remove last element
			pos->second.pop_back();

			// remove empty lists
			if (pos->second.empty()) typeInfos.erase(type);
		}


		// --------------------- Implementations of resolution utilities --------------------

		const TypeInfo* TypeInfoStore::resolveInternal(ConversionContext& context, const core::TypePtr& in) {

			// normalize all types
			auto type = core::analysis::normalize(core::analysis::getCanonicalType(in));

			// lookup information within cache
			if (auto info = getInfo(type)) return info;

			// resolve information if there is no information yet
			auto info = resolveTypeInternal(context, type);

			// register information
			addInfo(type, info);

			// done
			return info;
		}


		const TypeInfo* TypeInfoStore::resolveTypeInternal(ConversionContext& context, const core::TypePtr& type) {

			// - Installed Type Handlers -------------------------------------------------------------------

			// try resolving it using type handlers
			for(const auto& handler : typeHandlers) {
				if (const TypeInfo* info = handler(context, type)) {
					return info;
				}
			}


			// - Include Table ----------------------------------------------------------------------------

			// lookup type within include table
			string name = getName(converter, type);
			auto pos2 = includeTable.find(name);
			if(pos2 != includeTable.end()) {
				// create new info referencing a header file
				const string& header = pos2->second;
				return type_info_utils::createInfo(converter.getFragmentManager(), name, header);
			}

			// also test struct variant
			name = "struct " + name;
			pos2 = includeTable.find(name);
			if(pos2 != includeTable.end()) {
				// create new info referencing a header file
				const string& header = pos2->second;
				return type_info_utils::createInfo(converter.getFragmentManager(), name, header);
			}


			// - Basic Conversion -------------------------------------------------------------------------

			// obtain type information
			const TypeInfo* info;

			// dispatch to corresponding sub-type implementation
			switch(type->getNodeType()) {
				case core::NT_GenericType: {
					// distinguish essential abstract types
					auto genType = type.as<core::GenericTypePtr>();
					if(core::lang::isReference(type)) 		info = resolveRefType(context, genType);
					else if(core::lang::isArray(type))     	info = resolveArrayType(context, genType);
					else if(core::lang::isChannel(type))   	info = resolveChannelType(context, genType);
					else info = resolveGenericType(context, genType);
					break;
				}
				case core::NT_TagType:      info = resolveTagType(context, type.as<core::TagTypePtr>()); break;
				case core::NT_TupleType:    info = resolveTupleType(context, type.as<core::TupleTypePtr>()); break;
				case core::NT_FunctionType: info = resolveFunctionType(context, type.as<core::FunctionTypePtr>()); break;
				case core::NT_NumericType:  info = resolveNumericType(context, type.as<core::NumericTypePtr>()); break;

				//			case core::NT_ChannelType:
				case core::NT_TypeVariable: info = resolveTypeVariable(context, type.as<core::TypeVariablePtr>()); break;
				default:
					// this should not happen ...
					LOG(FATAL) << "Unsupported IR Type encountered: " << type;
					assert_fail() << "Unsupported IR type encountered: " << type->getNodeType() << "\n";
					info = type_info_utils::createUnsupportedInfo(*converter.getCNodeManager(), type);
					break;
			}

			// return pointer to obtained information
			return info;
		}

		const TypeInfo* TypeInfoStore::resolveTypeVariable(ConversionContext& context, const core::TypeVariablePtr& ptr) {
			c_ast::CNodeManager& manager = *converter.getCNodeManager();
			return type_info_utils::createUnsupportedInfo(manager, ptr);
		}

		const TypeInfo* TypeInfoStore::resolveNumericType(ConversionContext& context, const core::NumericTypePtr& ptr) {
			c_ast::CNodeManager& manager = *converter.getCNodeManager();
			assert_eq(ptr->getValue()->getNodeType(), core::NT_Literal) << "Non-literal numeric types should not reach the backend";
			auto ident = manager.create<c_ast::Identifier>(ptr->getValue().as<core::LiteralPtr>()->getStringValue());
			c_ast::TypePtr numType = manager.create<c_ast::IntegralType>(ident);
			return type_info_utils::createInfo(numType);
		}

		const TypeInfo* TypeInfoStore::resolveGenericType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			auto& basic = converter.getNodeManager().getLangBasic();
			c_ast::CNodeManager& manager = *converter.getCNodeManager();

			// try find a match
			if(basic.isUnit(ptr)) {
				c_ast::TypePtr voidType = manager.create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Void);
				return type_info_utils::createInfo(voidType);
			}

			// ------------ integers -------------
			if(basic.isInt(ptr)) {
				c_ast::PrimitiveType::CType type;
				if(basic.isUInt1(ptr)) {
					type = c_ast::PrimitiveType::UInt8;
				} else if(basic.isUInt2(ptr)) {
					type = c_ast::PrimitiveType::UInt16;
				} else if(basic.isUInt4(ptr)) {
					type = c_ast::PrimitiveType::UInt32;
				} else if(basic.isUInt8(ptr) || basic.isUIntInf(ptr)) {
					type = c_ast::PrimitiveType::UInt64;
				} else if(basic.isUInt16(ptr)) {
					type = c_ast::PrimitiveType::UInt128;
				} else if(basic.isInt1(ptr)) {
					type = c_ast::PrimitiveType::Int8;
				} else if(basic.isInt2(ptr)) {
					type = c_ast::PrimitiveType::Int16;
				} else if(basic.isInt4(ptr)) {
					type = c_ast::PrimitiveType::Int32;
				} else if(basic.isInt8(ptr) || basic.isUIntInf(ptr)) {
					type = c_ast::PrimitiveType::Int64;
				} else if(basic.isInt16(ptr)) {
					type = c_ast::PrimitiveType::Int128;
				} else {
					LOG(FATAL) << "Unsupported integer type: " << *ptr;
					// assert_fail() << "Unsupported Integer type encountered!";
					return type_info_utils::createUnsupportedInfo(manager, ptr);
				}

				// create primitive type + include dependency
				c_ast::TypePtr intType = manager.create<c_ast::PrimitiveType>(type);
				c_ast::CodeFragmentPtr definition = c_ast::IncludeFragment::createNew(converter.getFragmentManager(), "stdint.h");

				return type_info_utils::createInfo(intType, definition);
			}

			// ------------ Floating Point -------------
			if(basic.isFloat(ptr)) { return type_info_utils::createInfo(manager, "float"); }
			if(basic.isDouble(ptr)) { return type_info_utils::createInfo(manager, "double"); }
			if(basic.isLongDouble(ptr)) { return type_info_utils::createInfo(manager, "long double"); }

			// ------------ Char and Wide char support ----
			if(basic.isChar(ptr)) { return type_info_utils::createInfo(manager, "char"); }
			if(basic.isWChar16(ptr)) {
				return type_info_utils::createInfo(manager, "wchar_t"); // windows likes 16bits wchar
			}
			if(basic.isWChar32(ptr)) {
				return type_info_utils::createInfo(manager, "wchar_t"); // in unix is 32bit wide
			}

			// ------------ string  -----------------------
			if(basic.isString(ptr)) { return type_info_utils::createInfo(manager, "char*"); }


			// ------------- boolean ------------------

			if(basic.isBool(ptr)) {
				// create bool type
				c_ast::TypePtr boolType = manager.create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Bool);

				// add dependency to header
				c_ast::CodeFragmentPtr definition = c_ast::IncludeFragment::createNew(converter.getFragmentManager(), "stdbool.h");

				return type_info_utils::createInfo(boolType, definition);
			}

			auto getNamedType = [&](const core::NodePtr& nodePtr){
				std::string name = ptr->getName()->getValue();
				// only demangle the name for intercepted types
				if(annotations::c::hasIncludeAttached(nodePtr)) name = insieme::utils::demangle(name);
				if(core::annotations::hasAttachedName(ptr)) {
					name = core::annotations::getAttachedName(ptr);
					if(annotations::c::hasAttachedCTag(ptr)) {
						name = annotations::c::getAttachedCTag(ptr) + " " + name;
					}
				}
				return manager.create<c_ast::NamedType>(manager.create<c_ast::Identifier>(name));
			};

			// handle intercepted types and pure declarations
			if(annotations::c::hasIncludeAttached(ptr) || annotations::c::isDeclaration(ptr)) {
				c_ast::NamedTypePtr namedType = getNamedType(ptr);
				c_ast::CodeFragmentPtr definition;
				if(annotations::c::hasIncludeAttached(ptr)) {
					definition = c_ast::IncludeFragment::createNew(converter.getFragmentManager(), annotations::c::getAttachedInclude(ptr));
				}
				// also handle optional template arguments
				for(auto typeArg : ptr->getTypeParameterList()) {
					auto tempParamType = converter.getTypeManager().getTemplateArgumentType(context, typeArg);
					namedType->parameters.push_back(tempParamType);

					// we need to drop qualified-refs here in order to then correctly generate a dependency to the definition of the type
					if(core::lang::isQualifiedReference(typeArg)) typeArg = core::analysis::getReferencedType(typeArg);

					// if argument type is not intercepted, add a dependency on its definition
					auto tempParamTypeInfo = getInfo(typeArg);
					if (tempParamTypeInfo) {
						assert_true(definition) << "Tried to add a dependency to non-existent definition";
						auto def = tempParamTypeInfo->definition;
						if (contains(inDefinition, def)) {
							def = tempParamTypeInfo->declaration;
						}
						definition->addDependency(def);
					}
				}
				// if there is a definition then use it, otherwise create a forward declaration
				if(definition) {
					return type_info_utils::createInfo(namedType, definition);
				} else {
					auto declCode = manager.create<c_ast::TypeDeclaration>(namedType);
					c_ast::CodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), declCode);
					return type_info_utils::createInfo(namedType, declaration);
				}
			}

			// no match found => return unsupported type info
			LOG(FATAL) << "Unsupported type: " << *ptr;
			return type_info_utils::createUnsupportedInfo(manager, ptr);
		}

		namespace {
			// a record is "C-style" if all of its members, constructors and its destructor are defaulted and not explicitly called
			bool isCStyleRecord(const core::RecordPtr& rec) {
				if(rec.hasAttachedValue<UsedMemberTag>()) return false;
				for(auto c : rec->getConstructors()) {
					if(!core::analysis::isaDefaultMember(c)) return false;
				}
				for(auto m : rec->getMemberFunctions()) {
					if(!core::analysis::isaDefaultMember(m)) return false;
				}
				if(rec->hasDestructor()) {
					if(!core::analysis::isaDefaultMember(rec->getDestructor())) return false;
				}
				return rec->getPureVirtualMemberFunctions().empty() && rec->getStaticMemberFunctions().empty();
			}
		}

		const TagTypeInfo* TypeInfoStore::resolveTagType(ConversionContext& context, const core::TagTypePtr& tagType) {

			// NOTE: we simply assume every tag type to be a recursive type

			// extract some managers required for the task
			core::NodeManager& nodeManager = converter.getNodeManager();
			auto manager = converter.getCNodeManager();
			NameManager& nameManager = converter.getNameManager();
			auto fragmentManager = converter.getFragmentManager();

			// the one common declaration block for all types in the recursive block
			c_ast::CCodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager());

			// get list of definitions in this tag type group
			auto definitions = tagType->getDefinition();

			// an index of the defined types
			std::map<core::TagTypeBindingPtr,c_ast::NamedCompositeTypePtr> typeDefinitions;
			std::map<core::TagTypeBindingPtr,c_ast::CodeFragmentPtr> typeDefinitionFragments;
			std::map<core::TagTypeBindingPtr,TagTypeInfo*> typeTypeInfos;

			// A) create empty definitions for all the types in this recursive block
			for(const core::TagTypeBindingPtr& def : definitions) {

				// create recursive type represented by current definition
				core::TagTypePtr tagType = core::TagType::get(nodeManager, def->getTag(), definitions);

				// extract the record
				const core::RecordPtr& record = def->getRecord();

				// process this record
				bool isStruct = record.isa<core::StructPtr>();

				// fetch a name for the composed type
				string name = nameManager.getName(record, "type");

				// create identifier for struct and type name
				c_ast::IdentifierPtr typeName = manager->create(name);

				// create the composite type
				c_ast::NamedCompositeTypePtr type;
				if(isStruct) {
					type = manager->create<c_ast::StructType>(typeName);
				} else {
					type = manager->create<c_ast::UnionType>(typeName);
				}

				// register type definition locally
				typeDefinitions[def] = type;

				// add declaration of this type to declarations of the group
				declaration->appendCode(manager->create<c_ast::TypeDeclaration>(type));

				// create definition of this named composite type
				auto defCode = manager->create<c_ast::TypeDefinition>(type);
				c_ast::CodeFragmentPtr definition = c_ast::CCodeFragment::createNew(fragmentManager, defCode);
				definition->addDependency(declaration);

				// register fragments locally
				typeDefinitionFragments[def] = definition;

				// remember as currently under definition
				inDefinition.insert(definition);

				// create resulting type info
				auto res = type_info_utils::createInfo<TagTypeInfo>(type, declaration, definition);

				// register result locally
				typeTypeInfos[def] = res;

				// save current info (for recursive references)
				addInfo(tagType, res);									// for the full tag type
				addInfo(def->getTag(),res);								// for the tag type reference (temporary)

			}


			// B) add members and member functions for all records in this type
			for(const core::TagTypeBindingPtr& def : definitions) {

				// create recursive type represented by current definition
				core::TagTypePtr tagType = core::TagType::get(nodeManager, def->getTag(), definitions);

				// extract the record
				core::RecordPtr record = def->getRecord();

				// get the definition of this type
				c_ast::NamedCompositeTypePtr type = typeDefinitions[def];

				// get definition fragment of this type
				c_ast::CodeFragmentPtr definition = typeDefinitionFragments[def];

				// ----- fields -----
				for(const core::FieldPtr& entry : record->getFields()) {

					// get the name of the member
					c_ast::IdentifierPtr name = manager->create(insieme::utils::demangle(entry->getName()->getValue()));
					core::TypePtr curType = entry->getType();

					// special handling of variable sized arrays within structs / unions
					if(core::lang::isUnknownSizedArray(curType)) {

						// construct vector type to be used
						core::TypePtr elementType = core::lang::ArrayType(curType).getElementType();
						const TypeInfo* info = resolveType(context, elementType);
						auto memberType = manager->create<c_ast::VectorType>(info->rValueType);

						// add member
						type->elements.push_back(var(memberType, name));

						// remember definition
						if(info->definition) {
							definition->addDependency(info->definition);
						}

						continue;
					}

					// build up the type entry
					const TypeInfo* info = resolveType(context, curType);
					c_ast::TypePtr elementType = info->rValueType;
					type->elements.push_back(var(elementType, name));

					// remember definitions
					if(info->definition) {
						definition->addDependency(info->definition);
					}
				}

				// the function manager is required to convert member functions
				auto& funMgr = converter.getFunctionManager();

				// ----- members -------

				// add constructors, destructors and assignments, except if we are dealing with a C-style record (only default members)
				if(isCStyleRecord(record)) continue;

				// add constructors
				for(const auto& ctor : record->getConstructors()) {
					// skip ctors which are Literals
					if(ctor.isa<core::LambdaExprPtr>()) {
						funMgr.getInfo(context, tagType,ctor.as<core::LambdaExprPtr>());
					}
				}

				//// TODO: explicitly delete missing constructors
				//if (!core::analysis::hasDefaultConstructor(tagType)) {
				//	type->ctors.push_back(manager->create<c_ast::ConstructorPrototype>());
				//}

				// add destructors (only for structs)
				if(record->hasDestructor()) {
					// skip dtor which is a Literal
					if(record->getDestructor().isa<core::LambdaExprPtr>()) {
						auto dtor = record->getDestructor().as<core::LambdaExprPtr>();
						auto info = funMgr.getInfo(context, tagType,dtor);
						info.declaration.as<c_ast::DestructorPrototypePtr>()->isVirtual = record->hasVirtualDestructor();
					}
				}

				// TODO: explicitly delete missing destructor

				// add pure virtual function declarations
				core::IRBuilder builder(def->getNodeManager());
				for(const auto& pureVirtual : record->getPureVirtualMemberFunctions()) {
					auto type = pureVirtual->getType();
					type = (core::analysis::getObjectType(type).isa<core::TagTypeReferencePtr>()) ? tagType->peel(type) : type;
					funMgr.getInfo(context, builder.pureVirtualMemberFunction(pureVirtual->getName(), type));
				}

				// add member functions
				for(const auto& member : record->getMemberFunctions()) {
					funMgr.getInfo(context, tagType, member);
				}

				// add static member functions
				for(const auto& member : record->getStaticMemberFunctions()) {
					// TODO: This is only here, because it can't really be done nicely in the function manager, as we lose the TagType on the way and have no way to get it back - opposed to normal member functions

					// we extract the implementation
					const auto& impl = member->getImplementation().isa<core::LambdaExprPtr>();
					assert_true(impl);
					// get the info for the implementation itself. No special handling needed for static member functions
					auto& info = funMgr.getInfo(context, tagType, impl);
					// set the name function to the fully qualified name including class name to the name of the member function
					info.function->name->name = type->name->name + "::" + member->getNameAsString();

					// now we create a MemberFunction with the translated function and make it static
					auto mfun = converter.getCNodeManager()->create<c_ast::MemberFunction>(type->name, info.function);
					mfun->isStatic = true;
					// as well as a MemberFunctionPrototype
					auto decl = converter.getCNodeManager()->create<c_ast::MemberFunctionPrototype>(mfun, c_ast::BodyFlag::None);

					// The declaration is added to the members of the type
					type->members.push_back(decl);

					// finally we need to add requirements and dependencies for correct code generation
					definition->addRequirement(info.definition);
					info.definition->addDependency(definition);
				}


				// ----- struct or union specific elements -------

				// get the currently produced type information
				auto res = typeTypeInfos[def];

				// add struct specific features
				if (auto strct = record.isa<core::StructPtr>()) {

					// ------------- C utilities -----------------

					// define c ast nodes for constructor
					c_ast::NodePtr ifdef = manager->create<c_ast::OpaqueCode>("#ifdef _MSC_VER");
					c_ast::NodePtr endif = manager->create<c_ast::OpaqueCode>("#endif\n");


					// create list of parameters
					vector<c_ast::VariablePtr> params;
					int i = 0;
					for(const core::FieldPtr& cur : strct->getFields()) {
						params.push_back(c_ast::var(resolveType(context, cur->getType())->rValueType, format("m%0d", i++)));
					}

					// create struct initialization
					c_ast::VariablePtr resVar = c_ast::var(res->rValueType, "res");
					vector<c_ast::NodePtr> elements(params.begin(), params.end());

					c_ast::StatementPtr body =
						c_ast::compound(manager->create<c_ast::VarDecl>(resVar, manager->create<c_ast::VectorInit>(elements)), c_ast::ret(resVar));

					// create constructor (C-style)
					string name = converter.getNameManager().getName(strct, "type");
					c_ast::NodePtr ctr = manager->create<c_ast::Function>(c_ast::Function::INLINE, res->rValueType, manager->create(name + "_ctr"), params, body);

					// add constructor (C-style)
					res->constructors.push_back(c_ast::CCodeFragment::createNew(fragmentManager, toVector(ifdef, ctr, endif)));


					// ----------------- C++ ---------------------

					// add parent types
					c_ast::StructTypePtr type = static_pointer_cast<c_ast::StructType>(res->lValueType);
					for(auto parent : strct->getParents()) {
						// resolve parent type
						const TypeInfo* parentInfo = resolveType(context, parent->getType());

						// add dependency
						res->definition->addDependency(parentInfo->definition);

						// add parent to struct definition
						type->parents.push_back(manager->create<c_ast::Parent>(parent->isVirtual(), parentInfo->lValueType));
					}


				// add union specific features
				} else if (auto unon = record.isa<core::UnionPtr>()) {

					// create list of constructors for members
					int i = 0;
					for(const core::FieldPtr& cur : unon->getFields()) {
						// define c ast nodes for constructor
						c_ast::NodePtr ifdef = manager->create<c_ast::OpaqueCode>("#ifdef _MSC_VER");
						c_ast::NodePtr endif = manager->create<c_ast::OpaqueCode>("#endif\n");


						// create current parameter
						c_ast::VariablePtr param = c_ast::var(resolveType(context, cur->getType())->rValueType, "value");

						// create union initialization
						c_ast::VariablePtr resVar = c_ast::var(res->rValueType, "res");

						c_ast::StatementPtr body = c_ast::compound(
							manager->create<c_ast::VarDecl>(resVar, manager->create<c_ast::VectorInit>(toVector<c_ast::NodePtr>(param))), c_ast::ret(resVar));

						// create constructor
						string name = converter.getNameManager().getName(unon, "type");
						c_ast::NodePtr ctr = manager->create<c_ast::Function>(c_ast::Function::INLINE, res->rValueType, manager->create(format("%s_ctr_%d", name, i++)),
																			  toVector(param), body);

						// add constructor
						res->constructors.push_back(c_ast::CCodeFragment::createNew(fragmentManager, toVector(ifdef, ctr, endif)));
					}


				} else {
					assert_fail() << "Unsupported record type: " << record->getNodeType();
				}

			}

			// C) remove temporary mappings
			for(const core::TagTypeBindingPtr& def : definitions) {
				remInfo(def->getTag());
			}

			// remove in-definition status
			for(const auto& cur : typeDefinitionFragments) {
				inDefinition.erase(cur.second);
			}

			// return information for requested tag type
			return resolveType(context, tagType);
		}

		const TagTypeInfo* TypeInfoStore::resolveTupleType(ConversionContext& context, const core::TupleTypePtr& ptr) {
			// use struct conversion to resolve type => since tuple is represented using structs
			core::IRBuilder builder(ptr->getNodeManager());
			core::FieldList entries;
			unsigned counter = 0;
			transform(ptr->getElementTypes(), std::back_inserter(entries),
			          [&](const core::TypePtr& cur) { return builder.field(builder.stringValue(format("c%d", counter++)), cur); });

			return resolveTagType(context, builder.structType(entries));
		}

		const ArrayTypeInfo* TypeInfoStore::resolveArrayType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isArray, ptr) << "Can only convert array types.";
			assert_false(core::lang::isGenericSizedArray(ptr)) << "Can not handle generic type: " << *ptr;

			// distribute among more specialized cases

			// fixed size arrays (once known as vectors)
			if (core::lang::isFixedSizedArray(ptr)) {
				return resolveFixedSizedArrayType(context, ptr);
			}

			// unkown sized arrays, like 'int a[]';
			if (core::lang::isUnknownSizedArray(ptr)) {
				return resolveUnknownSizedArrayType(context, ptr);
			}

			// final case - variable sized array
			assert_pred1(core::lang::isVariableSizedArray, ptr);
			return resolveVariableSizedArrayType(context, ptr);
		}

		const ArrayTypeInfo* TypeInfoStore::resolveFixedSizedArrayType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isFixedSizedArray, ptr);

			// start with the parsing of the array type
			core::lang::ArrayType array(ptr);

			// obtain number of elements in array
			unsigned size = array.getNumElements();


			// ----- create the struct representing the array type ------

			const TypeInfo* elementTypeInfo = resolveType(context, array.getElementType());
			assert_true(elementTypeInfo);

			// check whether the type has been resolved while resolving the sub-type
			if (auto info = getInfo(ptr)) {
				assert(dynamic_cast<const ArrayTypeInfo*>(info));
				return static_cast<const ArrayTypeInfo*>(info);
			}

			// get the c-ast node manager
			auto manager = converter.getCNodeManager();

			// compose resulting info instance
			ArrayTypeInfo* res = new ArrayTypeInfo();
			string arrayName = converter.getNameManager().getName(ptr);
			auto name = manager->create(arrayName);

			// create L / R value name
			c_ast::NamedTypePtr vectorType = manager->create<c_ast::NamedType>(name);
			res->lValueType = vectorType;
			res->rValueType = vectorType;

			// create the external type
			c_ast::LiteralPtr vectorSize = manager->create<c_ast::Literal>(boost::lexical_cast<string>(size));
			c_ast::TypePtr pureVectorType = manager->create<c_ast::VectorType>(elementTypeInfo->lValueType, vectorSize);

			c_ast::IdentifierPtr dataElementName = manager->create("data");

			// create declaration
			c_ast::StructTypePtr vectorStructType = manager->create<c_ast::StructType>(name);
			vectorStructType->elements.push_back(var(pureVectorType, dataElementName));

			c_ast::NodePtr declaration = manager->create<c_ast::TypeDeclaration>(vectorStructType);
			res->declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), declaration);

			// create definition
			c_ast::NodePtr definition = manager->create<c_ast::TypeDefinition>(vectorStructType);
			res->definition = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), definition);
			res->definition->addDependency(res->declaration);
			res->definition->addDependency(elementTypeInfo->definition);

			// done
			return res;
		}


		const ArrayTypeInfo* TypeInfoStore::resolveUnknownSizedArrayType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isUnknownSizedArray, ptr);

			auto manager = converter.getCNodeManager();

			// ----- create array type representation ------

			const TypeInfo* elementTypeInfo = resolveType(context, core::lang::ArrayType(ptr).getElementType());
			assert_true(elementTypeInfo);

			// check whether this array type has been resolved while resolving the sub-type (due to recursion)
			if (auto info = getInfo(ptr)) {
				assert_true(dynamic_cast<const ArrayTypeInfo*>(info));
				return static_cast<const ArrayTypeInfo*>(info);
			}

			// create array type information
			ArrayTypeInfo* res = new ArrayTypeInfo();

			// create C type (pointer to target type)
			c_ast::TypePtr arrayType = c_ast::vec(elementTypeInfo->rValueType);

			// set L / R value types
			res->lValueType = arrayType;
			res->rValueType = arrayType;

			// set declaration / definition (based on element type)
			res->declaration = elementTypeInfo->declaration;
			res->definition = elementTypeInfo->definition;

			// done
			return res;
		}


		const ArrayTypeInfo* TypeInfoStore::resolveVariableSizedArrayType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isVariableSizedArray, ptr);

			// start with the parsing of the array type
			core::lang::ArrayType array(ptr);

			// ----- create array type representation ------

			const TypeInfo* elementTypeInfo = resolveType(context, core::lang::ArrayType(ptr).getElementType());
			assert_true(elementTypeInfo);

			// create array type information
			ArrayTypeInfo* res = new ArrayTypeInfo();

			// create C type (pointer to target type)
			c_ast::ExpressionPtr size = converter.getStmtConverter().convert(context, array.getSize()).as<c_ast::ExpressionPtr>();
			c_ast::TypePtr arrayType = c_ast::vec(elementTypeInfo->rValueType, size);

			// set L / R value types
			res->lValueType = arrayType;
			res->rValueType = arrayType;

			// set declaration / definition (based on element type)
			res->declaration = elementTypeInfo->declaration;
			res->definition = elementTypeInfo->definition;

			// done
			return res;
		}


		const ChannelTypeInfo* TypeInfoStore::resolveChannelType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			assert_true(core::lang::isChannel(ptr)) << "Can only convert channel types.";
			return type_info_utils::createUnsupportedInfo<ChannelTypeInfo>(*converter.getCNodeManager(), ptr);
		}

		const RefTypeInfo* TypeInfoStore::resolveRefType(ConversionContext& context, const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isReference, ptr) << "Can only convert reference types.";

			// check that it is a plain, C++ or C++ R-Value reference
			assert_true(core::lang::isPlainReference(ptr) || core::lang::isCppReference(ptr) || core::lang::isCppRValueReference(ptr)
			            || core::lang::isQualifiedReference(ptr))
			    << "Unsupported reference type: " << *ptr;

			auto manager = converter.getCNodeManager();

			// parse reference type
			core::lang::ReferenceType ref(ptr);

			// obtain information covering sub-type
			auto elementType = ref.getElementType();
			const TypeInfo* subType = resolveType(context, elementType);

			// check whether this ref type has been resolved while resolving the sub-type (due to recursion)
			if (auto info = getInfo(ptr)) {
				assert(dynamic_cast<const RefTypeInfo*>(info));
				return static_cast<const RefTypeInfo*>(info);
			}

			// create result
			RefTypeInfo* res = new RefTypeInfo();

			// produce R and L value type

			// generally, a level of indirection needs to be added
			switch (ref.getKind()) {
				case core::lang::ReferenceType::Kind::Plain: {

					// local (l-value) values are simply qualified types, the rest is a pointer
					res->lValueType = c_ast::qualify(subType->lValueType, ref.isConst(), ref.isVolatile());
					res->rValueType = c_ast::ptr(c_ast::qualify(subType->rValueType, ref.isConst(), ref.isVolatile()));

					break;
				}

				case core::lang::ReferenceType::Kind::CppReference: {

					assert_false(core::lang::isReference(elementType)) << "Unsupported references to references!";

					// here we just add a reference to the nested type
					res->lValueType = c_ast::ref(subType->lValueType, ref.isConst(), ref.isVolatile());
					res->rValueType = c_ast::ref(subType->rValueType, ref.isConst(), ref.isVolatile());

					break;
				}

				case core::lang::ReferenceType::Kind::CppRValueReference: {

					assert_false(core::lang::isReference(elementType)) << "Unsupported references to references!";

					// here we just add a reference to the nested type
					res->lValueType = c_ast::rvalue_ref(subType->lValueType, ref.isConst(), ref.isVolatile());
					res->rValueType = c_ast::rvalue_ref(subType->rValueType, ref.isConst(), ref.isVolatile());

					break;
				}

				case core::lang::ReferenceType::Kind::Qualified: {

					// local (l-value) values are simply qualified types, the rest is a pointer
					res->lValueType = c_ast::qualify(subType->lValueType, ref.isConst(), ref.isVolatile());
					res->rValueType = res->lValueType;

					break;
				}

				case core::lang::ReferenceType::Kind::Undefined: {
					assert_fail() << "Should not be reachable!"; break;
				}
			}

			// support nested references
			if (core::lang::isReference(elementType)) {
				// for nested references, the lValue type is composed differently
				res->lValueType = c_ast::ptr(subType->lValueType, ref.isConst(), ref.isVolatile());
			}

			// special support for unknown sized arrays
			if (core::lang::isUnknownSizedArray(elementType)) {
				// make it a pointer to the arrays element type
				auto arrayElementType = core::lang::ArrayType(elementType).getElementType();
				const TypeInfo* arrayElementTypeInfo = resolveType(context, arrayElementType);

				// both, R and L value are just pointers
				res->lValueType = c_ast::ptr(c_ast::qualify(arrayElementTypeInfo->lValueType, ref.isConst(), ref.isVolatile()));
				res->rValueType = c_ast::ptr(c_ast::qualify(arrayElementTypeInfo->rValueType, ref.isConst(), ref.isVolatile()));
			}

			// the declaration / definition of the sub-type is also the declaration / definition of the pointer type
			res->declaration = subType->declaration;
			res->definition = subType->declaration;


			// ---------------- add a new operator ------------------------

			if(ref.isPlain()) {

				string newOpName = "_ref_new_" + converter.getNameManager().getName(ptr);
				res->newOperatorName = manager->create(newOpName);

				// the argument variable
				string resultTypeName = toString(c_ast::CPrint(res->rValueType));
				string valueTypeName = toString(c_ast::CPrint(subType->lValueType));

				std::stringstream code;
				code << "/* New Operator for type " << toString(*ptr) << "*/ \n"
					"static inline " << resultTypeName << " " << newOpName << "(" << valueTypeName << " value) {\n"
					"    " << resultTypeName << " res = (" << resultTypeName << ")malloc(sizeof(" << valueTypeName << "));\n"
					"    *res = value;\n"
					"    return res;\n"
					"}\n";

				// attach the new operator
				c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
				res->newOperator = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), cCode);

				// add dependencies
				res->newOperator->addDependency(subType->definition);

				// add include for malloc
				res->newOperator->addInclude(string("stdlib.h"));
			}

			// done
			return res;
		}

		const FunctionTypeInfo* TypeInfoStore::resolveFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr) {
			// dispatch to pointer-specific type!
			if(ptr->isPlain()) { return resolvePlainFunctionType(context, ptr); }
			if(ptr->isMemberFunction()) { return resolveMemberFunctionType(context, ptr); }
			//TODO
			if(ptr->isVirtualMemberFunction()) { assert_not_implemented(); }
			return resolveThickFunctionType(context, ptr);
		}

		const FunctionTypeInfo* TypeInfoStore::resolvePlainFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr) {
			assert_true(ptr->isPlain()) << "Only supported for plain function types!";

			auto manager = converter.getCNodeManager();

			// get name for function type
			auto params = ptr->getParameterTypes();

			FunctionTypeInfo* res = new FunctionTypeInfo();
			res->plain = true;

			// construct the C AST function type token
			const TypeInfo* retTypeInfo = resolveType(context, ptr->getReturnType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType);

			// add result type dependencies
			vector<c_ast::CodeFragmentPtr> declDependencies;
			declDependencies.push_back(retTypeInfo->declaration);

			// add remaining parameters
			for_each(ptr->getParameterTypes()->getElements(), [&](const core::TypePtr& cur) {
				const TypeInfo* info = resolveType(context, cur);
				functionType->parameterTypes.push_back(info->rValueType);
				declDependencies.push_back(info->declaration);
			});

			// get the name and create a typedef
			c_ast::IdentifierPtr funcTypeName = manager->create(converter.getNameManager().getName(ptr));
			c_ast::TypeDefinitionPtr def = manager->create<c_ast::TypeDefinition>(functionType, funcTypeName);
			res->declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), def);

			// construct a dummy fragment combining all dependencies
			res->declaration->addDependencies(declDependencies);
			res->definition = res->declaration;

			// R / L value names
			c_ast::NamedTypePtr funcType = manager->create<c_ast::NamedType>(funcTypeName);
			funcType->isFunctionType = true;		// mark as function
			res->rValueType = funcType;
			res->lValueType = res->rValueType;

			// ------------ Initialize the rest ------------------

			res->callerName = c_ast::IdentifierPtr();
			res->caller = c_ast::CodeFragmentPtr();
			res->constructorName = c_ast::IdentifierPtr();
			res->constructor = c_ast::CodeFragmentPtr();

			// done
			return res;
		}

		const FunctionTypeInfo* TypeInfoStore::resolveMemberFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr) {
			assert_true(ptr->isMemberFunction()) << "Only supported for Member function types!";

			auto manager = converter.getCNodeManager();

			// get name for function type
			auto params = ptr->getParameterTypes();

			FunctionTypeInfo* res = new FunctionTypeInfo();
			res->plain = true;

			// construct the C AST function type token, since is a member function, we need to provide the this class type
			const TypeInfo* retTypeInfo = resolveType(context, ptr->getReturnType());
			const TypeInfo* ownerType = resolveType(context, core::lang::ReferenceType(params[0]).getElementType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType, ownerType->rValueType);

			// add result type dependencies
			vector<c_ast::CodeFragmentPtr> declDependencies;
			declDependencies.push_back(retTypeInfo->declaration);
			declDependencies.push_back(ownerType->declaration);

			// add remaining parameters
			auto param_it = params.begin() + 1;
			auto param_end = params.end();
			for(; param_it != param_end; ++param_it) {
				const TypeInfo* info = resolveType(context, *param_it);
				functionType->parameterTypes.push_back(info->rValueType);
				declDependencies.push_back(info->declaration);
			}

			// get the name and create a typedef  (member functions pointers NEED to be defined with the pointer itself
			// we can not define a member function type and add an * later on.
			c_ast::IdentifierPtr funcTypeName = manager->create(converter.getNameManager().getName(ptr));
			c_ast::TypeDefinitionPtr def = manager->create<c_ast::TypeDefinition>(c_ast::ptr(functionType), funcTypeName);

			// construct a the code fragment corresponding to the declaration
			res->declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), def);
			res->declaration->addDependencies(declDependencies);
			res->definition = res->declaration;

			// R / L value names
			c_ast::TypePtr funcType = manager->create<c_ast::NamedType>(funcTypeName);
			res->rValueType = funcType;
			res->lValueType = res->rValueType;

			// ------------ Initialize the rest ------------------

			res->callerName = c_ast::IdentifierPtr();
			res->caller = c_ast::CodeFragmentPtr();
			res->constructorName = c_ast::IdentifierPtr();
			res->constructor = c_ast::CodeFragmentPtr();

			// done
			return res;
		}

		const FunctionTypeInfo* TypeInfoStore::resolveThickFunctionType(ConversionContext& context, const core::FunctionTypePtr& ptr) {
			assert_false(ptr->isPlain()) << "Only supported for non-plain function types!";

			auto manager = converter.getCNodeManager();

			// create new entry:

			// get name for function type
			string name = converter.getNameManager().getName(ptr, "funType");
			string functorName = name;
			string callerName = name + "_call";
			string ctrName = name + "_ctr";
			auto params = ptr->getParameterTypes();

			FunctionTypeInfo* res = new FunctionTypeInfo();
			res->plain = false;

			// create the struct type defining the closure
			vector<c_ast::CodeFragmentPtr> declDependencies;
			vector<c_ast::CodeFragmentPtr> defDependencies;
			c_ast::StructTypePtr structType = manager->create<c_ast::StructType>(manager->create(name));


			// construct the C AST function type token
			const TypeInfo* retTypeInfo = resolveType(context, ptr->getReturnType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType);

			// add parameter capturing the closure
			c_ast::TypePtr structPointerType = manager->create<c_ast::PointerType>(structType);
			functionType->parameterTypes.push_back(structPointerType);

			// add result type dependencies
			declDependencies.push_back(retTypeInfo->declaration);
			defDependencies.push_back(retTypeInfo->definition);

			// add remaining parameters
			for_each(ptr->getParameterTypes()->getElements(), [&](const core::TypePtr& cur) {
				const TypeInfo* info = resolveType(context, cur);
				functionType->parameterTypes.push_back(info->rValueType);
				declDependencies.push_back(info->declaration);
				defDependencies.push_back(info->definition);
			});


			// construct the function type => struct including a function pointer
			c_ast::VariablePtr varCall = var(c_ast::ptr(functionType), "call");
			structType->elements.push_back(varCall);

			// create declaration
			res->declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), manager->create<c_ast::TypeDeclaration>(structType));

			// create definition
			res->definition = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), manager->create<c_ast::TypeDefinition>(structType));
			res->definition->addDependency(res->declaration);
			res->definition->addDependencies(declDependencies);

			// R / L value names
			c_ast::TypePtr namedType = manager->create<c_ast::NamedType>(manager->create(name));
			res->rValueType = c_ast::ptr(namedType);
			res->lValueType = res->rValueType;


			// ---------------- add caller ------------------------

			res->callerName = manager->create(callerName);

			// TODO: use C AST structures for this function

			// the argument variable
			string resultTypeName = toString(c_ast::CPrint(retTypeInfo->rValueType));

			std::stringstream code;
			code << "/* Type safe function for invoking closures of type " << name << " = " << toString(*ptr) << " */ \n"
			                                                                                                     "static inline "
			     << resultTypeName << " " << callerName << "(" << name << "* closure";

			int i = 0;
			if(!params.empty()) {
				code << ", " << join(", ", functionType->parameterTypes.begin() + 1, functionType->parameterTypes.end(),
				                     [&](std::ostream& out, const c_ast::TypePtr& cur) {
					                     out << c_ast::ParameterPrinter(cur, manager->create(format("p%d", ++i)));
					                 });
			}
			code << ") {\n    ";
			if(resultTypeName != "void") { code << "return"; }
			code << " closure->call(closure";
			i = 0;
			if(!params.empty()) {
				code << ", " << join(", ", functionType->parameterTypes.begin() + 1, functionType->parameterTypes.end(),
				                     [&](std::ostream& out, const c_ast::TypePtr&) { out << format("p%d", ++i); });
			}
			code << ");\n}\n";


			// attach the new operator
			c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
			res->caller = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), cCode);
			res->caller->addDependency(res->definition);
			res->caller->addDependencies(defDependencies);

			// ---------------- add constructor ------------------------

			// Shape of the constructor:
			//
			//  	static inline functorName* name_ctr(functorName* target, caller* call) {
			//			*target = (functorName){call};
			//			return target;
			//		}

			res->constructorName = manager->create(ctrName);

			c_ast::FunctionPtr constructor = manager->create<c_ast::Function>();

			constructor->returnType = res->rValueType;
			constructor->name = res->constructorName;
			constructor->flags = c_ast::Function::STATIC | c_ast::Function::INLINE;
			c_ast::VariablePtr varTarget = c_ast::var(constructor->returnType, "target");
			constructor->parameter.push_back(varTarget);
			constructor->parameter.push_back(varCall);

			c_ast::StatementPtr assign = c_ast::assign(c_ast::deref(varTarget), c_ast::init(namedType, varCall));
			constructor->body = c_ast::compound(assign, c_ast::ret(varTarget));

			res->constructor = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), constructor);
			res->constructor->addDependency(res->definition);
			res->constructor->addDependencies(defDependencies);

			// done
			return res;
		}

		const TypeInfo* TypeInfoStore::resolveCVectorType(ConversionContext& context, const core::TypePtr& elementType, const c_ast::ExpressionPtr& size) {
			// obtain reference to CNodeManager
			c_ast::CNodeManager& manager = *converter.getCNodeManager();

			// resolve type information
			const TypeInfo* elementInfo = resolveType(context, elementType);

			// create vector type to be used within C-AST
			auto cType = manager.create<c_ast::VectorType>(elementInfo->lValueType, size);

			// create new type information
			TypeInfo* res = type_info_utils::createInfo(cType);
			res->declaration = elementInfo->declaration;
			res->definition = elementInfo->definition;

			// register it (for destruction)
			allInfos.insert(res);

			// return created type info
			return res;
		}

	} // end namespace detail

	TypeIncludeTable getBasicTypeIncludeTable() {
		// the basic table is empty
		return TypeIncludeTable();
	}

} // end namespace backend
} // end namespace insieme
