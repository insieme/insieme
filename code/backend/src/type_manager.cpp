/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
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

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/annotations/c/include.h"
#include "insieme/annotations/c/decl_only.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/channel.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/lang/const_extension.h"

#include "insieme/utils/logging.h"

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
		struct info_trait<core::StructType> {
			typedef StructTypeInfo type;
		};
		template <>
		struct info_trait<core::TupleType> {
			typedef StructTypeInfo type;
		};
		template <>
		struct info_trait<core::UnionType> {
			typedef UnionTypeInfo type;
		};
		template <>
		struct info_trait<core::FunctionType> {
			typedef FunctionTypeInfo type;
		};
		template <>
		struct info_trait<core::RecType> {
			typedef TypeInfo type;
		};

		class TypeInfoStore {
			const Converter& converter;

			TypeIncludeTable includeTable;

			TypeHandlerList typeHandlers;

			utils::map::PointerMap<core::TypePtr, const TypeInfo*> typeInfos; // < may contain duplicates

			std::set<const TypeInfo*> allInfos;

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
			result_type resolveType(const T& type) {
				// lookup type information using internal mechanism
				const TypeInfo* info = resolveInternal(type);
				assert(dynamic_cast<result_type>(info));
				return static_cast<result_type>(info);
			}

			const TypeInfo* resolveCVectorType(const core::TypePtr& elementType, const c_ast::ExpressionPtr& size);

			const c_ast::CodeFragmentPtr getDefinitionOf(const c_ast::TypePtr& type) const {
				auto pos = std::find_if(allInfos.begin(), allInfos.end(), [&](const TypeInfo* cur) { return *cur->rValueType == *type; });
				if(pos != allInfos.end()) { return (*pos)->definition; }
				return c_ast::CodeFragmentPtr();
			}

		  private:
			// --------------- Internal resolution utilities -----------------

			const TypeInfo* addInfo(const core::TypePtr& type, const TypeInfo* info);
			const TypeInfo* resolveInternal(const core::TypePtr& type);
			const TypeInfo* resolveTypeInternal(const core::TypePtr& type);

			const TypeInfo* resolveTypeVariable(const core::TypeVariablePtr& ptr);
			const TypeInfo* resolveGenericType(const core::GenericTypePtr& ptr);

			const StructTypeInfo* resolveStructType(const core::StructTypePtr& ptr);
			const StructTypeInfo* resolveTupleType(const core::TupleTypePtr& ptr);

			const UnionTypeInfo* resolveUnionType(const core::UnionTypePtr& ptr);

			const ArrayTypeInfo* resolveArrayType(const core::GenericTypePtr& ptr);
			const ArrayTypeInfo* resolveFixedSizedArrayType(const core::GenericTypePtr& ptr);
			const ArrayTypeInfo* resolveVariableSizedArrayType(const core::GenericTypePtr& ptr);
			const ArrayTypeInfo* resolveUnknownSizedArrayType(const core::GenericTypePtr& ptr);

			const ChannelTypeInfo* resolveChannelType(const core::GenericTypePtr& ptr);

			const FunctionTypeInfo* resolveFunctionType(const core::FunctionTypePtr& ptr);
			const FunctionTypeInfo* resolvePlainFunctionType(const core::FunctionTypePtr& ptr);
			const FunctionTypeInfo* resolveMemberFunctionType(const core::FunctionTypePtr& ptr);
			const FunctionTypeInfo* resolveThickFunctionType(const core::FunctionTypePtr& ptr);

			const RefTypeInfo* resolveRefType(const core::GenericTypePtr& ptr);

			const TypeInfo* resolveRecType(const core::RecTypePtr& ptr);
			void resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr);

			template <typename ResInfo>
			ResInfo* resolveNamedCompositType(const core::NamedCompositeTypePtr&, bool);
		};
	}

	TypeManager::TypeManager(const Converter& converter) : store(new detail::TypeInfoStore(converter, getBasicTypeIncludeTable(), TypeHandlerList())) {}

	TypeManager::TypeManager(const Converter& converter, const TypeIncludeTable& includeTable, const TypeHandlerList& handlers)
	    : store(new detail::TypeInfoStore(converter, includeTable, handlers)) {}

	TypeManager::~TypeManager() {
		delete store;
	}


	const TypeInfo& TypeManager::getTypeInfo(const core::TypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const StructTypeInfo& TypeManager::getTypeInfo(const core::StructTypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const StructTypeInfo& TypeManager::getTypeInfo(const core::TupleTypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const UnionTypeInfo& TypeManager::getTypeInfo(const core::UnionTypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const FunctionTypeInfo& TypeManager::getTypeInfo(const core::FunctionTypePtr& type) {
		// just take value from store
		return *(store->resolveType(type));
	}

	const RefTypeInfo& TypeManager::getRefTypeInfo(const core::GenericTypePtr& type) {
		return *(static_cast<const RefTypeInfo*>(store->resolveType(type)));
	}

	const ArrayTypeInfo& TypeManager::getArrayTypeInfo(const core::GenericTypePtr& type) {
		return *(static_cast<const ArrayTypeInfo*>(store->resolveType(type)));
	}

	const ChannelTypeInfo& TypeManager::getChannelTypeInfo(const core::GenericTypePtr& type) {
		return *(static_cast<const ChannelTypeInfo*>(store->resolveType(type)));
	}

	const TypeInfo& TypeManager::getCVectorTypeInfo(const core::TypePtr& elementType, const c_ast::ExpressionPtr& size) {
		// take value from store
		return *(store->resolveCVectorType(elementType, size));
	}


	const c_ast::CodeFragmentPtr TypeManager::getDefinitionOf(const c_ast::TypePtr& type) {
		// as usual, ask store ...
		return store->getDefinitionOf(type);
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

		c_ast::ExpressionPtr NoOp(const c_ast::SharedCNodeManager&, const c_ast::ExpressionPtr& node) {
			return node;
		}
	}

	TypeInfo::TypeInfo() : externalize(&type_info_utils::NoOp), internalize(&type_info_utils::NoOp) {}

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
			typeInfos.insert(std::make_pair(type, info));
			allInfos.insert(info);
			return info;
		}

		// --------------------- Implementations of resolution utilities --------------------

		const TypeInfo* TypeInfoStore::resolveInternal(const core::TypePtr& type) {

			// lookup information within cache
			auto pos = typeInfos.find(type);
			if (pos != typeInfos.end()) return pos->second;

			// resolve information if there is no information yet
			auto info = resolveTypeInternal(type);

			// register information
			addInfo(type, info);

			// done
			return info;
		}


		const TypeInfo* TypeInfoStore::resolveTypeInternal(const core::TypePtr& type) {


			// - Installed Type Handlers -------------------------------------------------------------------

			// try resolving it using type handlers
			for(const auto& handler : typeHandlers) {
				if (const TypeInfo* info = handler(converter, type)) {
					return info;
				}
			}


			// - Header Annotation ------------------------------------------------------------------------

			// if type is annotated with an include file => use the include file
			if(annotations::c::hasIncludeAttached(type) && core::annotations::hasAttachedName(type)) {
				const string& name = core::annotations::getAttachedName(type);
				const string& header = annotations::c::getAttachedInclude(type);
				return type_info_utils::createInfo(converter.getFragmentManager(), name, header);
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
					if(core::lang::isReference(type)) 		info = resolveRefType(genType);
					else if(core::lang::isArray(type))     	info = resolveArrayType(genType);
					else if(core::lang::isChannel(type))   	info = resolveChannelType(genType);
					else info = resolveGenericType(genType);
					break;
				}
				case core::NT_StructType: info = resolveStructType(static_pointer_cast<const core::StructType>(type)); break;
				case core::NT_UnionType: info = resolveUnionType(static_pointer_cast<const core::UnionType>(type)); break;
				case core::NT_TupleType: info = resolveTupleType(static_pointer_cast<const core::TupleType>(type)); break;
				case core::NT_FunctionType: info = resolveFunctionType(static_pointer_cast<const core::FunctionType>(type)); break;
				case core::NT_RecType:
					info = resolveRecType(static_pointer_cast<const core::RecType>(type));
					break;
				//			case core::NT_ChannelType:
				case core::NT_TypeVariable: info = resolveTypeVariable(static_pointer_cast<const core::TypeVariable>(type)); break;
				default:
					// this should not happen ...
					LOG(FATAL) << "Unsupported IR Type encountered: " << type;
					assert_fail() << "Unsupported IR type encountered: " << type->getNodeType() << "\n";
					info = type_info_utils::createUnsupportedInfo(*converter.getCNodeManager());
					break;
			}

			// return pointer to obtained information
			return info;
		}

		const TypeInfo* TypeInfoStore::resolveTypeVariable(const core::TypeVariablePtr& ptr) {
			c_ast::CNodeManager& manager = *converter.getCNodeManager();
			return type_info_utils::createInfo(manager, "<" + ptr->getVarName()->getValue() + ">");
		}

		const TypeInfo* TypeInfoStore::resolveGenericType(const core::GenericTypePtr& ptr) {
			auto& basic = converter.getNodeManager().getLangBasic();
			auto builder = core::IRBuilder(converter.getNodeManager());
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
				} else if(basic.isUInt8(ptr)) {
					type = c_ast::PrimitiveType::UInt64;
				} else if(basic.isUInt16(ptr)) {
					type = c_ast::PrimitiveType::UInt128;
				} else if(basic.isInt1(ptr)) {
					type = c_ast::PrimitiveType::Int8;
				} else if(basic.isInt2(ptr)) {
					type = c_ast::PrimitiveType::Int16;
				} else if(basic.isInt4(ptr)) {
					type = c_ast::PrimitiveType::Int32;
				} else if(basic.isInt8(ptr)) {
					type = c_ast::PrimitiveType::Int64;
				} else if(basic.isInt16(ptr)) {
					type = c_ast::PrimitiveType::Int128;
				} else {
					LOG(FATAL) << "Unsupported integer type: " << *ptr;
					// assert_fail() << "Unsupported Integer type encountered!";
					return type_info_utils::createUnsupportedInfo(manager);
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

			// -------------- type literals -------------

			if(ptr == builder.getTypeLiteralType(builder.typeVariable("a"))) {
				// creates a empty struct and a new type "type"

				// create type literal type
				c_ast::IdentifierPtr name = manager.create("type");
				c_ast::TypePtr typeType = manager.create<c_ast::NamedType>(name);
				c_ast::TypeDefinitionPtr def = manager.create<c_ast::TypeDefinition>(manager.create<c_ast::StructType>(manager.create("_type")), name);

				// also add empty instance
				c_ast::VarDeclPtr decl = manager.create<c_ast::VarDecl>(manager.create<c_ast::Variable>(typeType, manager.create("type_token")));

				c_ast::CodeFragmentPtr code = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), def, decl);

				// add dependency to header
				return type_info_utils::createInfo(typeType, code);
			}

			if(core::analysis::isTypeLiteralType(ptr)) {
				// use same info then for the generic case
				return resolveInternal(builder.getTypeLiteralType(builder.typeVariable("a")));
			}

			if(annotations::c::isDeclOnly(ptr)) {
				// if a genericType has a DeclOnlyAnnotation determine Kind and only declare the type
				switch(annotations::c::getDeclOnlyKind(ptr)) {
				case annotations::c::DeclOnlyTag::Kind::Struct:
				case annotations::c::DeclOnlyTag::Kind::Class: {
					auto name = manager.create(toString(*ptr));
					auto type = manager.create<c_ast::StructType>(name);
					auto forwardDecl = manager.create<c_ast::TypeDeclaration>(type);
					auto decl = c_ast::CCodeFragment::createNew(converter.getFragmentManager());
					decl->appendCode(forwardDecl);
					return type_info_utils::createInfo(type, decl);
				}
				case annotations::c::DeclOnlyTag::Kind::Enum: return type_info_utils::createInfo(manager, "enum " + toString(*ptr));
				case annotations::c::DeclOnlyTag::Kind::Union: return type_info_utils::createInfo(manager, "union " + toString(*ptr));
				}
			}

			// no match found => return unsupported type info
			LOG(FATAL) << "Unsupported type: " << *ptr;
			return type_info_utils::createUnsupportedInfo(manager, toString(*ptr));
		}

		template <typename ResInfo>
		ResInfo* TypeInfoStore::resolveNamedCompositType(const core::NamedCompositeTypePtr& ptr, bool isStruct) {
			// The resolution of a named composite type is based on 3 steps
			//		- first, get a name for the resulting C struct / union
			//		- create a code fragment including a declaration of the struct / union (no definition)
			//		- create a code fragment including a defintion of the sturct / union
			//		- the representation of the struct / union is the same internally and externally

			// get C node manager
			auto manager = converter.getCNodeManager();
			auto fragmentManager = converter.getFragmentManager();

			// fetch a name for the composed type
			string name = converter.getNameManager().getName(ptr, "type");

			// get struct and type name
			c_ast::IdentifierPtr typeName = manager->create(name);

			// create the composite type
			c_ast::NamedCompositeTypePtr type;
			if(isStruct) {
				type = manager->create<c_ast::StructType>(typeName);
			} else {
				type = manager->create<c_ast::UnionType>(typeName);
			}

			// collect dependencies
			std::set<c_ast::CodeFragmentPtr> definitions;

			// add elements
			for(const core::NamedTypePtr& entry : ptr->getEntries()) {
				// get the name of the member
				c_ast::IdentifierPtr name = manager->create(entry->getName()->getValue());
				core::TypePtr curType = entry->getType();

				// special handling of variable sized arrays within structs / unions
				if(core::lang::isUnknownSizedArray(curType)) {

					// construct vector type to be used
					core::TypePtr elementType = core::lang::ArrayType(curType).getElementType();
					const TypeInfo* info = resolveType(elementType);
					auto memberType = manager->create<c_ast::VectorType>(info->rValueType);

					// add member
					type->elements.push_back(var(memberType, name));

					// remember definition
					if(info->definition) { definitions.insert(info->definition); }

					continue;
				}

				// build up the type entry
				const TypeInfo* info = resolveType(curType);
				c_ast::TypePtr elementType = info->rValueType;
				type->elements.push_back(var(elementType, name));

				// remember definitions
				if(info->definition) { definitions.insert(info->definition); }
			}

			// create declaration of named composite type
			auto declCode = manager->create<c_ast::TypeDeclaration>(type);
			c_ast::CodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(fragmentManager, declCode);

			// create definition of named composite type
			auto defCode = manager->create<c_ast::TypeDefinition>(type);
			c_ast::CodeFragmentPtr definition = c_ast::CCodeFragment::createNew(fragmentManager, defCode);
			definition->addDependency(declaration);
			definition->addDependencies(definitions);

			// create resulting type info
			return type_info_utils::createInfo<ResInfo>(type, declaration, definition);
		}

		const StructTypeInfo* TypeInfoStore::resolveStructType(const core::StructTypePtr& ptr) {
			StructTypeInfo* res = resolveNamedCompositType<StructTypeInfo>(ptr, true);

			// get C node manager
			auto manager = converter.getCNodeManager();
			auto fragmentManager = converter.getFragmentManager();


			// ------------- C utilities -----------------

			// define c ast nodes for constructor
			c_ast::NodePtr ifdef = manager->create<c_ast::OpaqueCode>("#ifdef _MSC_VER");
			c_ast::NodePtr endif = manager->create<c_ast::OpaqueCode>("#endif\n");


			// create list of parameters
			vector<c_ast::VariablePtr> params;
			int i = 0;
			for(const core::NamedTypePtr& cur : ptr) {
				params.push_back(c_ast::var(resolveType(cur->getType())->rValueType, format("m%0d", i++)));
			}

			// create struct initialization
			c_ast::VariablePtr resVar = c_ast::var(res->rValueType, "res");
			vector<c_ast::NodePtr> elements(params.begin(), params.end());

			c_ast::StatementPtr body =
			    c_ast::compound(manager->create<c_ast::VarDecl>(resVar, manager->create<c_ast::VectorInit>(elements)), c_ast::ret(resVar));

			// create constructor (C-style)
			string name = converter.getNameManager().getName(ptr, "type");
			c_ast::NodePtr ctr = manager->create<c_ast::Function>(c_ast::Function::INLINE, res->rValueType, manager->create(name + "_ctr"), params, body);

			// add constructor (C-style)
			res->constructor = c_ast::CCodeFragment::createNew(fragmentManager, toVector(ifdef, ctr, endif));


			// ----------------- C++ ---------------------

			// add parent types
			c_ast::StructTypePtr type = static_pointer_cast<c_ast::StructType>(res->lValueType);
			for(auto parent : ptr->getParents()) {
				// resolve parent type
				const TypeInfo* parentInfo = resolveType(parent->getType());

				// add dependency
				res->definition->addDependency(parentInfo->definition);

				// add parent to struct definition
				type->parents.push_back(manager->create<c_ast::Parent>(parent->isVirtual(), parentInfo->lValueType));
			}

			// TODO: port this to new infrastructure

//			// -- Process Meta-Infos --
//
//			// skip rest if there is no meta-info present
//			if(!core::hasMetaInfo(ptr)) { return res; }
//
//			// save current info (otherwise the following code will result in an infinite recursion)
//			addInfo(ptr, res);
//
//			auto& nameMgr = converter.getNameManager();
//			core::ClassMetaInfo info = core::getMetaInfo(ptr);
//			auto& funMgr = converter.getFunctionManager();
//
//			// add constructors
//			for(const core::ExpressionPtr& cur : info.getConstructors()) {
//				assert(cur.isa<core::LambdaExprPtr>() && "metaInfo should only contain ctors which are lambdaExprs at this point");
//				// let function manager handle it
//				funMgr.getInfo(cur.as<core::LambdaExprPtr>());
//			}
//
//			// add destructor
//			if(auto dtor = info.getDestructor()) {
//				assert(dtor.isa<core::LambdaExprPtr>() && "metaInfo should only contain dtor which is a lambdaExprs at this point");
//				// let function manager handle it
//				funMgr.getInfo(dtor.as<core::LambdaExprPtr>(), false, info.isDestructorVirtual());
//			}
//
//			// add member functions
//			for(const core::MemberFunction& cur : info.getMemberFunctions()) {
//				// fix name of all member functions before converting them
//				auto impl = cur.getImplementation();
//				if(!core::analysis::isPureVirtual(impl)) { nameMgr.setName(core::analysis::normalize(impl), cur.getName()); }
//			}
//			for(const core::MemberFunction& cur : info.getMemberFunctions()) {
//				// process function using function manager
//				auto impl = cur.getImplementation();
//				if(!core::analysis::isPureVirtual(impl)) {
//					// might be than there is no implementation for the function?
//					// what about ignoring it and allow backend compiler to synthetize it?
//					if(impl.isa<core::LiteralPtr>()) { continue; }
//
//					// generate code for member function
//					funMgr.getInfo(impl.as<core::LambdaExprPtr>(), cur.isConst(), cur.isVirtual());
//
//				} else {
//					// resolve the type of this function and all its dependencies using the function manager
//					// by asking for an temporary "external function"
//
//					// create temporary literal ...
//					core::NodeManager& nodeMgr = ptr->getNodeManager();
//
//					// ... and resolve dependencies (that's all, function manager will do the rest)
//					funMgr.getInfo(core::Literal::get(nodeMgr, impl.getType(), cur.getName()), cur.isConst());
//				}
//			}

			// done
			return res;
		}

		const UnionTypeInfo* TypeInfoStore::resolveUnionType(const core::UnionTypePtr& ptr) {
			UnionTypeInfo* res = resolveNamedCompositType<UnionTypeInfo>(ptr, false);

			// get C node manager
			auto manager = converter.getCNodeManager();
			auto fragmentManager = converter.getFragmentManager();

			// create list of constructors for members
			int i = 0;
			for(const core::NamedTypePtr& cur : ptr) {
				// define c ast nodes for constructor
				c_ast::NodePtr ifdef = manager->create<c_ast::OpaqueCode>("#ifdef _MSC_VER");
				c_ast::NodePtr endif = manager->create<c_ast::OpaqueCode>("#endif\n");


				// create current parameter
				c_ast::VariablePtr param = c_ast::var(resolveType(cur->getType())->rValueType, "value");

				// create union initialization
				c_ast::VariablePtr resVar = c_ast::var(res->rValueType, "res");

				c_ast::StatementPtr body = c_ast::compound(
				    manager->create<c_ast::VarDecl>(resVar, manager->create<c_ast::VectorInit>(toVector<c_ast::NodePtr>(param))), c_ast::ret(resVar));

				// create constructor
				string name = converter.getNameManager().getName(ptr, "type");
				c_ast::NodePtr ctr = manager->create<c_ast::Function>(c_ast::Function::INLINE, res->rValueType, manager->create(format("%s_ctr_%d", name, i++)),
				                                                      toVector(param), body);

				// add constructor
				res->constructors.push_back(c_ast::CCodeFragment::createNew(fragmentManager, toVector(ifdef, ctr, endif)));
			}

			// done
			return res;
		}

		const StructTypeInfo* TypeInfoStore::resolveTupleType(const core::TupleTypePtr& ptr) {
			// use struct conversion to resolve type => since tuple is represented using structs
			core::IRBuilder builder(ptr->getNodeManager());
			core::NamedCompositeType::Entries entries;
			unsigned counter = 0;
			transform(ptr->getElementTypes(), std::back_inserter(entries),
			          [&](const core::TypePtr& cur) { return builder.namedType(builder.stringValue(format("c%d", counter++)), cur); });

			return resolveStructType(builder.structType(entries));
		}

		const ArrayTypeInfo* TypeInfoStore::resolveArrayType(const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isArray, ptr) << "Can only convert array types.";

			// distribute among more specialized cases

			// fixed size arrays (once known as vectors)
			if (core::lang::isFixedSizedArray(ptr)) {
				return resolveFixedSizedArrayType(ptr);
			}

			// unkown sized arrays, like 'int a[]';
			if (core::lang::isUnknownSizedArray(ptr)) {
				return resolveUnknownSizedArrayType(ptr);
			}

			// final case - variable iszed array
			assert_pred1(core::lang::isVariableSizedArray, ptr);
			return resolveVariableSizedArrayType(ptr);
		}

		const ArrayTypeInfo* TypeInfoStore::resolveFixedSizedArrayType(const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isFixedSizedArray, ptr);

			// start with the parsing of the array type
			core::lang::ArrayType array(ptr);

			// obtain number of elements in array
			unsigned size = array.getNumElements();


			// ----- create the struct representing the array type ------

			const TypeInfo* elementTypeInfo = resolveType(array.getElementType());
			assert_true(elementTypeInfo);

			// check whether the type has been resolved while resolving the sub-type
			auto pos = typeInfos.find(ptr);
			if(pos != typeInfos.end()) {
				assert(dynamic_cast<const ArrayTypeInfo*>(pos->second));
				return static_cast<const ArrayTypeInfo*>(pos->second);
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
			res->externalType = pureVectorType;

			c_ast::IdentifierPtr dataElementName = manager->create("data");

			// create externalizer
			res->externalize = [dataElementName](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
				return (node->getNodeType() == c_ast::NT_Literal) ? node : access(node, dataElementName);
			};

			// create internalizer
			res->internalize = [vectorType](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) { return init(vectorType, node); };

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


		const ArrayTypeInfo* TypeInfoStore::resolveUnknownSizedArrayType(const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isUnknownSizedArray, ptr);

			auto manager = converter.getCNodeManager();

			// ----- create array type representation ------

			const TypeInfo* elementTypeInfo = resolveType(core::lang::ArrayType(ptr).getElementType());
			assert_true(elementTypeInfo);

			// check whether this array type has been resolved while resolving the sub-type (due to recursion)
			auto pos = typeInfos.find(ptr);
			if(pos != typeInfos.end()) {
				assert_true(dynamic_cast<const ArrayTypeInfo*>(pos->second));
				return static_cast<const ArrayTypeInfo*>(pos->second);
			}

			// create array type information
			ArrayTypeInfo* res = new ArrayTypeInfo();

			// create C type (pointer to target type)
			c_ast::TypePtr arrayType = c_ast::vec(elementTypeInfo->rValueType);

			// set L / R value types
			res->lValueType = arrayType;
			res->rValueType = arrayType;
			res->externalType = arrayType;

			// set externalizer / internalizer
			res->externalize = &type_info_utils::NoOp;
			res->internalize = &type_info_utils::NoOp;

			// set declaration / definition (based on element type)
			res->declaration = elementTypeInfo->declaration;
			res->definition = elementTypeInfo->definition;

			// done
			return res;
		}


		const ArrayTypeInfo* TypeInfoStore::resolveVariableSizedArrayType(const core::GenericTypePtr& ptr) {
			assert_pred1(core::lang::isVariableSizedArray, ptr);

			assert_not_implemented();
			return nullptr;

//			// start with the parsing of the array type
//			core::lang::ArrayType array(ptr);
//
//			// ----- create array type representation ------
//
//			const TypeInfo* elementTypeInfo = resolveType(core::lang::ArrayType(ptr).getElementType());
//			assert_true(elementTypeInfo);
//
//			// check whether this array type has been resolved while resolving the sub-type (due to recursion)
//			auto pos = typeInfos.find(ptr);
//			if(pos != typeInfos.end()) {
//				assert_true(dynamic_cast<const ArrayTypeInfo*>(pos->second));
//				return static_cast<const ArrayTypeInfo*>(pos->second);
//			}
//
//			// create array type information
//			ArrayTypeInfo* res = new ArrayTypeInfo();
//
//			// create C type (pointer to target type)
//			c_ast::ExpressionPtr size = converter.getStmtConverter().convert(context, array.getSize());
//			c_ast::TypePtr arrayType = c_ast::vec(elementTypeInfo->rValueType, size);
//
//			// set L / R value types
//			res->lValueType = arrayType;
//			res->rValueType = arrayType;
//			res->externalType = arrayType;
//
//			// set externalizer / internalizer
//			res->externalize = &type_info_utils::NoOp;
//			res->internalize = &type_info_utils::NoOp;
//
//			// set declaration / definition (based on element type)
//			res->declaration = elementTypeInfo->declaration;
//			res->definition = elementTypeInfo->definition;
//
//			// done
//			return res;
		}


		const ChannelTypeInfo* TypeInfoStore::resolveChannelType(const core::GenericTypePtr& ptr) {
			assert_true(core::lang::isChannel(ptr)) << "Can only convert channel types.";
			return type_info_utils::createUnsupportedInfo<ChannelTypeInfo>(*converter.getCNodeManager());
		}

		const RefTypeInfo* TypeInfoStore::resolveRefType(const core::GenericTypePtr& ptr) {
			assert_true(core::lang::isReference(ptr)) << "Can only convert reference types.";

			auto manager = converter.getCNodeManager();

			// parse reference type
			core::lang::ReferenceType ref(ptr);

			// obtain information covering sub-type
			auto elementType = ref.getElementType();
			const TypeInfo* subType = resolveType(elementType);

			// check whether this ref type has been resolved while resolving the sub-type (due to recursion)
			auto pos = typeInfos.find(ptr);
			if(pos != typeInfos.end()) {
				assert(dynamic_cast<const RefTypeInfo*>(pos->second));
				return static_cast<const RefTypeInfo*>(pos->second);
			}

			// create result
			RefTypeInfo* res = new RefTypeInfo();

			// produce R and L value type
			// generally, a level of indirection needs to be added
			res->lValueType = c_ast::modifier(subType->lValueType, ref.isConst(), ref.isVolatile());
			res->rValueType = c_ast::ptr(subType->rValueType, ref.isConst(), ref.isVolatile());

			// handle special nested types
			if (core::lang::isUnknownSizedArray(elementType)) {
				// make it a pointer to the arrays element type
				auto arrayElementType = core::lang::ArrayType(elementType).getElementType();
				const TypeInfo* arrayElementTypeInfo = resolveType(arrayElementType);

				// both, R and L value are just pointers
				res->lValueType = c_ast::ptr(arrayElementTypeInfo->lValueType, ref.isConst(), ref.isVolatile());
				res->rValueType = c_ast::ptr(arrayElementTypeInfo->rValueType, ref.isConst(), ref.isVolatile());

			}
//			if(core::lang::isUnknownSizedArray(elementType)) {
//				// if target is an array, indirection can be skipped (array is always implicitly a reference)
//				res->lValueType = subType->lValueType;
//				res->rValueType = subType->rValueType;
//
//				// special case: if ptr is a source than we have to make the element type of the array const
//				if(ref.isConst()) {
//					assert_eq(res->lValueType, res->rValueType);               // we assume that those are equal (they should)
//					assert_true(res->lValueType.isa<c_ast::PointerTypePtr>()); // the type should also be a pointer
//
//					// get element type
//					auto elementType = res->lValueType.as<c_ast::PointerTypePtr>()->elementType;
//
//					// create new types
//					res->lValueType = c_ast::ptr(c_ast::makeConst(elementType));
//					res->rValueType = res->lValueType;
//				}
//
//			} else if(!core::lang::isReference(elementType)) {
//				// if the target is a non-ref / non-array, on level of indirection can be omitted for local variables (implicit in C)
//				res->lValueType = subType->lValueType;
//
//				// if it is a source, the value type has to be const
//				if(ref.isConst()) {
//					res->lValueType = c_ast::makeConst(subType->lValueType);
//					res->rValueType = c_ast::ptr(res->lValueType);
//				}
//
//			} else if(core::lang::isArray(core::analysis::getReferencedType(ref.getElementType()))) {
//				// if the target is a ref pointing to an array, the implicit indirection of the array needs to be considered
//				res->lValueType = subType->lValueType;
//			}

			// produce external type
			res->externalType = c_ast::ptr(subType->externalType, ref.isConst(), ref.isVolatile());

//			// special handling of references to vectors (implicit pointer in C)
//			if(core::lang::isFixedSizedArray(elementType)) {
//				res->externalType = c_ast::ptr(resolveType(core::lang::ArrayType(elementType).getElementType())->externalType);
//			}
//
//			// special handling of references to arrays (always pointer in C)
//			if(core::lang::isArray(elementType)) { res->externalType = subType->externalType; }

			// add externalization operators
			if(*res->rValueType == *res->externalType) {
				// no difference => do nothing
				res->externalize = &type_info_utils::NoOp;
				res->internalize = &type_info_utils::NoOp;
			} else {
				// requires a cast
				res->externalize = [res](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					return c_ast::cast(res->externalType, node);
				};
				res->internalize = [res](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					return c_ast::cast(res->rValueType, node);
				};
			}

			// special treatment for exporting vectors
			if(core::lang::isFixedSizedArray(elementType)) {
				res->externalize = [res, subType](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					// special treatment for literals (e.g. string literals)
					if(node->getNodeType() == c_ast::NT_Literal && static_pointer_cast<const c_ast::Literal>(node)->value[0] == '\"') { return node; }
					// generated code: ((elementName*)X)
					return c_ast::cast(res->externalType, node);
				};
				res->internalize = [res](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					// generate initializser: (arraytype){node}
					return c_ast::init(res->rValueType, node);
				};
			}

			// the declaration / definition of the sub-type is also the declaration / definition of the pointer type
			res->declaration = subType->declaration;
			res->definition = subType->declaration;


			// ---------------- add a new operator ------------------------

			string newOpName = "_ref_new_" + converter.getNameManager().getName(ptr);
			res->newOperatorName = manager->create(newOpName);

			// the argument variable
			string resultTypeName = toString(c_ast::CPrint(res->rValueType));
			string valueTypeName = toString(c_ast::CPrint(subType->lValueType));

			std::stringstream code;
			code << "/* New Operator for type " << toString(*ptr) << "*/ \n"
			                                                         "static inline "
			     << resultTypeName << " " << newOpName << "(" << valueTypeName << " value) {\n"
			                                                                      "    "
			     << resultTypeName << " res = malloc(sizeof(" << valueTypeName << "));\n"
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

			// done
			return res;
		}


		const FunctionTypeInfo* TypeInfoStore::resolveFunctionType(const core::FunctionTypePtr& ptr) {
			// dispatch to pointer-specific type!
			if(ptr->isPlain()) { return resolvePlainFunctionType(ptr); }
			if(ptr->isMemberFunction()) { return resolveMemberFunctionType(ptr); }
			return resolveThickFunctionType(ptr);
		}

		const FunctionTypeInfo* TypeInfoStore::resolvePlainFunctionType(const core::FunctionTypePtr& ptr) {
			assert_true(ptr->isPlain()) << "Only supported for plain function types!";

			auto manager = converter.getCNodeManager();

			// get name for function type
			auto params = ptr->getParameterTypes();

			FunctionTypeInfo* res = new FunctionTypeInfo();
			res->plain = true;

			// construct the C AST function type token
			const TypeInfo* retTypeInfo = resolveType(ptr->getReturnType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType);

			// add result type dependencies
			vector<c_ast::CodeFragmentPtr> declDependencies;
			declDependencies.push_back(retTypeInfo->declaration);

			// add remaining parameters
			for_each(ptr->getParameterTypes()->getElements(), [&](const core::TypePtr& cur) {
				const TypeInfo* info = resolveType(cur);
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
			c_ast::TypePtr funcType = manager->create<c_ast::NamedType>(funcTypeName, true);
			res->rValueType = c_ast::ptr(funcType);
			res->lValueType = res->rValueType;

			// external type handling
			res->externalType = res->rValueType;
			res->externalize = &type_info_utils::NoOp;
			res->internalize = &type_info_utils::NoOp;

			// ------------ Initialize the rest ------------------

			res->callerName = c_ast::IdentifierPtr();
			res->caller = c_ast::CodeFragmentPtr();
			res->constructorName = c_ast::IdentifierPtr();
			res->constructor = c_ast::CodeFragmentPtr();

			// done
			return res;
		}

		const FunctionTypeInfo* TypeInfoStore::resolveMemberFunctionType(const core::FunctionTypePtr& ptr) {
			assert_true(ptr->isMemberFunction()) << "Only supported for Member function types!";

			auto manager = converter.getCNodeManager();

			// get name for function type
			auto params = ptr->getParameterTypes();

			FunctionTypeInfo* res = new FunctionTypeInfo();
			res->plain = true;

			// construct the C AST function type token, since is a member function, we need to provide the this class type
			const TypeInfo* retTypeInfo = resolveType(ptr->getReturnType());
			const TypeInfo* ownerType = resolveType(core::lang::ReferenceType(params[0]).getElementType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType, ownerType->rValueType);

			// add result type dependencies
			vector<c_ast::CodeFragmentPtr> declDependencies;
			declDependencies.push_back(retTypeInfo->declaration);
			declDependencies.push_back(ownerType->declaration);

			// add remaining parameters
			auto param_it = params.begin() + 1;
			auto param_end = params.end();
			for(; param_it != param_end; ++param_it) {
				const TypeInfo* info = resolveType(*param_it);
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

			// external type handling
			res->externalType = res->rValueType;
			res->externalize = &type_info_utils::NoOp;
			res->internalize = &type_info_utils::NoOp;

			// ------------ Initialize the rest ------------------

			res->callerName = c_ast::IdentifierPtr();
			res->caller = c_ast::CodeFragmentPtr();
			res->constructorName = c_ast::IdentifierPtr();
			res->constructor = c_ast::CodeFragmentPtr();

			// done
			return res;
		}

		const FunctionTypeInfo* TypeInfoStore::resolveThickFunctionType(const core::FunctionTypePtr& ptr) {
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
			const TypeInfo* retTypeInfo = resolveType(ptr->getReturnType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType);

			// add parameter capturing the closure
			c_ast::TypePtr structPointerType = manager->create<c_ast::PointerType>(structType);
			functionType->parameterTypes.push_back(structPointerType);

			// add result type dependencies
			declDependencies.push_back(retTypeInfo->declaration);
			defDependencies.push_back(retTypeInfo->definition);

			// add remaining parameters
			for_each(ptr->getParameterTypes()->getElements(), [&](const core::TypePtr& cur) {
				const TypeInfo* info = resolveType(cur);
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

			// external type handling
			res->externalType = res->rValueType;
			res->externalize = &type_info_utils::NoOp;
			res->internalize = &type_info_utils::NoOp;


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
				                     [&, this](std::ostream& out, const c_ast::TypePtr& cur) {
					                     out << c_ast::ParameterPrinter(cur, manager->create(format("p%d", ++i)));
					                 });
			}
			code << ") {\n    ";
			if(resultTypeName != "void") { code << "return"; }
			code << " closure->call(closure";
			i = 0;
			if(!params.empty()) {
				code << ", " << join(", ", functionType->parameterTypes.begin() + 1, functionType->parameterTypes.end(),
				                     [&, this](std::ostream& out, const c_ast::TypePtr&) { out << format("p%d", ++i); });
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


		const TypeInfo* TypeInfoStore::resolveRecType(const core::RecTypePtr& ptr) {
			// the magic is done by resolving the recursive type definition
			resolveRecTypeDefinition(ptr->getDefinition());

			// look up type again (now it should be known - otherwise this will loop infinitely :) )
			return resolveType(ptr);
		}

		void TypeInfoStore::resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr) {
			// extract some managers required for the task
			core::NodeManager& nodeManager = converter.getNodeManager();
			auto manager = converter.getCNodeManager();
			NameManager& nameManager = converter.getNameManager();

			// the one common declaration block for all types in the recursive block
			c_ast::CCodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(converter.getFragmentManager());

			// A) create a type info instance for each defined type and add definition
			for_each(ptr->getDefinitions(), [&](const core::RecTypeBindingPtr& cur) {

				// create recursive type represented by current type variable
				core::RecTypePtr type = core::RecType::get(nodeManager, cur->getVariable(), ptr);

				// create prototype
				c_ast::IdentifierPtr name = manager->create(nameManager.getName(type, "userdefined_rec_type"));
				// create declaration code
				c_ast::TypePtr cType;

				switch(cur->getType()->getNodeType()) {
				case core::NT_StructType: cType = manager->create<c_ast::StructType>(name); break;
				case core::NT_UnionType: cType = manager->create<c_ast::UnionType>(name); break;
				default: assert_fail() << "Cannot support recursive type which isn't a struct or union!";
				}

				// add declaration
				declaration->appendCode(manager->create<c_ast::TypeDeclaration>(cType));

				// create type info block
				TypeInfo* info = new TypeInfo();
				info->declaration = declaration;
				info->lValueType = cType;
				info->rValueType = cType;
				info->externalType = cType;

				// create the definition block (empty so far)
				info->definition = c_ast::CCodeFragment::createNew(converter.getFragmentManager());

				// register new type information
				typeInfos.insert(std::make_pair(type, info));
				allInfos.insert(info);
			});

			// B) unroll types and add definitions
			for_each(ptr->getDefinitions(), [&](const core::RecTypeBindingPtr& cur) {
				// obtain unrolled type
				core::TypePtr recType = core::RecType::get(nodeManager, cur->getVariable(), ptr);
				core::TypePtr unrolled = ptr->unrollOnce(nodeManager, cur->getVariable());

				// fix name of unrolled struct
				nameManager.setName(unrolled, nameManager.getName(recType));

				// get reference to pointer within map (needs to be updated)
				TypeInfo*& curInfo = const_cast<TypeInfo*&>(typeInfos.at(recType));
				TypeInfo* newInfo = const_cast<TypeInfo*>(resolveType(unrolled));

				assert_true(curInfo && newInfo) << "Both should be available now!";
				assert(curInfo != newInfo);

				// remove dependency to old declaration (would produce duplicated declaration)
				newInfo->definition->remDependency(newInfo->declaration);

				// we move the definition part of the newInfo to the curInfo
				*static_pointer_cast<c_ast::CCodeFragment>(curInfo->definition) = *static_pointer_cast<c_ast::CCodeFragment>(newInfo->definition);

				// also update lValue, rValue and external type
				curInfo->lValueType = newInfo->lValueType;
				curInfo->rValueType = newInfo->rValueType;
				curInfo->externalType = newInfo->externalType;

				// also re-direct the new setup (of the unrolled which might be used in the future)
				newInfo->declaration = curInfo->declaration;
				newInfo->definition = curInfo->definition;
			});
		}

		const TypeInfo* TypeInfoStore::resolveCVectorType(const core::TypePtr& elementType, const c_ast::ExpressionPtr& size) {
			// obtain reference to CNodeManager
			c_ast::CNodeManager& manager = *converter.getCNodeManager();

			// resolve type information
			const TypeInfo* elementInfo = resolveType(elementType);

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
