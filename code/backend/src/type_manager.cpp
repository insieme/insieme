/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/types.h"

namespace insieme {
namespace backend {


	namespace detail {

		class TypeInfoStore {

			const Converter& converter;

			utils::map::PointerMap<core::TypePtr, TypeInfo*> typeInfos;

		public:

			TypeInfoStore(const Converter& converter)
				: converter(converter), typeInfos() {}

			~TypeInfoStore() {
				// free all stored type information instances
				for_each(typeInfos, [](const std::pair<core::TypePtr, TypeInfo*>& cur) {
					delete cur.second;
				});
			}

			/**
			 * Obtains the type information stored for the given type within this container. If not
			 * present, the information will be automatically, recursively resolved.
			 *
			 * @param type the type for which the type informations should be assembled
			 */
			TypeInfo* resolveType(const core::TypePtr& type);

			/**
			 * Obtains the type information stored for the given function type within this container. If not
			 * present, the information will be automatically, recursively resolved.
			 *
			 * @param type the type for which the requested information should be obtained
			 */
			FunctionTypeInfo* resolveType(const core::FunctionTypePtr& type);

			RefTypeInfo* resolveType(const core::RefTypePtr& type);

		private:

			// --------------- Internal resolution utilities -----------------

			TypeInfo* resolveInternal(const core::TypePtr& type);

			TypeInfo* resolveGenericType(const core::GenericTypePtr& ptr);

			TypeInfo* resolveNamedCompositType(const core::NamedCompositeTypePtr&, bool);
			TypeInfo* resolveStructType(const core::StructTypePtr& ptr);
			TypeInfo* resolveUnionType(const core::UnionTypePtr& ptr);

			TypeInfo* resolveArrayType(const core::ArrayTypePtr& ptr);
			TypeInfo* resolveVectorType(const core::VectorTypePtr& ptr);
			TypeInfo* resolveChannelType(const core::ChannelTypePtr& ptr);

			TypeInfo* resolveRefOrVectorOrArrayType(const core::TypePtr& ptr);

			TypeInfo* resolveRecType(const core::RecTypePtr& ptr);

			FunctionTypeInfo* resolveFunctionType(const core::FunctionTypePtr& ptr);
			RefTypeInfo* resolveRefType(const core::RefTypePtr& ptr);
		};


	}


	TypeManager::TypeManager(const Converter& converter) : store(new detail::TypeInfoStore(converter)) {}

	TypeManager::~TypeManager() {
		delete store;
	}


	const TypeInfo& TypeManager::getTypeInfo(const core::TypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const FunctionTypeInfo& TypeManager::getTypeInfo(const core::FunctionTypePtr& type) {
		// just take value from store
		return *(store->resolveType(type));
	}

	const RefTypeInfo& TypeManager::getTypeInfo(const core::RefTypePtr& type) {
		// just take value from store
		return *(store->resolveType(type));
	}


	namespace detail {

		c_ast::NodePtr NoOp(const c_ast::NodePtr& node, c_ast::SharedCNodeManager&) {
			return node;
		}

		template<typename T = TypeInfo>
		T* createInfo(const c_ast::TypePtr& type) {
			// construct the type information
			T* res = new T();
			res->lValueType = type;
			res->rValueType = type;
			res->externalType = type;
			res->externalize = &NoOp;
			res->internalize = &NoOp;
			return res;
		}

		template<typename T = TypeInfo>
		T* createInfo(c_ast::CNodeManager& nodeManager, const string& name) {
			c_ast::IdentifierPtr ident = nodeManager.create(name);
			c_ast::TypePtr type = nodeManager.create<c_ast::PrimitiveType>(ident);
			return createInfo<T>(type);
		}

		template<typename T = TypeInfo>
		T* createUnsupportedInfo(c_ast::CNodeManager& nodeManager) {
			return createInfo<T>(nodeManager, "/* UNSUPPORTED TYPE */");
		}

		template<typename T = TypeInfo>
		T* createInfo(const c_ast::TypePtr& type, const c_ast::CodeFragmentPtr& definition) {
			T* res = createInfo<T>(type);
			res->declaration = definition;
			res->definition = definition;
			return res;
		}

		template<typename T = TypeInfo>
		T* createInfo(const c_ast::TypePtr& type,
				const c_ast::CodeFragmentPtr& declaration,
				const c_ast::CodeFragmentPtr& definition) {

			// declaration => definition
			assert(!declaration || definition);

			T* res = createInfo<T>(type);
			res->declaration = declaration;
			res->definition = definition;
			return res;
		}



		// --------------------- Type Specific Wrapper --------------------

		TypeInfo* TypeInfoStore::resolveType(const core::TypePtr& type) {

			// lookup type information using internal mechanism
			return resolveInternal(type);

		}

		FunctionTypeInfo* TypeInfoStore::resolveType(const core::FunctionTypePtr& type) {

			// lookup type information using internal mechanism
			TypeInfo* info = resolveInternal(type);
			assert(dynamic_cast<FunctionTypeInfo*>(info));
			return static_cast<FunctionTypeInfo*>(info);

		}

		RefTypeInfo* TypeInfoStore::resolveType(const core::RefTypePtr& type) {

			// lookup type information using internal mechanism
			TypeInfo* info = resolveInternal(type);
			assert(dynamic_cast<RefTypeInfo*>(info));
			return static_cast<RefTypeInfo*>(info);

		}

		// --------------------- Implementations of resolution utilities --------------------

		TypeInfo* TypeInfoStore::resolveInternal(const core::TypePtr& type) {

			// lookup information within cache
			auto pos = typeInfos.find(type);
			if (pos != typeInfos.end()) {
				return pos->second;
			}

			// obtain type information
			TypeInfo* info;

			// dispatch to corresponding sub-type implementation
			switch(type->getNodeType()) {
			case core::NT_GenericType:
				info = resolveGenericType(static_pointer_cast<const core::GenericType>(type)); break;
			case core::NT_StructType:
				info = resolveStructType(static_pointer_cast<const core::StructType>(type)); break;
			case core::NT_UnionType:
				info = resolveUnionType(static_pointer_cast<const core::UnionType>(type)); break;
			case core::NT_FunctionType:
				info = resolveFunctionType(static_pointer_cast<const core::FunctionType>(type)); break;
			case core::NT_VectorType:
				info = resolveVectorType(static_pointer_cast<const core::VectorType>(type)); break;
			case core::NT_ArrayType:
				info = resolveArrayType(static_pointer_cast<const core::ArrayType>(type)); break;
			case core::NT_RefType:
				info = resolveRefType(static_pointer_cast<const core::RefType>(type)); break;
			case core::NT_RecType:
				info = resolveRecType(static_pointer_cast<const core::RecType>(type)); break;
//			case core::NT_ChannelType:
			default:
				// this should not happen ...
				assert(false && "Unsupported IR type encountered!");
				info = createUnsupportedInfo(*converter.getCNodeManager()); break;
			}

			// store information
			typeInfos.insert(std::make_pair(type, info));

			// return pointer to obtained information
			return info;
		}


		TypeInfo* TypeInfoStore::resolveGenericType(const core::GenericTypePtr& ptr)  {

			auto& basic = converter.getNodeManager().getBasicGenerator();
			c_ast::CNodeManager& manager = *converter.getCNodeManager();

			// try find a match
			if (basic.isUnit(ptr)) {
				return createInfo(manager, "void");
			}
			if (basic.isAnyRef(ptr)) {
				return createInfo(manager, "void*");
			}

			// ------------ integers -------------
			if (basic.isUInt1(ptr)) {
				return createInfo(manager, "unsigned char");
			}
			if (basic.isUInt2(ptr)) {
				return createInfo(manager, "unsigned short");
			}
			if (basic.isUInt4(ptr)) {
				return createInfo(manager, "unsigned int");
			}
			if (basic.isUInt8(ptr)) {
				return createInfo(manager, "unsigned long");
			}

			if (basic.isInt1(ptr)) {
				return createInfo(manager, "char");
			}
			if (basic.isInt2(ptr)) {
				return createInfo(manager, "short");
			}
			if (basic.isInt4(ptr)) {
				return createInfo(manager, "int");
			}
			if (basic.isInt8(ptr)) {
				return createInfo(manager, "long");
			}

			// ------------ Floating Point -------------
			if (basic.isFloat(ptr)) {
				return createInfo(manager, "float");
			}
			if (basic.isDouble(ptr)) {
				return createInfo(manager, "double");
			}

			if (basic.isChar(ptr)) {
				return createInfo(manager, "char");
			}

			if (basic.isString(ptr)) {
				return createInfo(manager, "char*");
			}

			if (basic.isVarList(ptr)) {
				return createInfo(manager, "...");
			}

			// ------------- boolean ------------------

			if (basic.isBool(ptr)) {

				c_ast::IdentifierPtr identInt = manager.create("int");
				c_ast::TypePtr intType = manager.create<c_ast::PrimitiveType>(identInt);
				c_ast::IdentifierPtr identBool = manager.create("bool");
				c_ast::TypePtr boolType = manager.create<c_ast::PrimitiveType>(identBool);

				c_ast::TypeDefinitionPtr def = manager.create<c_ast::TypeDefinition>(intType, identBool);
				c_ast::CCodeFragmentPtr definition = c_ast::CCodeFragment::createNew(converter.getCNodeManager(), def);

				return createInfo(boolType, definition);
			}

			// TODO: replace by work item / work item group concepts
			// check for job types ...
			if(basic.isJob(ptr)) {
				return createInfo(manager, "isbr_Job*");
			}
			if(basic.isThreadGroup(ptr)) {
				return createInfo(manager, "isbr_ThreadGroup");
			}

			// no match found => return unsupported type info
			return createUnsupportedInfo(manager);
		}

		TypeInfo* TypeInfoStore::resolveNamedCompositType(const core::NamedCompositeTypePtr& ptr, bool isStruct) {

			// The resolution of a named composite type is based on 3 steps
			//		- first, get a name for the resulting C struct / union
			//		- create a code fragment including a declaration of the struct / union (no definition)
			//		- create a code fragment including a defintion of the sturct / union
			//		- the representation of the struct / union is the same internally and externally

			// get C node manager
			auto manager = converter.getCNodeManager();

			// fetch a name for the composed type
			string name = converter.getNameManager().getName(ptr, "userdefined_type");

			// get struct and type name
			c_ast::IdentifierPtr typeName = manager->create(name);

			// create the composite type
			c_ast::NamedCompositeTypePtr type;
			if (isStruct) {
				type = manager->create<c_ast::StructType>(typeName);
			} else {
				type = manager->create<c_ast::UnionType>(typeName);
			}

			// collect dependencies
			std::set<c_ast::CodeFragmentPtr> definitions;

			// add elements
			for_each(ptr->getEntries(), [&](const core::NamedCompositeType::Entry& entry) {
				c_ast::IdentifierPtr name = manager->create(entry.first->getName());
				const TypeInfo* info = resolveType(entry.second);
				c_ast::TypePtr elementType = info->lValueType;
				type->elements.push_back(std::make_pair(name, elementType));

				// remember definitons
				if (info->definition) {
					definitions.insert(info->definition);
				}
			});

			// create declaration of named composite type
			auto declCode = manager->create<c_ast::TypeDeclaration>(type);
			c_ast::CodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(manager, declCode);

			// create definition of named composite type
			auto defCode = manager->create<c_ast::TypeDefinition>(type);
			c_ast::CodeFragmentPtr definition = c_ast::CCodeFragment::createNew(manager, defCode);
			definition->addDependencies(definitions);

			// create resulting type info
			return createInfo(type, declaration, definition);
		}

		TypeInfo* TypeInfoStore::resolveStructType(const core::StructTypePtr& ptr) {
			return resolveNamedCompositType(ptr, true);
		}

		TypeInfo* TypeInfoStore::resolveUnionType(const core::UnionTypePtr& ptr) {
			return resolveNamedCompositType(ptr, false);
		}


		TypeInfo* TypeInfoStore::resolveArrayType(const core::ArrayTypePtr& ptr) {
			return createUnsupportedInfo(*converter.getCNodeManager());
		}

		TypeInfo* TypeInfoStore::resolveVectorType(const core::VectorTypePtr& ptr) {
			return createUnsupportedInfo(*converter.getCNodeManager());
		}

		TypeInfo* TypeInfoStore::resolveChannelType(const core::ChannelTypePtr& ptr) {
			return createUnsupportedInfo(*converter.getCNodeManager());
		}

		RefTypeInfo* TypeInfoStore::resolveRefType(const core::RefTypePtr& ptr) {

			auto manager = converter.getCNodeManager();

			// create result
			RefTypeInfo* res = new RefTypeInfo();

			// obtain information covering sub-type
			auto elementNodeType = ptr->getElementType()->getNodeType();
			TypeInfo* subType = resolveType(ptr->getElementType());

			// produce R and L value type
			res->lValueType = subType->lValueType;
			res->rValueType = manager->create<c_ast::PointerType>(subType->lValueType);

			// if child is already a ref => add pointer to R and L value
			if (elementNodeType == core::NT_RefType) {
				res->lValueType = manager->create<c_ast::PointerType>(subType->lValueType);
			}

			// produce external type
			res->externalType = manager->create<c_ast::PointerType>(subType->externalType);

			// special handling of references to vectors and arrays (implicit pointer in C)
			if (elementNodeType == core::NT_ArrayType || elementNodeType == core::NT_VectorType) {
				res->externalType = subType->externalType;
			}

			// add externalization operators
			if (*res->rValueType == *res->externalType) {
				// no difference => do nothing
				res->externalize = &NoOp;
				res->internalize = &NoOp;
			} else {
				// requires a cast
				res->externalize = [res](const c_ast::NodePtr& node, c_ast::SharedCNodeManager&) {
					return c_ast::cast(res->externalType, node);
				};
				res->internalize = [res](const c_ast::NodePtr& node, c_ast::SharedCNodeManager&) {
					return c_ast::cast(res->rValueType, node);
				};
			}

			// special treatment for exporting arrays and vectors
			if (elementNodeType == core::NT_ArrayType || elementNodeType == core::NT_VectorType) {
				res->externalize = [res](const c_ast::NodePtr& node, c_ast::SharedCNodeManager& manager) {
					// generated code: ((externalName)X.data)
					return c_ast::cast(res->externalType, c_ast::access(node, manager->create("data")));
				};
				res->internalize = [res](const c_ast::NodePtr& node, c_ast::SharedCNodeManager&) {
					assert(false && "Not implemented");
					// TODO: call constructor for underlying array type
					return c_ast::cast(res->rValueType, node);
				};
			}

			// add a declaration dependency if necessary
			if(subType->declaration) {
				// add a declaration for the pointer too ...
				res->declaration = subType->declaration;
				res->definition = subType->declaration;
			}


			// ---------------- add a new operator ------------------------

			string newOpName = "_ref_new_" + converter.getNameManager().getName(ptr);
			res->newOperatorName = manager->create(newOpName);

			// the argument variable
			string resultTypeName = toString(c_ast::CPrint(res->rValueType));
			string valueTypeName = toString(c_ast::CPrint(subType->lValueType));

			std::stringstream code;
			code << "/* New Operator for type " << toString(*ptr) << "*/ \n"
					"static inline " << resultTypeName << " " << newOpName << "(" << valueTypeName << " value) {\n"
					"    " << resultTypeName << " res = malloc(sizeof(" << valueTypeName << "));\n"
					"    *res = value;\n"
					"    return res;\n"
					"}\n";

			// attach the new operator
			c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
			res->newOperator = c_ast::CCodeFragment::createNew(manager, cCode);

			// done
			return res;
		}

		TypeInfo* TypeInfoStore::resolveRecType(const core::RecTypePtr& ptr) {
			return createUnsupportedInfo(*converter.getCNodeManager());
		}

		void resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr);


		FunctionTypeInfo* TypeInfoStore::resolveFunctionType(const core::FunctionTypePtr& ptr) {
			assert(false && "Not implemented!");
			return new FunctionTypeInfo();
		}

	}

} // end namespace backend
} // end namespace insieme
