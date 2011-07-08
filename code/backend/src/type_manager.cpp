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

#include <boost/lexical_cast.hpp>
#include <boost/type_traits/remove_const.hpp>

#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/types.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {


	namespace detail {

		template<typename Type>
		struct info_trait;

		template<> struct info_trait<core::Type> { typedef TypeInfo type; };
		template<> struct info_trait<core::GenericType> { typedef TypeInfo type; };
		template<> struct info_trait<core::RefType> { typedef RefTypeInfo type; };
		template<> struct info_trait<core::ArrayType> { typedef ArrayTypeInfo type; };
		template<> struct info_trait<core::VectorType> { typedef VectorTypeInfo type; };
		template<> struct info_trait<core::ChannelType> { typedef ChannelTypeInfo type; };
		template<> struct info_trait<core::FunctionType> { typedef FunctionTypeInfo type; };
		template<> struct info_trait<core::RecType> { typedef TypeInfo type; };


		class TypeInfoPtr {
			TypeInfo* ptr;
			bool owner;

		public:
			TypeInfoPtr(TypeInfo* ptr, bool owner=true)
				: ptr(ptr), owner(owner) {}

			operator TypeInfo*() const {
				return ptr;
			}

			TypeInfo* getPointer() const {
				return ptr;
			}

			bool isOwner() const {
				return owner;
			}
		};

		class TypeInfoStore {

			const Converter& converter;

			utils::map::PointerMap<core::TypePtr, TypeInfoPtr> typeInfos;

		public:

			TypeInfoStore(const Converter& converter)
				: converter(converter), typeInfos() {}

			~TypeInfoStore() {
				// free all stored type information instances
				for_each(typeInfos, [](const std::pair<core::TypePtr, TypeInfoPtr>& cur) {
					if (cur.second.isOwner()) delete cur.second.getPointer();
				});
			}



			/**
			 * Obtains the type information stored for the given function type within this container. If not
			 * present, the information will be automatically, recursively resolved.
			 *
			 * @param type the type for which the requested information should be obtained
			 */
			template<
				typename T,
				typename result_type = typename info_trait<typename boost::remove_const<typename T::element_type>::type>::type*
			>
			result_type resolveType(const T& type) {
				// lookup type information using internal mechanism
				TypeInfo* info = resolveInternal(type);
				assert(dynamic_cast<result_type>(info));
				return static_cast<result_type>(info);
			}

		private:

			// --------------- Internal resolution utilities -----------------

			TypeInfo* resolveInternal(const core::TypePtr& type);

			TypeInfo* resolveTypeVariable(const core::TypeVariablePtr& ptr);
			TypeInfo* resolveGenericType(const core::GenericTypePtr& ptr);

			TypeInfo* resolveNamedCompositType(const core::NamedCompositeTypePtr&, bool);
			TypeInfo* resolveStructType(const core::StructTypePtr& ptr);
			TypeInfo* resolveUnionType(const core::UnionTypePtr& ptr);

			ArrayTypeInfo* resolveArrayType(const core::ArrayTypePtr& ptr);
			VectorTypeInfo* resolveVectorType(const core::VectorTypePtr& ptr);
			ChannelTypeInfo* resolveChannelType(const core::ChannelTypePtr& ptr);

			FunctionTypeInfo* resolveFunctionType(const core::FunctionTypePtr& ptr);
			RefTypeInfo* resolveRefType(const core::RefTypePtr& ptr);

			TypeInfo* resolveRecType(const core::RecTypePtr& ptr);
			void resolveRecTypeDefinition(const core::RecTypeDefinitionPtr& ptr);
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

	const ArrayTypeInfo& TypeManager::getTypeInfo(const core::ArrayTypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const VectorTypeInfo& TypeManager::getTypeInfo(const core::VectorTypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}

	const ChannelTypeInfo& TypeManager::getTypeInfo(const core::ChannelTypePtr& type) {
		// take value from store
		return *(store->resolveType(type));
	}


	namespace detail {

		c_ast::ExpressionPtr NoOp(const c_ast::SharedCNodeManager&, const c_ast::ExpressionPtr& node) {
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
			c_ast::TypePtr type = nodeManager.create<c_ast::NamedType>(ident);
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
			case core::NT_TypeVariable:
				info = resolveTypeVariable(static_pointer_cast<const core::TypeVariable>(type)); break;
			default:
				// this should not happen ...
				LOG(FATAL) << "Unsupported IR Type encountered: " << type;
				assert(false && "Unsupported IR type encountered!");
				info = createUnsupportedInfo(*converter.getCNodeManager()); break;
			}

			// store information
			typeInfos.insert(std::make_pair(type, info));

			// return pointer to obtained information
			return info;
		}

		TypeInfo* TypeInfoStore::resolveTypeVariable(const core::TypeVariablePtr& ptr) {
			c_ast::CNodeManager& manager = *converter.getCNodeManager();
			return createInfo(manager, "<" + ptr->getVarName() + ">");
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
				return createInfo(manager.create<c_ast::VarArgsType>());
			}

			// ------------- boolean ------------------

			if (basic.isBool(ptr)) {

				c_ast::TypePtr intType = manager.create<c_ast::PrimitiveType>(c_ast::PrimitiveType::INT);

				c_ast::IdentifierPtr identBool = manager.create("bool");
				c_ast::TypePtr boolType = manager.create<c_ast::NamedType>(identBool);
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
				c_ast::TypePtr elementType = info->rValueType;
				type->elements.push_back(var(elementType, name));

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


		ArrayTypeInfo* TypeInfoStore::resolveArrayType(const core::ArrayTypePtr& ptr) {
			auto manager = converter.getCNodeManager();

			// obtain dimension of array
			unsigned dim = 0;
			const core::IntTypeParamPtr& dimPtr = ptr->getDimension();
			if (dimPtr->getNodeType() != core::NT_ConcreteIntTypeParam) {
				// non-concrete array types are not supported
				return createUnsupportedInfo<ArrayTypeInfo>(*manager);
			} else {
				dim = static_pointer_cast<const core::ConcreteIntTypeParam>(dimPtr)->getValue();
			}

			bool supportSize = converter.getConfig().supportArrayLength;

			// ----- create the struct representing the array type ------

			const TypeInfo* elementTypeInfo = resolveType(ptr->getElementType());
			assert(elementTypeInfo);

			// check whether this array type has been resolved while resolving the sub-type (due to recursion)
			auto pos = typeInfos.find(ptr);
			if (pos != typeInfos.end()) {
				assert(dynamic_cast<ArrayTypeInfo*>(pos->second.getPointer()));
				return static_cast<ArrayTypeInfo*>(pos->second.getPointer());
			}

			// create array type information
			ArrayTypeInfo* res = new ArrayTypeInfo();
			string arrayName = converter.getNameManager().getName(ptr);
			auto name = manager->create(arrayName);
			auto structName = manager->create("_" + arrayName);

			// create L / R value name
			c_ast::NamedTypePtr arrayType = manager->create<c_ast::NamedType>(name);
			res->lValueType = arrayType;
			res->rValueType = arrayType;

			// create the external type
			c_ast::TypePtr dataPointer = elementTypeInfo->lValueType;
			for (unsigned i=0; i<dim; i++) {
				dataPointer = manager->create<c_ast::PointerType>(dataPointer);
			}
			res->externalType = dataPointer;

			c_ast::IdentifierPtr dataPointerName = manager->create("data");

			// create externalizer
			res->externalize = [dataPointerName](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
				return access(parenthese(node), dataPointerName);
			};

			// create internalizer
			res->internalize = [arrayType](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
				return init(arrayType, node);
			};

			// create declaration = definition ... create struct
			c_ast::StructTypePtr arrayStructType = manager->create<c_ast::StructType>(structName);
			arrayStructType->elements.push_back(var(dataPointer, dataPointerName));

			// add size element - if requested
			if (supportSize) {
				c_ast::IdentifierPtr sizeName = manager->create("size");
				c_ast::TypePtr sizeType = manager->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UNSIGNED);
				c_ast::LiteralPtr dimensions = manager->create<c_ast::Literal>(boost::lexical_cast<string>(dim));
				sizeType = manager->create<c_ast::VectorType>(sizeType, dimensions);
				arrayStructType->elements.push_back(var(sizeType, sizeName));
			}

			c_ast::NodePtr definition = manager->create<c_ast::TypeDefinition>(arrayStructType, name);

			res->declaration = c_ast::CCodeFragment::createNew(manager, definition);
			res->definition = res->declaration;
			res->declaration->addDependency(elementTypeInfo->definition);


			// ---------------------- add constructor ---------------------

			// todo: convert this to pure C-AST code
			string ctrName = name->name + "_ctr";
			res->constructorName = manager->create(ctrName);

			std::stringstream code;
			code << "/* A constructor for the array type " << arrayName << " = "<< toString(*ptr) << "*/ \n"
					"static inline " << arrayName << " " << ctrName << "(";

			for (unsigned i=0; i<dim; i++) {
				code << "unsigned s" << (i+1);
				if (i!=dim-1) {
					code << ",";
				}
			}

			code << ") {\n";
			code << "    return ((" << arrayName << "){malloc(sizeof(" << c_ast::CPrint(elementTypeInfo->lValueType) << ")";
			for (unsigned i=0; i<dim; i++) {
				code << "*s" << (i+1);
			}
			code << ")";

			if (supportSize) {
				code << ",{";
				for (unsigned i=0; i<dim; i++) {
					code << "s" << (i+1);
					if (i!=dim-1) {
						code << ",";
					}
				}
				code << "}";
			}

			code << "});\n}\n";

			// attach the new operator
			c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
			res->constructor = c_ast::CCodeFragment::createNew(manager, cCode);
			res->constructor->addDependency(res->definition);

			return res;

		}

		VectorTypeInfo* TypeInfoStore::resolveVectorType(const core::VectorTypePtr& ptr) {
			auto manager = converter.getCNodeManager();

			// obtain dimension of array
			unsigned size = 0;
			const core::IntTypeParamPtr& sizePtr = ptr->getSize();
			if (sizePtr->getNodeType() != core::NT_ConcreteIntTypeParam) {
				// non-concrete array types are not supported
				return createUnsupportedInfo<VectorTypeInfo>(*manager);
			} else {
				size = static_pointer_cast<const core::ConcreteIntTypeParam>(sizePtr)->getValue();
			}

			// ----- create the struct representing the array type ------

			const TypeInfo* elementTypeInfo = resolveType(ptr->getElementType());
			assert(elementTypeInfo);

			// check whether the type has been resolved while resolving the sub-type
			auto pos = typeInfos.find(ptr);
			if (pos != typeInfos.end()) {
				assert(dynamic_cast<VectorTypeInfo*>(pos->second.getPointer()));
				return static_cast<VectorTypeInfo*>(pos->second.getPointer());
			}

			// compose resulting info instance
			VectorTypeInfo* res = new VectorTypeInfo();
			string vectorName = converter.getNameManager().getName(ptr);
			auto name = manager->create(vectorName);
			auto structName = manager->create("_" + vectorName);

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
				return access(node, dataElementName);
			};

			// create internalizer
			res->internalize = [vectorType](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
				return init(vectorType, node);
			};

			// create declaration = definition ... create struct
			c_ast::StructTypePtr vectorStructType = manager->create<c_ast::StructType>(structName);
			vectorStructType->elements.push_back(var(pureVectorType, dataElementName));

			c_ast::NodePtr definition = manager->create<c_ast::TypeDefinition>(vectorStructType, name);
			res->declaration = c_ast::CCodeFragment::createNew(manager, definition);
			res->definition = res->declaration;
			res->declaration->addDependency(elementTypeInfo->definition);


			// ---------------------- add init uniform operator ---------------------

			// todo: convert this to pure C-AST code
			string initUniformName = name->name + "_init_uniform";
			res->initUniformName = manager->create(initUniformName);

			// ---------------------- add init uniform ---------------------

			c_ast::VariablePtr valueParam = manager->create<c_ast::Variable>(elementTypeInfo->lValueType, manager->create("value"));

			std::stringstream code;
			code << "/* A constructor initializing a vector of type " << vectorName << " = "<< toString(*ptr) << " uniformly */ \n"
					"static inline " << vectorName << " " << initUniformName << "("
							<< c_ast::ParameterPrinter(valueParam) << ") {\n"
					"    " << vectorName << " res;\n"
					"    for(int i=0; i<" << size << ";++i) {\n"
					"        res.data[i] = value;\n"
					"    }\n"
					"    return res;\n"
					"}\n";

			// attach the init operator
			c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
			res->initUniform = c_ast::CCodeFragment::createNew(manager, cCode);
			res->initUniform->addDependency(res->definition);

			// done
			return res;
		}

		ChannelTypeInfo* TypeInfoStore::resolveChannelType(const core::ChannelTypePtr& ptr) {
			return createUnsupportedInfo<ChannelTypeInfo>(*converter.getCNodeManager());
		}

		RefTypeInfo* TypeInfoStore::resolveRefType(const core::RefTypePtr& ptr) {

			auto manager = converter.getCNodeManager();

			// create result
			RefTypeInfo* res = new RefTypeInfo();

			// obtain information covering sub-type
			auto elementNodeType = ptr->getElementType()->getNodeType();
			TypeInfo* subType = resolveType(ptr->getElementType());

			// check whether this ref type has been resolved while resolving the sub-type (due to recursion)
			auto pos = typeInfos.find(ptr);
			if (pos != typeInfos.end()) {
				assert(dynamic_cast<RefTypeInfo*>(pos->second.getPointer()));
				return static_cast<RefTypeInfo*>(pos->second.getPointer());
			}

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
				res->externalize = [res](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					return c_ast::cast(res->externalType, node);
				};
				res->internalize = [res](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					return c_ast::cast(res->rValueType, node);
				};
			}

			// special treatment for exporting arrays and vectors
			if (elementNodeType == core::NT_ArrayType || elementNodeType == core::NT_VectorType) {
				res->externalize = [res](const c_ast::SharedCNodeManager& manager, const c_ast::ExpressionPtr& node) {
					// generated code: ((externalName)X.data)
					return c_ast::access(c_ast::parenthese(c_ast::deref(node)), manager->create("data"));
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
					"static inline " << resultTypeName << " " << newOpName << "(" << valueTypeName << " value) {\n"
					"    " << resultTypeName << " res = malloc(sizeof(" << valueTypeName << "));\n"
					"    *res = value;\n"
					"    return res;\n"
					"}\n";

			// attach the new operator
			c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
			res->newOperator = c_ast::CCodeFragment::createNew(manager, cCode);

			// add dependencies
			res->newOperator->addDependency(subType->definition);

			// add include for malloc
			res->newOperator->addInclude(string("stdlib.h"));

			// done
			return res;
		}


		FunctionTypeInfo* TypeInfoStore::resolveFunctionType(const core::FunctionTypePtr& ptr) {

			auto manager = converter.getCNodeManager();

			// create new entry:

			// get name for function type
			string name = converter.getNameManager().getName(ptr, "funType");
			string functorName = name;
			string callerName = name + "_call";
			string ctrName = name + "_ctr";
			auto params = ptr->getParameterTypes();

			FunctionTypeInfo* res = new FunctionTypeInfo();

			// create the struct type defining the closure
			c_ast::StructTypePtr structType = manager->create<c_ast::StructType>(manager->create("_" + name));

			// construct the C AST function type token
			TypeInfo* retTypeInfo = resolveType(ptr->getReturnType());
			c_ast::FunctionTypePtr functionType = manager->create<c_ast::FunctionType>(retTypeInfo->rValueType);

			// add parameter capturing the closure
			c_ast::TypePtr structPointerType = manager->create<c_ast::PointerType>(structType);
			functionType->parameterTypes.push_back(structPointerType);

			// add remaining parameters
			vector<c_ast::CodeFragmentPtr> declDependencies;
			vector<c_ast::CodeFragmentPtr> defDependencies;
			for_each(ptr->getParameterTypes(), [&](const core::TypePtr& cur) {
				TypeInfo* info = resolveType(cur);
				functionType->parameterTypes.push_back(info->rValueType);
				declDependencies.push_back(info->declaration);
				defDependencies.push_back(info->definition);
			});

			// add result type dependencies
			declDependencies.push_back(resolveType(ptr->getReturnType())->declaration);
			defDependencies.push_back(resolveType(ptr->getReturnType())->definition);


			// construct the function type => struct including a function pointer
			c_ast::VariablePtr varCall = var(c_ast::ptr(functionType), "call");
			structType->elements.push_back(varCall);
			res->declaration = c_ast::CCodeFragment::createNew(manager, manager->create<c_ast::TypeDefinition>(structType, manager->create(name)));
			res->declaration->addDependencies(declDependencies);
			res->definition = res->declaration;

			// R / L value names
			c_ast::TypePtr namedType = manager->create<c_ast::NamedType>(manager->create(name));
			res->rValueType = c_ast::ptr(namedType);
			res->lValueType = res->rValueType;

			// external type handling
			res->externalType = res->rValueType;
			res->externalize = &NoOp;
			res->internalize = &NoOp;


			// ---------------- add caller ------------------------

			res->callerName = manager->create(callerName);

			// TODO: use C AST structures for this function

			// the argument variable
			string resultTypeName = toString(c_ast::CPrint(retTypeInfo->rValueType));

			std::stringstream code;
			code << "/* Type safe function for invoking closures of type " << name << " = "<< toString(*ptr) << " */ \n"
					"static inline " << resultTypeName << " " << callerName << "(" << name << "* closure";

			int i = 0;
			if (!params.empty()) {
				code << ", " << join(", ",
						functionType->parameterTypes.begin()+1,
						functionType->parameterTypes.end(),
						[&, this](std::ostream& out, const c_ast::TypePtr& cur) {
							out << c_ast::ParameterPrinter(cur, manager->create(format("p%d", ++i)));
				});
			}
			code << ") {\n    ";
			if (resultTypeName != "void") code << "return";
			code << " closure->call(closure";
			i = 0;
			if (!params.empty()) {
				code << ", " << join(", ",
						functionType->parameterTypes.begin()+1,
						functionType->parameterTypes.end(),
						[&, this](std::ostream& out, const c_ast::TypePtr&) {
							out << format("p%d", ++i);
				});
			}
			code << ");\n}\n";


			// attach the new operator
			c_ast::OpaqueCodePtr cCode = manager->create<c_ast::OpaqueCode>(code.str());
			res->caller = c_ast::CCodeFragment::createNew(manager, cCode);
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

			res->constructor = c_ast::CCodeFragment::createNew(manager, constructor);
			res->constructor->addDependency(res->definition);
			res->constructor->addDependencies(defDependencies);

			// done
			return res;
		}


		TypeInfo* TypeInfoStore::resolveRecType(const core::RecTypePtr& ptr) {

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

			// A) create a type info instance for each defined type and add definition
			for_each(ptr->getDefinitions(), [&](const std::pair<core::TypeVariablePtr, core::TypePtr>& cur) {

				// create recursive type represented by current type variable
				core::RecTypePtr type = core::RecType::get(nodeManager, cur.first, ptr);

				// create prototype
				c_ast::IdentifierPtr name = manager->create(nameManager.getName(type, "userdefined_rec_type"));

				// create declaration code
				c_ast::TypePtr cType;

				switch(cur.second->getNodeType()) {
				case core::NT_StructType:
					cType = manager->create<c_ast::StructType>(name); break;
				case core::NT_UnionType:
					cType = manager->create<c_ast::UnionType>(name); break;
				default:
					assert(false && "Cannot support recursive type which isn't a struct or union!");
				}

				TypeInfo* info = new TypeInfo();
				info->declaration = c_ast::CCodeFragment::createNew(manager, manager->create<c_ast::TypeDeclaration>(cType));
				info->lValueType = cType;
				info->rValueType = cType;
				info->externalType = cType;

				// register new type information
				typeInfos.insert(std::make_pair(type, info));
			});

			// B) unroll types and add definitions
			for_each(ptr->getDefinitions(), [&](const std::pair<core::TypeVariablePtr, core::TypePtr>& cur) {
				// obtain unrolled type
				core::TypePtr recType = core::RecType::get(nodeManager, cur.first, ptr);
				core::TypePtr unrolled = ptr->unrollOnce(nodeManager, cur.first);

				// fix name of unrolled struct
				nameManager.setName(unrolled, nameManager.getName(recType));

				TypeInfoPtr& entry = typeInfos.at(recType);
				TypeInfo* curInfo = entry.getPointer();
				TypeInfo* newInfo = resolveType(unrolled);

				assert(curInfo && newInfo && "Both should be available now!");
				assert(curInfo != newInfo);

				// combine them and updated within type info map (not being owned by the pointer)
				newInfo->declaration = curInfo->declaration;
				entry = TypeInfoPtr(newInfo, false);

				// remove old information
				delete curInfo;
			});

		}

	}

} // end namespace backend
} // end namespace insieme
