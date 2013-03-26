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

#pragma once

#include <functional>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_types.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {

	class Converter;

	class TypeInfo;
	class FunctionTypeInfo;
	class StructTypeInfo;
	class UnionTypeInfo;
	class RefTypeInfo;
	class ArrayTypeInfo;
	class VectorTypeInfo;
	class ChannelTypeInfo;

	typedef TypeInfo* TypeInfoPtr;
	typedef std::map<string, string> TypeIncludeTable;

	TypeIncludeTable getBasicTypeIncludeTable();


	typedef std::function<const TypeInfo*(const Converter&, const core::TypePtr&)> TypeHandler;

	typedef vector<TypeHandler> TypeHandlerList;

	static inline TypeHandlerList getBasicTypeHandlerList() { return TypeHandlerList(); }

	namespace detail {
		class TypeInfoStore;
	}

	class TypeManager {

		detail::TypeInfoStore* store;

	public:

		TypeManager(const Converter& converter);

		TypeManager(const Converter& converter, const TypeIncludeTable& includeTable, const TypeHandlerList& handlers);

		virtual ~TypeManager();

		virtual const TypeInfo& getTypeInfo(const core::TypePtr&);

		virtual const StructTypeInfo& getTypeInfo(const core::StructTypePtr&);

		virtual const StructTypeInfo& getTypeInfo(const core::TupleTypePtr&);

		virtual const UnionTypeInfo& getTypeInfo(const core::UnionTypePtr&);

		virtual const FunctionTypeInfo& getTypeInfo(const core::FunctionTypePtr&);

		virtual const RefTypeInfo& getTypeInfo(const core::RefTypePtr&);

		virtual const ArrayTypeInfo& getTypeInfo(const core::ArrayTypePtr& type);

		virtual const VectorTypeInfo& getTypeInfo(const core::VectorTypePtr& type);

		virtual const ChannelTypeInfo& getTypeInfo(const core::ChannelTypePtr& type);

		virtual const TypeInfo& getCVectorTypeInfo(const core::TypePtr& elementType, const c_ast::ExpressionPtr& size);

		// this one is only working for already resolved types
		virtual const c_ast::CodeFragmentPtr getDefinitionOf(const c_ast::TypePtr& type);
	};


	/**
	 * A type definition for a function converting one C AST node pointer into another
	 * node pointer. Elements of this type are used to realize variable and parameter
	 * declarations and conversions between plain C and Inspire types.
	 *
	 *  - The shared C-node manager should be used for generating new C-AST nodes.
	 *  - the C-AST expression is the converted value representation
	 */
	typedef std::function<c_ast::ExpressionPtr(const c_ast::SharedCNodeManager&, const c_ast::ExpressionPtr&)> NodeConverter;

	struct TypeInfo {

		// to be included:
		// 		- l / r value name
		//		- external name
		//		- declaration pattern
		//		- parameter pattern
		//		- externalization
		//		- internalization

		c_ast::TypePtr lValueType;

		c_ast::TypePtr rValueType;

		c_ast::TypePtr externalType; // always rValue

		NodeConverter externalize;

		NodeConverter internalize;

		c_ast::CodeFragmentPtr declaration;

		c_ast::CodeFragmentPtr definition;

		virtual ~TypeInfo() {};
	};

	struct StructTypeInfo : public TypeInfo {

		// to be included:
		//		- a constructor function

		c_ast::CodeFragmentPtr constructor;

	};

	struct UnionTypeInfo : public TypeInfo {

		// to be included:
		//		- a constructor per member

		std::vector<c_ast::CodeFragmentPtr> constructors;

	};

	struct FunctionTypeInfo : public TypeInfo {

		// to be included
		//		- plain flag
		//		- closure name
		//		- caller name
		//		- references to code fragments of utilities

		bool plain;

		c_ast::IdentifierPtr callerName;

		c_ast::CodeFragmentPtr caller;

		c_ast::IdentifierPtr constructorName;

		c_ast::CodeFragmentPtr constructor;

	};

	struct RefTypeInfo : public TypeInfo {

		// to be included
		//		- new operator

		c_ast::IdentifierPtr newOperatorName;

		c_ast::CodeFragmentPtr newOperator;

	};

	struct ArrayTypeInfo : public TypeInfo {
		// to be included
		//		- nothing extra so far
	};

	struct VectorTypeInfo : public StructTypeInfo {
		// to be included
		//		- init uniform operator

		c_ast::IdentifierPtr initUniformName;

		c_ast::CodeFragmentPtr initUniform;

	};

	struct ChannelTypeInfo : public TypeInfo {
		// to be included
		//		- read and write operations?
	};


	namespace type_info_utils {

		c_ast::ExpressionPtr NoOp(const c_ast::SharedCNodeManager&, const c_ast::ExpressionPtr& node);

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
		T* createInfo(const c_ast::SharedCodeFragmentManager& fragmentManager, const string& name, const string& includeFile) {
			const c_ast::SharedCNodeManager& nodeManager = fragmentManager->getNodeManager();
			c_ast::IdentifierPtr ident = nodeManager->create(name);
			c_ast::TypePtr type = nodeManager->create<c_ast::NamedType>(ident);
			T* res = createInfo<T>(type);

			c_ast::CodeFragmentPtr decl = c_ast::DummyFragment::createNew(fragmentManager);
			decl->addInclude(includeFile);
			res->declaration = decl;
			res->definition = decl;
			return res;
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

	}

} // end namespace backend
} // end namespace insieme
