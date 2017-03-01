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

#include <functional>

#include <boost/noncopyable.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_types.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {

	class UsedMemberTag {};

	class Converter;

	struct TypeInfo;
	struct FunctionTypeInfo;
	struct TagTypeInfo;
	struct RefTypeInfo;
	struct ArrayTypeInfo;
	struct VectorTypeInfo;
	struct ChannelTypeInfo;

	typedef TypeInfo* TypeInfoPtr;

	typedef std::map<string, string> TypeIncludeTable;

	TypeIncludeTable getBasicTypeIncludeTable();

	typedef std::function<const TypeInfo*(ConversionContext&, const core::TypePtr&)> TypeHandler;

	typedef vector<TypeHandler> TypeHandlerList;

	static inline TypeHandlerList getBasicTypeHandlerList() {
		return TypeHandlerList();
	}

	namespace detail {
		class TypeInfoStore;
	}

	class TypeManager : private boost::noncopyable {
		const Converter& converter;
		detail::TypeInfoStore* store;

	  public:
		TypeManager(const Converter& converter);

		TypeManager(const Converter& converter, const TypeIncludeTable& includeTable, const TypeHandlerList& handlers);

		~TypeManager();

		const TypeInfo& getTypeInfo(ConversionContext& context, const core::TypePtr&);

		const TagTypeInfo& getTypeInfo(ConversionContext& context, const core::TagTypePtr&);

		const TagTypeInfo& getTypeInfo(ConversionContext& context, const core::TupleTypePtr&);

		const FunctionTypeInfo& getTypeInfo(ConversionContext& context, const core::FunctionTypePtr&);

		const RefTypeInfo& getRefTypeInfo(ConversionContext& context, const core::GenericTypePtr&);

		const ArrayTypeInfo& getArrayTypeInfo(ConversionContext& context, const core::GenericTypePtr& type);

		const ChannelTypeInfo& getChannelTypeInfo(ConversionContext& context, const core::GenericTypePtr& type);

		const TypeInfo& getCVectorTypeInfo(ConversionContext& context, const core::TypePtr& elementType, const c_ast::ExpressionPtr& size);

		// this one is only working for already resolved types
		const c_ast::CodeFragmentPtr getDefinitionOf(const c_ast::TypePtr& type);

		// get type for use in template argument list
		const c_ast::TypePtr getTemplateArgumentType(ConversionContext& context, const core::TypePtr& type);

		// ----------------------- Management -----------------------

		TypeIncludeTable& getTypeIncludeTable();

		void addTypeHandler(const TypeHandler& handler);

		void addTypeHandler(const TypeHandlerList& list);
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

		TypeInfo();

		virtual ~TypeInfo(){};
	};

	struct TagTypeInfo : public TypeInfo {
		// to be included:
		//		- a constructor function

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

	struct ChannelTypeInfo : public TypeInfo {
		// to be included
		//		- read and write operations?
	};


	namespace type_info_utils {

		c_ast::ExpressionPtr NoOp(const c_ast::SharedCNodeManager&, const c_ast::ExpressionPtr& node);

		template <typename T = TypeInfo>
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

		template <typename T = TypeInfo>
		T* createInfo(c_ast::CNodeManager& nodeManager, c_ast::PrimitiveType::CType primitiveType) {
			c_ast::IdentifierPtr ident = nodeManager.create("primitive-type");
			c_ast::TypePtr type = nodeManager.create<c_ast::PrimitiveType>(primitiveType);
			return createInfo<T>(type);
		}

		template <typename T = TypeInfo>
		T* createInfo(c_ast::CNodeManager& nodeManager, const string& name) {
			c_ast::IdentifierPtr ident = nodeManager.create(name);
			c_ast::TypePtr type = nodeManager.create<c_ast::NamedType>(ident);
			return createInfo<T>(type);
		}

		template <typename T = TypeInfo>
		T* createInfo(const c_ast::SharedCodeFragmentManager& fragmentManager, const string& name, const string& includeFile) {
			const c_ast::SharedCNodeManager& nodeManager = fragmentManager->getNodeManager();
			c_ast::IdentifierPtr ident = nodeManager->create(name);
			c_ast::TypePtr type = nodeManager->create<c_ast::NamedType>(ident);
			T* res = createInfo<T>(type);

			c_ast::CodeFragmentPtr decl = c_ast::IncludeFragment::createNew(fragmentManager, includeFile);
			res->declaration = decl;
			res->definition = decl;
			return res;
		}


		template <typename T = TypeInfo>
		T* createUnsupportedInfo(c_ast::CNodeManager& nodeManager, const core::TypePtr& type) {
			return createInfo<T>(nodeManager, "/* UNSUPPORTED TYPE: " + toString(*type) + " */");
		}

		template <typename T = TypeInfo>
		T* createInfo(const c_ast::TypePtr& type, const c_ast::CodeFragmentPtr& definition) {
			T* res = createInfo<T>(type);
			res->declaration = definition;
			res->definition = definition;
			return res;
		}

		template <typename T = TypeInfo>
		T* createInfo(const c_ast::TypePtr& type, const c_ast::CodeFragmentPtr& declaration, const c_ast::CodeFragmentPtr& definition) {
			// declaration => definition
			assert(!declaration || definition);

			T* res = createInfo<T>(type);
			res->declaration = declaration;
			res->definition = definition;
			res->declaration->addRequirement(definition);
			return res;
		}

		const TypeInfo* headerAnnotatedTypeHandler(const Converter& converter, const core::TypePtr& type,
			std::function<void(std::string&, const core::TypePtr&)> nameModifier = [](std::string& s, const core::TypePtr& p) {});
	}

} // end namespace backend
} // end namespace insieme
