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
#include "insieme/core/types.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {

	class Converter;

	class TypeInfo;
	class FunctionTypeInfo;
	class RefTypeInfo;
	class ArrayTypeInfo;
	class VectorTypeInfo;
	class ChannelTypeInfo;

	namespace detail {
		class TypeInfoStore;
	}


	class TypeManager {

		detail::TypeInfoStore* store;

	public:

		TypeManager(const Converter& converter);

		virtual ~TypeManager();

		virtual const TypeInfo& getTypeInfo(const core::TypePtr&);

		virtual const FunctionTypeInfo& getTypeInfo(const core::FunctionTypePtr&);

		virtual const RefTypeInfo& getTypeInfo(const core::RefTypePtr&);

		virtual const ArrayTypeInfo& getTypeInfo(const core::ArrayTypePtr& type);

		virtual const VectorTypeInfo& getTypeInfo(const core::VectorTypePtr& type);

		virtual const ChannelTypeInfo& getTypeInfo(const core::ChannelTypePtr& type);
	};


	/**
	 * A type definition for a function converting one C AST node pointer into another
	 * node pointer. Elements of this type are used to realize variable and parameter
	 * declarations and conversions between plain C and Inspire types.
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

	struct FunctionTypeInfo : public TypeInfo {

		// to be included
		//		- closure name
		//		- caller name
		//		- references to code fragments of utilities

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

	struct VectorTypeInfo : public TypeInfo {
		// to be included
		//		- init uniform operator

		c_ast::IdentifierPtr initUniformName;

		c_ast::CodeFragmentPtr initUniform;

	};

	struct ChannelTypeInfo : public TypeInfo {
		// to be included
		//		- read and write operations?
	};


} // end namespace backend
} // end namespace insieme
