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

#include "insieme/core/encoder/lists.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace core {
namespace encoder {

	namespace {

		/**
		 * A helper function constructing a generic list type instance.
		 */
		core::TypePtr getListType(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			core::TypePtr alpha = builder.typeVariable("a");
			auto ret = GenericType::get(manager, ListExtension::LIST_TYPE_NAME, toVector(alpha));
			lang::markAsBuiltIn(ret);
			return ret;
		}

		/**
		 * A helper function constructing the empty literal used for encoding lists.
		 */
		core::LiteralPtr getEmptyLiteral(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			core::TypePtr list = getListType(manager);
			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr typeArgType = builder.genericType("type", toVector(alpha));
			core::TypePtr emptyType = builder.functionType(typeArgType, list);
			auto ret = builder.literal(emptyType, "empty");
			lang::markAsBuiltIn(ret);
			return ret;
		}

		/**
		 * A helper function constructing the cons literal used for encoding lists.
		 */
		core::LiteralPtr getConsLiteral(core::NodeManager& manager) {
			core::IRBuilder builder(manager);
			core::TypePtr list = getListType(manager);
			core::TypePtr alpha = builder.typeVariable("a");
			core::TypePtr consType = builder.functionType(toVector(alpha, list), list);
			auto ret = builder.literal(consType, "cons");
			lang::markAsBuiltIn(ret);
			return ret;
		}

	}

	string ListExtension::LIST_TYPE_NAME = "list";

	ListExtension::ListExtension(core::NodeManager& manager)
		: Extension(manager), empty(getEmptyLiteral(manager)), cons(getConsLiteral(manager)) {}


	bool isListType(const core::TypePtr& type) {
		if (type->getNodeType() != NT_GenericType) {
			return false;
		}

		// check generic type properties
		core::GenericTypePtr genType = static_pointer_cast<const GenericType>(type);
		return genType->getName()->getValue() == ListExtension::LIST_TYPE_NAME &&
				genType->getTypeParameter().size() == static_cast<std::size_t>(1) &&
				genType->getIntTypeParameter().empty();
	}

	const core::TypePtr getElementType(const core::TypePtr& listType) {
		assert_true(isListType(listType)) << "Not applicable to non-list type!";
		return static_pointer_cast<const core::GenericType>(listType)->getTypeParameter()->getElement(0);
	}

	const core::TypePtr getListType(const core::TypePtr& elementType) {
		IRBuilder builder(elementType->getNodeManager());
		return builder.genericType(ListExtension::LIST_TYPE_NAME, toVector(elementType));
	}


} // end namespace lists
} // end namespace core
} // end namespace insieme
