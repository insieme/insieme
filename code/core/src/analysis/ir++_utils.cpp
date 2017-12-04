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

#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/datapath/datapath.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/channel.h"

#include "insieme/utils/assert.h"

namespace insieme {
namespace core {
namespace analysis {

	bool isIRpp(const NodePtr& node) {
		return visitDepthFirstOnceInterruptible(node, [](const NodePtr& cur) -> bool {

			// check whether there are structs using inheritance
			if(StructPtr structType = cur.isa<StructPtr>()) {
				// if not empty => it is a IR++ code
				if(!structType->getParents().empty()) { return true; }
			}

			// check function types
			if(FunctionTypePtr funType = cur.isa<FunctionTypePtr>()) {
				return funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction() || funType->isVirtualMemberFunction();
			}

			// if there are exceptions, it is IR++
			if(cur->getNodeType() == NT_ThrowStmt || cur->getNodeType() == NT_TryCatchStmt) { return true; }

			// no IR++ content found
			return false;
		}, true);
	}

	bool isObjectType(const TypePtr& type) {

		// exclude predefined types
		if (lang::isArray(type)) return false;
		if (lang::isReference(type)) return false;
		if (lang::isPointer(type)) return false;
		if (lang::isChannel(type)) return false;

		// decide whether something is an object type based on the node type
		switch(type->getNodeType()) {
		case NT_GenericType:
			// no built-in type is an object type
			return !type->getNodeManager().getLangBasic().isPrimitive(type);
		case NT_TagType:
		case NT_TagTypeReference:
		case NT_TypeVariable:  return true; // all this types are always object types
		default: break;
		}

		// everything else is not an object type
		return false;
	}

	bool isObjectReferenceType(const TypePtr& type) {
		return isRefType(type) && isObjectType(getReferencedType(type));
	}

	bool isObjectReferenceType(const GenericTypePtr& type) {
		return isRefType(type) && isObjectType(getReferencedType(type));
	}

	// --------------------------- C++ calls ---------------------------------------------

	bool isConstructorCall(const core::ExpressionPtr& expr) {
		core::CallExprPtr call = expr.isa<core::CallExprPtr>();
		if (!call) return false;
		auto funType = call->getFunctionExpr()->getType().isa<FunctionTypePtr>();
		return funType && funType->isConstructor();
	}

	namespace {

		bool isCopyOrMoveConstructor(const NodePtr& node, bool constParam, lang::ReferenceType::Kind kind) {

			// extract the type
			auto type = getType(node).isa<FunctionTypePtr>();

			// if it is not a function type at all => it is not a ctor
			if (!type) return false;

			// test that the function is a constructor
			if (!type->isConstructor()) return false;

			// check number of parameters
			if(type->getParameterTypeList().size() != 2) return false;

			// get the this type
			auto thisType = getObjectType(type);

			// the second parameter must be a reference
			if(!core::lang::isReference(type->getParameterType(1))) return false;

			// and the type referenced by the second argument must be equivalent to the this type
			auto refType = core::lang::ReferenceType(type->getParameterType(1));
			if(refType.isConst() != constParam || refType.isVolatile() || *refType.getElementType() != *thisType) return false;

			// finally we check the reference kind to determine whether it is a copy or move constructor
			return refType.getKind() == kind;
		}

	}

	bool isCopyConstructor(const NodePtr& node) {
		return isCopyOrMoveConstructor(node,true,lang::ReferenceType::Kind::CppReference);
	}

	bool isMoveConstructor(const NodePtr& node) {
		return isCopyOrMoveConstructor(node,false,lang::ReferenceType::Kind::CppRValueReference);
	}


	namespace {

		bool isOfCopyOrMoveAssignmentType(const NodePtr& node, bool constParam, lang::ReferenceType::Kind kind) {

			// extract the type
			auto type = getType(node).isa<FunctionTypePtr>();

			// if it is not a function type at all => it is not a ctor
			if (!type) return false;

			// test that the function is a constructor
			if (!type->isMemberFunction()) return false;

			// check number of parameters
			if(type->getParameterTypeList().size() != 2) return false;

			// get the this type
			auto thisType = getObjectType(type);

			// the result type must be a reference to the this type
			if (!core::lang::isReference(type->getReturnType())) return false;

			auto retType = core::lang::ReferenceType(type->getReturnType());
			if (retType.isConst() || retType.isVolatile() || !retType.isCppReference()) return false;

			// the second parameter must be a reference
			if(!core::lang::isReference(type->getParameterType(1))) return false;

			// and the type referenced by the second argument must be equivalent to the this type
			auto refType = core::lang::ReferenceType(type->getParameterType(1));
			if(refType.isConst() != constParam || refType.isVolatile() || *refType.getElementType() != *thisType) return false;

			// finally we check the reference kind to determine whether it is a copy or move constructor
			return refType.getKind() == kind;
		}

	}

	bool isOfCopyAssignmentType(const NodePtr& node) {
		return isOfCopyOrMoveAssignmentType(node, true, lang::ReferenceType::Kind::CppReference);
	}

	bool isOfMoveAssignmentType(const NodePtr& node) {
		return isOfCopyOrMoveAssignmentType(node, false, lang::ReferenceType::Kind::CppRValueReference);
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme
