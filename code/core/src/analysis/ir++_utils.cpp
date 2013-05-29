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

#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/ir++_extension.h"

namespace insieme {
namespace core {
namespace analysis {

	bool isIRpp(const NodePtr& node) {
		return visitDepthFirstOnceInterruptible(node, [](const NodePtr& cur)->bool {

			// check whether there are structs using inheritance
			if (StructTypePtr structType = cur.isa<StructTypePtr>()) {

				// if not empty => it is a IR++ code
				if (!structType->getParents().empty()) {
					return true;
				}

				// if there is meta-info => it is a IR++ code
				if (hasMetaInfo(structType)) {
					return true;
				}
			}

			// check function types
			if (FunctionTypePtr funType = cur.isa<FunctionTypePtr>()) {
				return funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction();
			}

			// if there are exceptions, it is IR++
			if (cur->getNodeType() == NT_ThrowStmt || cur->getNodeType() == NT_TryCatchStmt) {
				return true;
			}

			// no IR++ content found
			return false;
		}, true);
	}

	bool isObjectType(const TypePtr& type) {


		// decide whether something is an object type based on the node type
		switch(type->getNodeType()) {
		case NT_GenericType:
			// no built-in type is an object type
			return !type->getNodeManager().getLangBasic().isPrimitive(type);
		case NT_StructType:
		case NT_TypeVariable:
			return true;			// all this types are always object types
		case NT_RecType:
			return isObjectType(type.as<RecTypePtr>()->unroll());
		default: break;
		}

		// everything else is not an object type
		return false;
	}

	bool isObjectReferenceType(const TypePtr& type) {
		return type->getNodeType() == NT_RefType && isObjectReferenceType(type.as<RefTypePtr>());
	}

	bool isObjectReferenceType(const RefTypePtr& type) {
		return isObjectType(type->getElementType());
	}

	bool isPureVirtual(const CallExprPtr& call) {
		// check for null
		if (!call) return false;

		// check whether it is a call to the pure-virtual literal
		const auto& ext = call->getNodeManager().getLangExtension<lang::IRppExtensions>();
		return isCallOf(call, ext.getPureVirtual());
	}

	bool isPureVirtual(const NodePtr& node) {
		return isPureVirtual(node.isa<CallExprPtr>());
	}


	// ---------------------------- References --------------------------------------

	namespace {

		bool isRef(const TypePtr& type, const string& memberName) {

			// filter out null-pointer
			if (!type) return false;

			// must be a struct type
			StructTypePtr structType = type.isa<StructTypePtr>();
			if (!structType) return false;

			// only one member
			if (structType.size() != 1u) return false;

			// check the one member element
			NamedTypePtr element = structType[0];
			return element->getType()->getNodeType() == NT_RefType
					&& element->getType().as<RefTypePtr>()->getElementType()->getNodeType() != NT_RefType
					&& !isCppRef(element->getType()) && !isConstCppRef(element->getType())
					&& element->getName().getValue() == memberName;
		}

		TypePtr getRef(const TypePtr& elementType, const string& memberName) {
			IRBuilder builder(elementType->getNodeManager());
			return builder.structType(toVector(
					builder.namedType(memberName, builder.refType(elementType))
			));
		}

		const string CppRefStringMember = "_cpp_ref";
		const string CppConstRefStringMember = "_const_cpp_ref";
	}

	bool isCppRef(const TypePtr& type) {
		return isRef(type, CppRefStringMember);
	}

	TypePtr getCppRef(const TypePtr& elementType) {
		return getRef(elementType, CppRefStringMember);
	}

	bool isConstCppRef(const TypePtr& type) {
		return isRef(type, CppConstRefStringMember);
	}

	TypePtr getConstCppRef(const TypePtr& elementType) {
		return getRef(elementType, CppConstRefStringMember);
	}

	TypePtr getCppRefElementType(const TypePtr& cppRefType) {
		assert(isCppRef(cppRefType) || isConstCppRef(cppRefType));
		return cppRefType.as<StructTypePtr>()[0]->getType().as<RefTypePtr>()->getElementType();
	}

	// --------------------------- C++ calls ---------------------------------------------

	bool isConstructorCall(const core::ExpressionPtr& expr){
		if (core::CallExprPtr call = expr.isa<core::CallExprPtr>()){
			if(core::LiteralPtr lit = call->getFunctionExpr().isa<core::LiteralPtr>()){
				if (lit->getType().as<core::FunctionTypePtr>()->isConstructor())
					return true;
			}
			else if(core::LambdaExprPtr func = call->getFunctionExpr().isa<core::LambdaExprPtr>()){
				// is it a constructor call?
				if (func->getFunctionType()->isConstructor())
					return true;
			}
		}
		return false;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
