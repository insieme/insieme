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
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

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

	// ---------------------------- Defaulted Members --------------------------------------

	namespace {
		struct DefaultedTag {};

		LiteralPtr getDefaultedMarker(const IRBuilder& builder) {
			auto defaulted = builder.stringLit("INSIEME_DEFAULTED");
			defaulted.attachValue<DefaultedTag>();
			return defaulted;
		}
	}

	LambdaExprPtr markAsDefaultMember(const LambdaExprPtr& lambda) {
		IRBuilder builder(lambda->getNodeManager());
		StatementList newBody{ getDefaultedMarker(builder) };
		std::copy(lambda->getBody()->begin(), lambda->getBody()->end(), std::back_inserter(newBody));
		return builder.lambdaExpr(lambda->getType(), lambda->getParameterList(), builder.compoundStmt(newBody), lambda->getReference()->getNameAsString());
	}

	bool isaDefaultMember(const NodePtr& node) {
		auto n = node;
		if(auto mem = n.isa<MemberFunctionPtr>()) {
			n = mem->getImplementation();
		}
		if (auto lambda = n.isa<LambdaExprPtr>()) {
			if (!lambda->getFunctionType()->isMember()) return false;
			auto body = lambda->getBody();
			if (!body || body->size() < 1) return false;
			auto first = body[0];
			return first == getDefaultedMarker(IRBuilder(lambda->getNodeManager())) && first.hasAttachedValue<DefaultedTag>();
		} else if (auto lambda = n.isa<LambdaPtr>()) {
			if (!lambda->getType()->isMember()) return false;
			auto body = lambda->getBody();
			if (!body || body->size() < 1) return false;
			auto first = body[0];
			return first == getDefaultedMarker(IRBuilder(lambda->getNodeManager())) && first.hasAttachedValue<DefaultedTag>();
		}
		return false;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
