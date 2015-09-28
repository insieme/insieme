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

#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/ir++_extension.h"

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
				return funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction();
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


	// --------------------------- data member pointer -----------------------------------

	bool isMemberPointer(const TypePtr& type) {
		// filter out null-pointer
		if(!type) { return false; }

		// must be a struct type
		TagTypePtr tagType = type.isa<TagTypePtr>();
		if(!tagType || !tagType.isStruct()) { return false; }

		// has 3 elements
		auto fields = tagType->getFields();
		if(fields.size() != 3u) { return false; }

		FieldPtr element = fields[0];
		if(!isTypeLiteralType(element->getType())) { return false; }
		if(element->getName().getValue() != "objType") { return false; }

		element = fields[1];
		// if ( !isId(element->getType()))	return false; // TODO: ask if is an identifier
		if(element->getName().getValue() != "id") { return false; }

		element = fields[2];
		if(!isTypeLiteralType(element->getType())) { return false; }
		if(element->getName().getValue() != "membType") { return false; }

		return true;
	}

	TypePtr getMemberPointer(const TypePtr& classType, const TypePtr& membTy) {
		NodeManager& manager = classType.getNodeManager();
		IRBuilder builder(manager);
		return builder.structType(toVector(builder.field(builder.stringValue("objType"), builder.getTypeLiteralType(classType)),
		                                   builder.field(builder.stringValue("id"), builder.getLangBasic().getIdentifier()),
		                                   builder.field(builder.stringValue("membType"), builder.getTypeLiteralType(membTy))));
	}

	ExpressionPtr getMemberPointerValue(const TypePtr& classType, const std::string& fieldName, const TypePtr& membType) {
		NodeManager& manager = classType.getNodeManager();
		IRBuilder builder(manager);

		// retrieve the name and the field type to build the desired member pointer struct
		core::ExpressionPtr access = manager.getLangExtension<lang::IRppExtensions>().getMemberPointerCtor();
		return builder.callExpr(access, toVector<core::ExpressionPtr>(builder.getTypeLiteral(classType), builder.getIdentifierLiteral(fieldName),
		                                                              builder.getTypeLiteral(membType)));
	}

	ExpressionPtr getMemberPointerAccess(const ExpressionPtr& base, const ExpressionPtr& expr) {
		NodeManager& manager = base.getNodeManager();
		IRBuilder builder(manager);

		// retrieve the name and the field type to build the desired access
		core::ExpressionPtr access = manager.getLangExtension<lang::IRppExtensions>().getMemberPointerAccess();
		return builder.callExpr(access, toVector(base, expr));
	}

	ExpressionPtr getMemberPointerCheck(const ExpressionPtr& expr) {
		NodeManager& manager = expr.getNodeManager();
		IRBuilder builder(manager);

		// retrieve the name and the field type to build the desired access
		core::ExpressionPtr access = manager.getLangExtension<lang::IRppExtensions>().getMemberPointerCheck();
		return builder.callExpr(access, expr);
	}


	// --------------------------- C++ calls ---------------------------------------------

	bool isConstructorCall(const core::ExpressionPtr& expr) {
		core::CallExprPtr call = expr.isa<core::CallExprPtr>();
		return call && call->getFunctionExpr()->getType().as<FunctionTypePtr>()->isConstructor();
	}


	LambdaExprPtr createDefaultConstructor(const TypePtr& type) {
		assert_true(isObjectType(type)) << "to create DefaultCtor a objectType is needed";
		NodeManager& manager = type.getNodeManager();
		IRBuilder builder(manager);

		// the type of the reference to be initialized by constructor (this type)
		TypePtr refType = builder.refType(type);

		// create the type of the resulting function
		FunctionTypePtr ctorType = builder.functionType(toVector(refType), refType, FK_CONSTRUCTOR);

		// build the constructor
		VariablePtr thisVar = builder.variable(builder.refType(refType), 1);
		return builder.lambdaExpr(ctorType, toVector(thisVar), builder.compoundStmt());
	}


	bool isDefaultConstructor(const LambdaExprPtr& lambda) {
		// it has to be a lambda
		if(!lambda) { return false; }

		// it has to be a constructor
		if(lambda->getFunctionType()->getKind() != FK_CONSTRUCTOR) { return false; }

		// it has to have a single parameter
		if(lambda->getParameterList()->size() != 1u) { return false; }

		// TODO: check the actual initialization of the members and parent types
		// if the body is empty, be fine
		return lambda->getBody() == IRBuilder(lambda->getNodeManager()).compoundStmt();
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme
