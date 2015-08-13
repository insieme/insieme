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

#include "insieme/core/transform/utils/member_access_literal_updater.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {
namespace utils {

#define BASIC builder.getNodeManager().getLangBasic()

/**
 * Visitor which checks if the type literal argument of composite and tuple calls are aligned with the actual type of the struct/tuple.
 * If not the type literal is replaced with the appropriate one
 */
const NodePtr MemberAccessLiteralUpdater::resolveElement(const NodePtr& ptr) {
	// if we reach a type stop recursion
	if(ptr->getNodeCategory() == NC_Type || ptr->getNodeCategory() == NC_IntTypeParam) {
		return ptr;
	}
	
	// recursive replacement has to be continued
	NodePtr res = ptr->substitute(builder.getNodeManager(), *this);
	
	auto& refExt = builder.getNodeManager().getLangExtension<lang::ReferenceExtension>();

	if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(res)) {
		ExpressionPtr fun = call->getFunctionExpr();
		// struct access
		if(BASIC.isCompositeMemberAccess(fun)) {
		
			const StructTypePtr& structTy = static_pointer_cast<const StructType>(call->getArgument(0)->getType());
			// TODO find better way to extract Identifier from IdentifierLiteral
			const StringValuePtr& id = static_pointer_cast<const Literal>(call->getArgument(1))->getValue();
			const TypePtr& type = structTy->getTypeOfMember(id);
			if(call->getArgument(2)->getType() != type || call->getType() != type) {
				res = builder.callExpr(type, fun, call->getArgument(0), call->getArgument(1), builder.getTypeLiteral(type));
			}
		}
		
		if(refExt.isRefMemberAccess(fun)) {
		
			const StructTypePtr& structTy = static_pointer_cast<const StructType>(
			                                    analysis::getReferencedType(call->getArgument(0)->getType()));
			// TODO find better way to extract Identifier from IdentifierLiteral
			const StringValuePtr& id = static_pointer_cast<const Literal>(call->getArgument(1))->getValue();
			const TypePtr& elemType = structTy->getTypeOfMember(id);
			const GenericTypePtr& refTy = builder.refType(elemType);
			if(call->getArgument(2)->getType() != elemType || call->getType() != refTy)
				res = builder.callExpr(refTy, fun, call->getArgument(0), call->getArgument(1),
				                       builder.getTypeLiteral(elemType));
		}
		
		if(refExt.isRefArrayElement(fun)) {
			bool isRef = analysis::isRefType(call->getArgument(0)->getType());
			const GenericTypePtr& seTy = (isRef ? analysis::getReferencedType(call->getArgument(0)->getType()) : call->getArgument(0)->getType()).as<GenericTypePtr>();
			const TypePtr& type = isRef ? builder.refType(seTy->getTypeParameter(0)) : seTy->getTypeParameter(0);
			
			if(call->getType() != type) {
				res = builder.callExpr(type, fun, call->getArguments());
			}
		}
		
		if(refExt.isRefComponentAccess(fun) || BASIC.isTupleMemberAccess(fun)) {
		
			ExpressionPtr arg = call->getArgument(1);
			int idx = -1;
			
			// search for the literal in the second argument
			auto lambdaVisitor = makeLambdaVisitor([&idx, this](const NodePtr& node)->bool {
				// check for literal, assuming it will always be a valid integer
				if(const LiteralPtr& lit = dynamic_pointer_cast<const Literal>(node)) {
					if(BASIC.isInt(lit->getType())) {
						idx = atoi(lit->getValue()->getValue().c_str());
						return true;
					}
				}
				return false;
			});
			
			if(!visitDepthFirstInterruptible(arg, lambdaVisitor) || idx == -1) {
				LOG(ERROR) << fun;
				assert_fail() << "Tuple access does not contain a literal as index";
			}
			
			bool isRef = analysis::isRefType(call->getArgument(0)->getType());
			const TupleTypePtr tupleTy = dynamic_pointer_cast<const TupleType>(isRef ? analysis::getReferencedType(call->getArgument(0)->getType()) : call->getArgument(0)->getType());
			if(!tupleTy) { //TODO remove dirty workaround
				return res;
			}
			assert_true(tupleTy) << "Tuple acces on a non tuple variable called";
			const TypePtr& elemTy = tupleTy->getElement(idx);
			
			
			const TypePtr& retTy = isRef ? builder.refType(elemTy) : elemTy;
			const LiteralPtr& elemTyLit = builder.getTypeLiteral(elemTy);
			
			if(*call->getType() != *retTy || *call->getArgument(2)->getType() != *elemTyLit->getType()) {
				res = builder.callExpr(retTy, isRef ? refExt.getRefComponentAccess() : BASIC.getTupleMemberAccess(), call->getArgument(0), call->getArgument(1),
				                       elemTyLit);
			}
		}
	}
	
	
	// check whether something has changed ...
	if(res == ptr) {
		// => nothing changed
		return ptr;
	}
	
	// preserve annotations
	utils::migrateAnnotations(ptr, res);
	
	// done
	return res;
}

} // end namespace utils
} // end namespace transform
} // end namespace core
} // end namespace insieme
