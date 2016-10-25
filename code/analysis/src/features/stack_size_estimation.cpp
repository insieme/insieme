/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/features/stack_size_estimation.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/varargs_extension.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace {
		const unsigned STACK_SIZE_RECURSION_ESTIMATE = 1;
		const unsigned STACK_SIZE_VARIABLE_ARRAY_ESTIMATE = 100;
		const unsigned STACK_SIZE_ALLOCA_ESTIMATE = 1024 * 1024;

		class StackSizeVisitor : public core::IRVisitor<StackSizeResult> {

			const utils::map::PointerMap<core::LambdaReferencePtr, core::LambdaExprPtr>& lambdaMappings;

			const core::LambdaReferencePtr recursiveLambdaReference;

			utils::set::PointerSet<core::LambdaReferencePtr> visitedReferences;

			bool debug = false;

			public:
			StackSizeVisitor(const utils::map::PointerMap<core::LambdaReferencePtr, core::LambdaExprPtr>& lambdaMappings,
											 const core::LambdaReferencePtr recursiveLambdaReference) :
												 IRVisitor(false), lambdaMappings(lambdaMappings), recursiveLambdaReference(recursiveLambdaReference) { }

			StackSizeResult visitDeclaration(const core::DeclarationPtr& decl) override {
				if(debug) std::cout << "Visiting declaration" << decl << std::endl;
				unsigned size = getTypeSize(decl->getType());
				return {size, size + visit(decl->getInitialization()).second};
			}

			StackSizeResult visitCallExpr(const core::CallExprPtr& call) override {
				if (const auto& lambdaRef = call->getFunctionExpr().isa<core::LambdaReferencePtr>()) {
					if(debug) std::cout << "Visiting call to LambdaReference" << std::endl;
					//stop descending once we encounter the recursive lambda reference
					if(lambdaRef == recursiveLambdaReference) {
						if(debug) std::cout << "Stopping at passed recursive reference" << std::endl;
						return {0, 0};
					}
					//only follow LambdaReferences if we haven't already seen them
					if(visitedReferences.insert(lambdaRef).second) {
						if(debug) std::cout << "Which is not a recursive one" << std::endl;
						auto subSize = visitNode(lambdaMappings.at(lambdaRef));
						return {subSize.first * STACK_SIZE_RECURSION_ESTIMATE, subSize.second * STACK_SIZE_RECURSION_ESTIMATE};
					}
					if(debug) std::cout << "Which we already visited" << std::endl;

					// special handling for alloca calls
				} else if(const auto& lit =  call->getFunctionExpr().isa<core::LiteralPtr>()) {
					if(lit->getValue()->getValue() == "__builtin_alloca") {
						return {STACK_SIZE_ALLOCA_ESTIMATE, STACK_SIZE_ALLOCA_ESTIMATE};
					}
				}

				if(debug) std::cout << "Visiting call" << std::endl;
				return visitNode(call);
			}

			StackSizeResult visitNode(const core::NodePtr& node) override {
				unsigned firstSize = 0;
				unsigned secondSize = 0;
				for(const auto& child : node->getChildList()) {
					auto childSize = visit(child);
					firstSize += childSize.first;
					secondSize = childSize.second > secondSize ? childSize.second : secondSize;
				}
				return {firstSize, firstSize + secondSize};
			}
		};
	}

	StackSizeResult estimateStackSize(const core::LambdaExprPtr& function) {
		utils::map::PointerMap<core::LambdaReferencePtr, core::LambdaExprPtr> lambdaMappings;
		core::visitDepthFirstOnce(function, [&lambdaMappings](const core::LambdaExprPtr& lambdaExpr) {
			lambdaMappings[lambdaExpr->getReference()] = lambdaExpr;
		});
		StackSizeVisitor visitor(lambdaMappings, function->getReference());
		return visitor.visit(function);
	}

	unsigned getTypeSize(const core::TypePtr& typeIn) {
		core::TypePtr type = typeIn;
		const auto& basic = type->getNodeManager().getLangBasic();
		const auto& refExt = type->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		if(core::analysis::isRefType(type)) type = core::analysis::getReferencedType(type);

		if(core::analysis::isTypeLiteralType(type) || basic.isIdentifier(type) || basic.isUnit(type) || core::analysis::isGeneric(type)) {
			return 0;
		} else if(type.isa<core::FunctionTypePtr>()) {
			return 8;
		} else if(core::analysis::isRefType(type)) {
			return 8;
		} else if(type == basic.getBool()) {
			return 1;
		} else if(type == basic.getChar()) {
			return 1;
		} else if(type == basic.getInt1() || type == basic.getUInt1()) {
			return 1;
		} else if(type == basic.getInt2() || type == basic.getUInt2()) {
			return 2;
		} else if(type == basic.getInt4() || type == basic.getUInt4() || type == basic.getReal4()) {
			return 4;
		} else if(type == basic.getInt8() || type == basic.getUInt8() || type == basic.getReal8()) {
			return 8;
		} else if(type == basic.getInt16() || type == basic.getUInt16()) {
			return 16;
		} else if(type == basic.getIntGen() || type == basic.getUIntGen()) {
			return 4;
		} else if(type == basic.getIntInf() || type == basic.getUIntInf()) {
			return 16;
		} else if(type == basic.getJob() || type == basic.getThreadGroup()) {
			return 0;
		} else if(type == refExt.getMemLoc()) {
			return 8;
		} else if(core::lang::isPointer(type)) {
			return 8;
		} else if(core::lang::isVarList(type)) {
			return 24;
		} else if(core::lang::isArray(type)) {
			if(core::lang::isFixedSizedArray(type)) {
				core::lang::ArrayType array(type);
				return array.getNumElements() * getTypeSize(core::lang::getArrayElementType(type));
			} else {
				return STACK_SIZE_VARIABLE_ARRAY_ESTIMATE * getTypeSize(core::lang::getArrayElementType(type));
			}
		} else if(const auto& structType = core::analysis::isStruct(type)) {
			unsigned size = 0;
			for(const auto& field : structType->getFields()->getFields()) {
				size += getTypeSize(field->getType());
			}
			return size;
		} else if(const auto& unionType = core::analysis::isUnion(type)) {
			unsigned size = 0;
			for(const auto& field : unionType->getFields()->getFields()) {
				auto fieldSize = getTypeSize(field->getType());
				size = fieldSize > size ? fieldSize : size;
			}
			return size;
		} else {
			assert_not_implemented() << "Unknown type: " << *type;
			return 0;
		}
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

