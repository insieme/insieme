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

#include "insieme/backend/addons/simd_vector.h"
#include "insieme/core/annotations/naming.h"


#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/lang/simd_vector.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"



namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* SIMDVectorTypeHandler(const Converter& converter, const core::TypePtr& type) {
			static const TypeInfo* NOT_HANDLED = NULL;
			auto manager = converter.getCNodeManager();

			//get VECTORTYPE (== vector<elemTy, #l>) and produce "elemTy __attribute__(vector_size(#l * sizeof(elemTy)))"
			
			// check whether it is a SIMDVectorType
			if (!(core::lang::isSIMDVector(type) )) {
				return NOT_HANDLED;	// not handled by this handler
			}
		
			const core::VectorTypePtr& ptr = core::lang::getSIMDVectorType(type);

			// get the size of the vector
			unsigned size = 0;
			const core::IntTypeParamPtr& sizePtr = ptr->getSize();
			if (sizePtr->getNodeType() != core::NT_ConcreteIntTypeParam) {
				// non-concrete array types are not supported
				return type_info_utils::createUnsupportedInfo<TypeInfo>(*manager);
			} else {
				size = static_pointer_cast<const core::ConcreteIntTypeParam>(sizePtr)->getValue();
			}

			TypeManager& typeManager = converter.getTypeManager();

			// get the elementtype of the vector
			const TypeInfo* elementTypeInfo = &typeManager.getTypeInfo(ptr->getElementType());
			assert(elementTypeInfo);

			//__attribute__((vector_size(n)) where n specifies the size in bytes
			string elementTypeName = toString(insieme::backend::c_ast::CPrint(elementTypeInfo->rValueType));
			string str = elementTypeName + " __attribute__((vector_size(" + toString(size) + "*sizeof("+ elementTypeName+"))))";
			auto res = type_info_utils::createInfo<TypeInfo>(*manager, str);
			return res;
		}


		OperatorConverterTable getSIMDVectorOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<insieme::core::lang::SIMDVectorExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			res[ext.getSIMDAdd()] 	  = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDSub()] 	  = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDMul()] 	  = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDDiv()] 	  = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDMod()] 	  = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDAnd()] 	  = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDOr()] 	  = OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDXor()] 	  = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDLShift()] 	  = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDRShift()] 	  = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1) ); });

			res[ext.getSIMDEq()] 	  = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDNe()] 	  = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDLt()] 	  = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDLe()] 	  = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDGt()] 	  = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1) ); });
			res[ext.getSIMDGe()] 	  = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1) ); });

			res[ext.getSIMDNot()] 	  = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });
			res[ext.getSIMDMinus()]		= OP_CONVERTER({ return c_ast::minus(CONVERT_ARG(0)); });
			
			//res[ext.getSIMDInitUniform()] 	= OP_CONVERTER({ return CONVERT_ARG(0); });
			res[ext.getSIMDInitUndefined()] = OP_CONVERTER({ return CONVERT_ARG(0); });
			res[ext.getSIMDInitPartial()] 	= OP_CONVERTER({
					const core::TypePtr type = call->getType();
					const TypeInfo& info = GET_TYPE_INFO(type);

					auto values = insieme::core::encoder::toValue<vector<insieme::core::ExpressionPtr>>(ARG(0));
					auto converted = ::transform(values, [&](const core::ExpressionPtr& cur)->c_ast::NodePtr { return CONVERT_EXPR(cur); });

					return c_ast::init(info.rValueType , converted);
					});

			/*
			res[ext.getVectorToSIMD()] 	  = OP_CONVERTER({ return CONVERT_ARG(0); });
			res[ext.getSIMDToVector()] 	  = OP_CONVERTER({ return CONVERT_ARG(0); });
			*/
			#include "insieme/backend/operator_converter_end.inc"
			return res;
		}

	}

	void SIMDVector::installOn(Converter& converter) const {

		// registers type handler
		converter.getTypeManager().addTypeHandler(SIMDVectorTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getSIMDVectorOperatorTable(converter.getNodeManager()));

	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme
