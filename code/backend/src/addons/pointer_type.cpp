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

#include "insieme/backend/addons/pointer_type.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/lang/pointer.h"
//
//#include "insieme/core/analysis/ir_utils.h"
//#include "insieme/core/analysis/ir++_utils.h"
//#include "insieme/core/lang/basic.h"
//#include "insieme/core/lang/complex.h"




namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* PointerTypeHandler(const Converter& converter, const core::TypePtr& type) {
			// only interested in pointers
			if (!core::lang::isPointer(type)) return nullptr;

			// parse the pointer type
			core::lang::PointerType ptr(type);

			// build up TypeInfo for the pointer type
			TypeManager& typeManager = converter.getTypeManager();

			// load type information of base type
			const TypeInfo& elementInfo = typeManager.getTypeInfo(ptr.getElementType());

			// create pointer type
			auto cType = c_ast::ptr(c_ast::qualify(elementInfo.lValueType, ptr.isConst(), ptr.isVolatile()));

			// build up and return resulting type information
			return type_info_utils::createInfo(cType, elementInfo.declaration);
		}


		OperatorConverterTable getPointerTypeOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::PointerExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			// ------------------ point from / to ref ---------------

			res[ext.getPtrToRef()] = OP_CONVERTER { return CONVERT_ARG(0); };
			res[ext.getPtrFromRef()] = OP_CONVERTER { return CONVERT_ARG(0); };
			res[ext.getPtrFromArray()] = OP_CONVERTER { return CONVERT_ARG(0); };
			res[ext.getPtrOfFunction()] = OP_CONVERTER { return CONVERT_ARG(0); };


			// ------------------------ casts -----------------------

			auto cast = OP_CONVERTER {
				if (call[0]->getType() == call->getType()) return CONVERT_ARG(0);
				return c_ast::cast(CONVERT_TYPE(call->getType()), CONVERT_ARG(0));
			};

			res[ext.getPtrCast()] = cast;
			res[ext.getPtrReinterpret()] = cast;
			res[ext.getPtrConstCast()] = cast;
			res[ext.getPtrVolatileCast()] = cast;

			res[ext.getPtrFromIntegral()] = cast;
			res[ext.getPtrToIntegral()] = cast;


			// ------------------------ sub-referencing ------------------------

			res[ext.getPtrNarrow()] =           OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrExpand()] =           OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrArrayElement()] =     OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrMemberAccess()] =     OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrComponentAccess()] =  OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrScalarToPtrArray()] = OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };

			res[ext.getPtrSubscript()] = OP_CONVERTER { return c_ast::subscript(CONVERT_ARG(0), CONVERT_ARG(1)); };


			// ------------------------ de-referencing ------------------------

			res[ext.getPtrDeref()] = OP_CONVERTER { return c_ast::deref(CONVERT_ARG(0)); };


			// ------------------------ null ------------------------

			res[ext.getPtrNull()] = OP_CONVERTER { return c_ast::cast(CONVERT_TYPE(call->getType()), C_NODE_MANAGER->create<c_ast::Literal>("0")); };


			// ------------------------ comparison operators ------------------------

			res[ext.getPtrEqual()] =        OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrNotEqual()] =     OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrLessThan()] =     OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrLessEqual()] =    OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrGreaterThan()] =  OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrGreaterEqual()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };


			// ------------------------ pointer arithmetic ------------------------

			res[ext.getPtrAdd()] =     OP_CONVERTER { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrSub()] =     OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrPostInc()] = OP_CONVERTER { return c_ast::postInc(c_ast::deref(CONVERT_ARG(0))); };
			res[ext.getPtrPostDec()] = OP_CONVERTER { return c_ast::postDec(c_ast::deref(CONVERT_ARG(0))); };
			res[ext.getPtrPreInc()] =  OP_CONVERTER { return c_ast::preInc(c_ast::deref(CONVERT_ARG(0))); };
			res[ext.getPtrPreDec()] =  OP_CONVERTER { return c_ast::preDec(c_ast::deref(CONVERT_ARG(0))); };


			#include "insieme/backend/operator_converter_end.inc"

			return res;
		}
	}

	void PointerType::installOn(Converter& converter) const {
		// registers type handler
		converter.getTypeManager().addTypeHandler(PointerTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getPointerTypeOperatorTable(converter.getNodeManager()));
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme
