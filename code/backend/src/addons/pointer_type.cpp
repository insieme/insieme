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


//			// -- sub-referencing --
//
//			/**
//			 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(PtrNarrow, "ptr_narrow",
//					"  lambda (ptr<'a,'c,'v> p, datapath<'a,'b> dp) -> ptr<'b,'c,'v> {                 "
//					"		return ptr_from_ref(ref_narrow(ptr_to_ref(p), dp));                        "
//					"  }                                                                               "
//			)
//
//			/**
//			 * The expand operation is the inverse operation of the narrow operation.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(PtrExpand, "ptr_expand",
//					"  lambda (ptr<'b,'c,'v> p, datapath<'a,'b> dp) -> ptr<'a,'c,'v> {                 "
//					"		return ptr_from_ref(ref_expand(ptr_to_ref(p), dp));                        "
//					"  }                                                                               "
//			)
//
//			/**
//			 * A derived operator providing access to an element in an array.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrArrayElement, "ptr_array_elem",
//				"lambda (ptr<array<'a,'s>,'c,'v> r, int<8> i) -> ptr<'a,'c,'v> { return ptr_narrow(r, dp_element(dp_root(type_lit(array<'a,'s>)),i)); }"
//			)
//
//			/**
//			 * A derived reference navigation operator providing access to a member of a struct / union.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrMemberAccess, "ptr_member_access",
//				"lambda (ptr<'a,'c,'v> r, identifier name, type<'b> type) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_member(dp_root(type_lit('a)),name,type)); }"
//			)
//
//			/**
//			 * A derived reference navigation operator providing access to a components of a tuple.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrComponentAccess, "ptr_component_access",
//				"lambda (ptr<'a,'c,'v> r, uint<8> pos, type<'b> type) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_component(dp_root(type_lit('a)),pos,type)); }"
//			)
//
//			/**
//			 * A derived reference-navigation operation providing an array view on a scalar.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrScalarToPtrArray, "ptr_scalar_to_ptr_array",
//				"lambda (ptr<'a,'c,'v> a) -> ptr<array<'a>,'c,'v> { return ptr_expand(a, dp_element(dp_root(type_lit(array<'a>)),0u)); }"
//			)
//
//			/**
//			 * A derived operator accessing a element addressed by a pointer + some offset.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrSubscript, "ptr_subscript",
//				"lambda (ptr<'a,'c,'v> p, int<8> i) -> ref<'a,'c,'v> { return p.data[p.offset + i]; }"
//			)
//
//			/**
//			 * A derived operator accessing a element addressed by a pointer.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrDeref, "ptr_deref",
//				"lambda (ptr<'a,'c,'v> p) -> 'a { return ref_deref(ptr_to_ref(p)); }"
//			)
//
//			// -- null --
//
//			/**
//			 * A function generating a null pointer of the specified type.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//					PtrNull, "ptr_null",
//					"lambda (type<'a> a, type<'c> c, type<'v> v) -> ptr<'a,'c,'v> { return struct ptr<'a,'c,'v> { ref_null(type_lit(array<'a,inf>),c,v), 0 }; }"
//			)
//
//
//			// -- comparison operators --
//
//			/**
//			 * An operator to compare two references on equality.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrEqual, "ptr_eq",
//				"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset == p2.offset; }"
//			)
//
//			/**
//			 * An operator to compare two references for inequality.
//			 */
//			LANG_EXT_DERIVED_WITH_NAME(PtrNotEqual, "ptr_ne", "lambda (ptr<'a,'c1,'v1> a, ptr<'a,'c2,'v2> b) -> bool { return !ptr_eq(a,b); }")
//
//
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrLessThan, "ptr_lt",
//				"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset < p2.offset; }"
//			)
//
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrLessEqual, "ptr_le",
//				"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset <= p2.offset; }"
//			)
//
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrGreaterEqual, "ptr_ge",
//				"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset >= p2.offset; }"
//			)
//
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrGreaterThan, "ptr_gt",
//				"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset > p2.offset; }"
//			)
//
//
//			// -- pointer arithmetic --
//
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrAdd, "ptr_add",
//				"lambda (ptr<'a,'c,'v> p, int<8> i) -> ptr<'a,'c,'v> { return struct ptr<'a,'c,'v> { p.data, p.offset + i }; }"
//			)
//
//			LANG_EXT_DERIVED_WITH_NAME(
//				PtrSub, "ptr_sub",
//				"lambda (ptr<'a,'c,'v> p, int<8> i) -> ptr<'a,'c,'v> { return struct ptr<'a,'c,'v> { p.data, p.offset - i }; }"
//			)
//
//			LANG_EXT_DERIVED(PtrPostInc, "lambda (ref<ptr<'a,'c,'v>> p) -> ptr<'a,'c,'v> { decl ptr<'a,'c,'v> temp = *p; p = ptr_add(*p, 1l); return temp; }")
//
//			LANG_EXT_DERIVED(PtrPostDec, "lambda (ref<ptr<'a,'c,'v>> p) -> ptr<'a,'c,'v> { decl ptr<'a,'c,'v> temp = *p; p = ptr_sub(*p, 1l); return temp; }")
//
//			LANG_EXT_DERIVED(PtrPreInc, "lambda (ref<ptr<'a,'c,'v>> p) -> ptr<'a,'c,'v> { p = ptr_add(*p, 1l); return *p; }")
//
//			LANG_EXT_DERIVED(PtrPreDec, "lambda (ref<ptr<'a,'c,'v>> p) -> ptr<'a,'c,'v> { p = ptr_sub(*p, 1l); return *p; }")


//			// ------------------ complex specific operators ---------------
//			res[ext.getRefComplexReal()] = OP_CONVERTER { return c_ast::ref(c_ast::complexReal(c_ast::deref(CONVERT_ARG(0)))); };
//
//			res[ext.getRefComplexImg()] = OP_CONVERTER { return c_ast::ref(c_ast::complexImag(c_ast::deref(CONVERT_ARG(0)))); };
//
//			res[ext.getComplexReal()] = OP_CONVERTER { return c_ast::complexReal(CONVERT_ARG(0)); };
//
//			res[ext.getComplexImg()] = OP_CONVERTER { return c_ast::complexImag(CONVERT_ARG(0)); };
//
//			// -------------------- cast operators -------------------------
//
//			res[ext.getConstantToComplex()] = OP_CONVERTER { return CONVERT_ARG(0); };
//
//			res[ext.getComplexToBool()] = OP_CONVERTER { return CONVERT_ARG(0); };
//
//			res[ext.getComplexToComplex()] = OP_CONVERTER { return CONVERT_ARG(0); };
//
//			// -------------------- generic operators ----------------------
//			res[gen.getGenAdd()] = OP_CONVERTER {
//				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); }
//				return NULL;
//			};
//
//			res[gen.getGenSub()] = OP_CONVERTER {
//				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); }
//				return NULL;
//			};
//
//			res[gen.getGenMul()] = OP_CONVERTER {
//				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); }
//				return NULL;
//			};
//
//			res[gen.getGenDiv()] = OP_CONVERTER {
//				if(isComplexType(ARG(0)->getType()) || isComplexType(ARG(1)->getType())) { return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); }
//				return NULL;
//			};

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
