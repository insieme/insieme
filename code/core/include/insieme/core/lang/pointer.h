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

#pragma once

#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace lang {

	/**
	     * An extension covering the derived pointer type and all its
	     * associated operators.
	     */
	class PointerExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		PointerExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		// -------------------- pointers ---------------------------

		/**
		 * The generic ref type template e.g. utilized as a reference for is-ref checks.
		 */
		//		LANG_EXT_TYPE_WITH_NAME(GenPtr, "generic_ptr_template", "struct _ir_pointer { ref<array<'a,'v,'c>> data; int<8> offset; }");
		LANG_EXT_TYPE_WITH_NAME(GenPtr, "generic_ptr_template", "ptr<'a,'v,'c>");


		TYPE_ALIAS("ptr<'a,'v,'c>", "struct _ir_pointer { ref<array<'a,'v,'c>> data; int<8> offset; }");
		TYPE_ALIAS("ptr<'a>", "ptr<'a,f,f>");

		// -- ptr <-> ref converters --


		/**
		 * A built-in operator to convert a reference into a pointer
		 */
		//		LANG_EXT_DERIVED_WITH_NAME(PtrFromRef, "ptr_from_ref", "lambda (ref<'a,'v,'c> r) -> struct _ir_pointer { ref<array<'a,'v,'c>> data; int<8>
		// offset;
		//} { return (struct _ir_pointer { ref<array<'a,'v,'c>> data; int<8> offset; }) { ref_scalar_to_ref_array(r), 0 }; }")
		LANG_EXT_LITERAL(PtrFromRef, "ptr_from_ref", "(ref<'a,'v,'c>) -> ptr<'a,'v,'c>")

		/**
		 * A built-in derived operator allocating memory on the heap.
		 */
		//		LANG_EXT_DERIVED_WITH_NAME(PtrToRef, "ptr_to_ref", "lambda (struct _ir_pointer { ref<array<'a,'v,'c>> data; int<8> offset; } p) -> ref<'a,'v,'c>
		//{
		// return p.data[p.offset]; }")
		LANG_EXT_LITERAL(PtrToRef, "ptr_to_ref", "(ptr<'a,'v,'c>) -> ref<'a,'v,'c>")


		// -- casts --

		/**
		 * A reinterpret cast altering the actual interpretation of the referenced memory cell.
		 */
		LANG_EXT_LITERAL(PtrReinterpret, "ptr_reinterpret", "(ptr<'a,'c,'v>,type<'b>) -> ptr<'b,'c,'v>")

		/**
		 * A simpler reference cast merely altering the view on the otherwise untouched memory location. This
		 * is the basis for e.g. const or volatile casts.
		 */
		LANG_EXT_LITERAL(PtrCast, "ptr_cast", "(ptr<'a,'c,'v>,type<'new_const>,type<'new_volatile>) -> ptr<'a,'new_const,'new_volatile>")


		/**
		 * A specialization of the ref_cast operator for modeling const casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrConstCast, "ptr_const_cast", "lambda (ptr<'a,'c,'v> r, type<'nc> c) -> ptr<'a,'nc,'v> { return ptr_cast(r,c,type('v)); }")

		/**
		 * A specialization of the ref_cast operator for modeling volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrVolatileCast, "ptr_volatile_cast",
		                           "lambda (ptr<'a,'c,'v> r, type<'nv> v) -> ptr<'a,'c,'nv> { return ptr_cast(r,type('c),v); }")


		// -- sub-referencing --

		/**
		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
		 */
		LANG_EXT_LITERAL(PtrNarrow, "ptr_narrow", "(ptr<'a,'c,'v>, datapath<'a,'b>) -> ptr<'b,'c,'v>")

		/**
		 * The expand operation is the inverse operation of the narrow operation.
		 */
		LANG_EXT_LITERAL(PtrExpand, "ptr_expand", "(ptr<'b,'c,'v>, datapath<'a,'b>) -> ptr<'a,'c,'v>")


		/**
		 * A derived operator providing access to an element in an array.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrArrayElement, "ref_array_elem",
		    "lambda (ptr<array<'a,'s>,'c,'v> r, int<8> i) -> ptr<'a,'c,'v> { return ptr_narrow(r, dp_element(dp_root(type(array<'a,'s>)),i)); }")

		/**
		 * A derived reference navigation operator providing access to a member of a struct / union.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrMemberAccess, "ref_member_access",
		    "lambda (ptr<'a,'c,'v> r, identifier name, type<'b> type) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_member(dp_root(type('a)),name,type)); }")

		/**
		 * A derived reference navigation operator providing access to a components of a tuple.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrComponentAccess, "ref_component_access",
		    "lambda (ptr<'a,'c,'v> r, uint<8> pos, type<'b> type) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_component(dp_root(type('a)),pos,type)); }")

		/**
		 * A derived reference-navigation operation providing an array view on a scalar.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrScalarToPtrArray, "ref_scalar_to_ref_array",
		                           "lambda (ptr<'a,'c,'v> a) -> ptr<array<'a,1>,'c,'v> { return ptr_expand(a, dp_element(dp_root(type(array<'a,1>)),0u)); }")


		// -- null --

		/**
		 * A null reference constant.
		 */
		LANG_EXT_LITERAL(PtrNull, "ptr_null", "ptr<any,f,f>")


		// -- comparison operators --

		/**
		 * An operator to compare two references on equality.
		 */
		LANG_EXT_LITERAL(PtrEqual, "ptr_eq", "(ptr<'a1,'c1,'v1>, ptr<'a2,'c2,'v2>) -> bool")

		/**
		 * An operator to compare two references for inequality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrNotEqual, "ptr_ne", "lambda (ptr<'a1,'c1,'v1> a, ptr<'a2,'c2,'v2> b) -> bool { return !ptr_eq(a,b); }")


		LANG_EXT_LITERAL(PtrLessThan, "ptr_lt", "(ptr<'a1,'c1,'v1>, ptr<'a2,'c2,'v2>) -> bool")
		LANG_EXT_LITERAL(PtrLessEqual, "ptr_le", "(ptr<'a1,'c1,'v1>, ptr<'a2,'c2,'v2>) -> bool")
		LANG_EXT_LITERAL(PtrGreaterEqual, "ptr_ge", "(ptr<'a1,'c1,'v1>, ptr<'a2,'c2,'v2>) -> bool")
		LANG_EXT_LITERAL(PtrGreaterThan, "ptr_gt", "(ptr<'a1,'c1,'v1>, ptr<'a2,'c2,'v2>) -> bool")


		// -- pointer arithmetic --

		LANG_EXT_LITERAL(PtrAdd, "ptr_add", "(ptr<'a,'c,'v>, int<8>) -> ptr<'a,'c,'v>")
		LANG_EXT_LITERAL(PtrSub, "ptr_sub", "(ptr<'a,'c,'v>, int<8>) -> ptr<'a,'c,'v>")
	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle pointer types in a more user
	 * friendly way then its raw encoding.
	 */
	class PointerType {
		TypePtr elementType;

		bool mConst;

		bool mVolatile;

		PointerType(const TypePtr& elementType, bool mConst, bool mVolatile) : elementType(elementType), mConst(mConst), mVolatile(mVolatile) {}

	  public:
		PointerType(const NodePtr& node);

		PointerType(const PointerType&) = default;
		PointerType(PointerType&&) = default;

		PointerType& operator=(const PointerType&) = default;
		PointerType& operator=(PointerType&&) = default;


		// --- utilities ---

		static bool isPointerType(const NodePtr& node);

		static GenericTypePtr create(const TypePtr& elementType, bool _const = false, bool _volatile = false);

		operator GenericTypePtr() const;

		// --- observers and mutators ---

		const TypePtr& getElementType() const {
			return elementType;
		}

		bool isConst() const {
			return mConst;
		}

		void setConst(bool newState = true) {
			mConst = newState;
		}

		bool isVolatile() const {
			return mVolatile;
		}

		void setVolatile(bool newState = true) {
			mVolatile = newState;
		}
	};

} // end namespace lang
} // end namespace core
} // end namespace insieme
