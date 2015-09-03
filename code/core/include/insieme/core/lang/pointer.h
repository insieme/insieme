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
#include "insieme/core/lang/reference.h"

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

		// this extension is based upon the symbols defined by the reference module
		IMPORT_MODULE(ReferenceExtension);

		// -------------------- pointers ---------------------------


		/**
		 * Define pointers as a struct with a alias-shortcut.
		 */
		TYPE_ALIAS("ptr<'a,'c,'v>", "struct _ir_pointer { ref<array<'a>,'c,'v> data; int<8> offset; }");

		/**
		 * Any non-qualified pointer is a pointer to a non-const, non volatile data element.
		 */
		TYPE_ALIAS("ptr<'a>", "ptr<'a,f,f>");

		/**
		 * The generic pointer type template e.g. utilized as a reference for is-pointer checks.
		 */
		LANG_EXT_TYPE_WITH_NAME(GenPtr, "generic_ptr_template", "ptr<'a,'c,'v>");			// note: this will be a struct due to the alias definitions above



		// -- ptr <-> ref converters --


		/**
		 * A built-in operator to convert a reference into a pointer
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrFromRef, "ptr_from_ref",
				"  lambda (ref<'a,'c,'v> r) -> ptr<'a,'c,'v> {                           "
				"		return struct ptr<'a,'c,'v> { ref_scalar_to_ref_array(r), 0l };  "
				"  }                                                                     "
		)

		/**
		 * A built-in operator to convert an array to a pointer
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrFromArray, "ptr_from_array",
				"  lambda (ref<array<'a,'s>,'c,'v> r) -> ptr<'a,'c,'v> {                             "
				"		return struct ptr<'a,'c,'v> { ref_reinterpret(r,type(array<'a,inf>)), 0l };  "
				"  }                                                                                 "
		)

		/**
		 * A built-in derived operator allocating memory on the heap.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrToRef, "ptr_to_ref",
				"  lambda (ptr<'a,'c,'v> p) -> ref<'a,'c,'v> {   "
				"		return p.data[p.offset];                 "
				"  }                                             "
		)


		// -- casts --

		/**
		 * A reinterpret cast altering the actual interpretation of the referenced memory cell.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrReinterpret, "ptr_reinterpret",
				"  lambda (ptr<'a,'c,'v> p, type<'b> t) -> ptr<'b,'c,'v> {               "
				"		return ptr_from_ref(ref_reinterpret(ptr_to_ref(p), type('b)));   "
				"  }                                                                     "
		)

		/**
		 * A simpler reference cast merely altering the view on the otherwise untouched memory location. This
		 * is the basis for e.g. const or volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrCast, "ptr_cast",
				"  lambda (ptr<'a,'c,'v> p, type<'new_const> c,type<'new_volatile> v) -> ptr<'a,'new_const,'new_volatile> {   "
				"		return struct ptr<'a,'new_const,'new_volatile> { ref_cast(p.data, c, v), p.offset };                  "
				"  }                                                                                                          "
		)

		/**
		 * A specialization of the ref_cast operator for modeling const casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrConstCast, "ptr_const_cast",
				"  lambda (ptr<'a,'c,'v> p, type<'nc> c) -> ptr<'a,'nc,'v> {                    "
				"		return struct ptr<'a,'nc,'v> { ref_const_cast(p.data, c), p.offset };   "
				"  }                                                                            "
		)

		/**
		 * A specialization of the ref_cast operator for modeling volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrVolatileCast, "ptr_volatile_cast",
				"  lambda (ptr<'a,'c,'v> p, type<'nv> v) -> ptr<'a,'c,'nv> {                       "
				"		return struct ptr<'a,'c,'nv> { ref_volatile_cast(p.data, v), p.offset };   "
				"  }                                                                               "
		)


		// -- sub-referencing --

		/**
		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrNarrow, "ptr_narrow",
				"  lambda (ptr<'a,'c,'v> p, datapath<'a,'b> dp) -> ptr<'b,'c,'v> {                 "
				"		return ptr_from_ref(ref_narrow(ptr_to_ref(p), dp));                        "
				"  }                                                                               "
		)

		/**
		 * The expand operation is the inverse operation of the narrow operation.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrExpand, "ptr_expand",
				"  lambda (ptr<'b,'c,'v> p, datapath<'a,'b> dp) -> ptr<'a,'c,'v> {                 "
				"		return ptr_from_ref(ref_expand(ptr_to_ref(p), dp));                        "
				"  }                                                                               "
		)

		/**
		 * A derived operator providing access to an element in an array.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrArrayElement, "ptr_array_elem",
		    "lambda (ptr<array<'a,'s>,'c,'v> r, int<8> i) -> ptr<'a,'c,'v> { return ptr_narrow(r, dp_element(dp_root(type(array<'a,'s>)),i)); }"
		)

		/**
		 * A derived reference navigation operator providing access to a member of a struct / union.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrMemberAccess, "ptr_member_access",
		    "lambda (ptr<'a,'c,'v> r, identifier name, type<'b> type) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_member(dp_root(type('a)),name,type)); }"
		)

		/**
		 * A derived reference navigation operator providing access to a components of a tuple.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrComponentAccess, "ptr_component_access",
		    "lambda (ptr<'a,'c,'v> r, uint<8> pos, type<'b> type) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_component(dp_root(type('a)),pos,type)); }"
		)

		/**
		 * A derived reference-navigation operation providing an array view on a scalar.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrScalarToPtrArray, "ptr_scalar_to_ptr_array",
		    "lambda (ptr<'a,'c,'v> a) -> ptr<array<'a>,'c,'v> { return ptr_expand(a, dp_element(dp_root(type(array<'a>)),0u)); }"
		)


		/**
		 * A derived operator accessing a element addressed by a pointer + some offset.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrSubscript, "ptr_subscript",
			"lambda (ptr<'a,'c,'v> p, int<8> i) -> ref<'a,'c,'v> { return p.data[p.offset + i]; }"
		)

		// -- null --

		/**
		 * A function generating a null pointer of the specified type.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
				PtrNull, "ptr_null",
				"lambda (type<'a> a, type<'c> c, type<'v> v) -> ptr<'a,'c,'v> { return struct ptr<'a,'c,'v> { ref_null(type(array<'a,inf>),c,v), 0 }; }"
		)


		// -- comparison operators --

		/**
		 * An operator to compare two references on equality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrEqual, "ptr_eq",
			"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset == p2.offset; }"
		)

		/**
		 * An operator to compare two references for inequality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrNotEqual, "ptr_ne", "lambda (ptr<'a,'c1,'v1> a, ptr<'a,'c2,'v2> b) -> bool { return !ptr_eq(a,b); }")


		LANG_EXT_DERIVED_WITH_NAME(
			PtrLessThan, "ptr_lt",
			"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset < p2.offset; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrLessEqual, "ptr_le",
			"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset <= p2.offset; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrGreaterEqual, "ptr_ge",
			"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset >= p2.offset; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrGreaterThan, "ptr_gt",
			"lambda (ptr<'a,'c1,'v1> p1, ptr<'a,'c2,'v2> p2) -> bool { return ref_eq(p1.data,p2.data) && p1.offset > p2.offset; }"
		)


		// -- pointer arithmetic --

		LANG_EXT_DERIVED_WITH_NAME(
			PtrAdd, "ptr_add",
			"lambda (ptr<'a,'c,'v> p, int<8> i) -> ptr<'a,'c,'v> { return struct ptr<'a,'c,'v> { p.data, p.offset + i }; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrSub, "ptr_sub",
			"lambda (ptr<'a,'c,'v> p, int<8> i) -> ptr<'a,'c,'v> { return struct ptr<'a,'c,'v> { p.data, p.offset - i }; }"
		)

	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle pointer types in a more user
	 * friendly way then its raw encoding.
	 */
	class PointerType {

		/**
		 * The type of element this pointer is addressing.
		 */
		TypePtr elementType;

		/**
		 * A flag indicating whether the addressed element is not mutable (constant) through
		 * this this pointer.
		 */
		bool mConst;

		/**
		 * A flag indicating whether the addressed element is volatile.
		 */
		bool mVolatile;

		/**
		 * Creates a new pointer addressing an element of the given type, being marked const and volatile as specified.
		 */
		PointerType(const TypePtr& elementType, bool mConst, bool mVolatile)
			: elementType(elementType), mConst(mConst), mVolatile(mVolatile) {}

	  public:

		/**
		 * Creates a new instance of this wrapper class by 'parsing' the given node.
		 * If the node is a type, it attempts to interpret it as a pointer type. If the
		 * node is an expression, it utilizes its type. If unsuccessful, a exception
		 * will be raised.
		 *
		 * See also: isPointer(NodePtr) utility function
		 */
		PointerType(const NodePtr& node);

		// a default copy constructor
		PointerType(const PointerType&) = default;

		// a default r-value copy constructor
		PointerType(PointerType&&) = default;

		// a default assignment operator
		PointerType& operator=(const PointerType&) = default;

		// a default r-value assignment operator
		PointerType& operator=(PointerType&&) = default;


		// --- utilities ---

		// a factory function for pointer types
		static TypePtr create(const TypePtr& elementType, bool _const = false, bool _volatile = false);

		// an implicit converter from this wrapper type to an IR type
		operator StructTypePtr() const;


		// --- observers and mutators ---

		const TypePtr& getElementType() const {
			return elementType;
		}

		void setElementType(const TypePtr& type) {
			assert_true(type) << "Element type must not be null!";
			elementType = type;
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
	
	/**
	 * Tests whether the given node is a pointer type or an expression of a pointer type.
	 */
	bool isPointer(const NodePtr& node);
	
	/**
	 * Creates a new pointer type based on the given specification.
	 */
	TypePtr buildPtrType(const TypePtr& elementType, bool _const = false, bool _volatile = false);


	bool doPointersDifferOnlyInQualifiers(const TypePtr& typeA, const TypePtr& typeB);
	
	ExpressionPtr buildPtrNull(const TypePtr& type);
	ExpressionPtr buildPtrFromRef(const ExpressionPtr& refExpr);
	ExpressionPtr buildPtrFromArray(const ExpressionPtr& arrExpr);
	ExpressionPtr buildPtrToRef(const ExpressionPtr& ptrExpr);
	ExpressionPtr buildPtrCast(const ExpressionPtr& ptrExpr, const TypePtr& targetTy);

	ExpressionPtr buildPtrSubscript(const ExpressionPtr& ptrExpr, const ExpressionPtr& subscriptExpr);

} // end namespace lang
} // end namespace core
} // end namespace insieme
