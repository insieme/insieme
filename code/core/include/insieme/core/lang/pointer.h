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

#pragma once

#include "insieme/core/lang/basic.h"
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
		 * Define pointers as a tuple with a alias-shortcut.
		 */
		TYPE_ALIAS("ptr<'a,'c,'v>", "( ref<array<'a>,'c,'v>, int<8> )");

		/**
		 * Any non-qualified pointer is a pointer to a non-const, non volatile data element.
		 */
		TYPE_ALIAS("ptr<'a>", "ptr<'a,f,f>");

		/**
		 * The generic pointer type template e.g. utilized as a reference for is-pointer checks.
		 */
		LANG_EXT_TYPE_WITH_NAME(GenPtr, "generic_ptr_template", "ptr<'a,'c,'v>");			// note: this will be a tuple due to the alias definitions above



		// -- ptr <-> ref converters --


		/**
		 * A built-in operator to convert a reference into a pointer
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrFromRef, "ptr_from_ref",
				"  (r : ref<'a,'c,'v>) -> ptr<'a,'c,'v> {                           "
				"		return ( ref_scalar_to_ref_array(r), 0l );  "
				"  }                                                                     "
		)

		/**
		 * A built-in operator to convert an array to a pointer
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrFromArray, "ptr_from_array",
				"  (r : ref<array<'a,'s>,'c,'v>) -> ptr<'a,'c,'v> {                             "
				"		return ( ref_reinterpret(r,type_lit(array<'a,inf>)), 0l );  "
				"  }                                                                                 "
		)

		/**
		 * A built-in derived operator for extracting references from pointers
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrToRef, "ptr_to_ref",
				"  (p : ptr<'a,'c,'v>) -> ref<'a,'c,'v> {          "
				"		return p.0[p.1];                           "
				"  }                                               "
		)

		/**
		 * A built-in derived operator for extracting array references from pointers
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrToArray, "ptr_to_array",
				"  (p : ptr<'a,'c,'v>) -> ref<array<'a>,'c,'v> {   "
				"		return ref_scalar_to_ref_array(p.0[p.1]);  "
				"  }                                               "
		)

		/**
		 * A built-in derived operator for obtaining pointers to functions.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrOfFunction, "ptr_of_function",
				"(fun : 'a) -> ptr<'a,t,f> {"
				"	return ptr_from_ref(ref_of_function(fun));"
				"}"
		)

		// -- casts --

		/**
		 * A reinterpret cast altering the actual interpretation of the referenced memory cell.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrReinterpret, "ptr_reinterpret",
				"  (p : ptr<'a,'c,'v>, t : type<'b>) -> ptr<'b,'c,'v> {               "
				"		return ptr_from_ref(ref_reinterpret(ptr_to_ref(p), type_lit('b)));   "
				"  }                                                                     "
		)

		/**
		 * A simpler reference cast merely altering the view on the otherwise untouched memory location. This
		 * is the basis for e.g. const or volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrCast, "ptr_cast",
				"  (p : ptr<'a,'c,'v>, c : type<'new_const>, v : type<'new_volatile>) -> ptr<'a,'new_const,'new_volatile> {   "
				"		return ( ref_cast(p.0, c, v, type_lit(plain)), p.1 );      "
				"  }                                                                                                          "
		)

		/**
		 * A specialization of the ref_cast operator for modeling const casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrConstCast, "ptr_const_cast",
				"  (p : ptr<'a,'c,'v>, c : type<'nc>) -> ptr<'a,'nc,'v> {                    "
				"		return ( ref_const_cast(p.0, c), p.1 );   "
				"  }                                                                            "
		)

		/**
		 * A specialization of the ref_cast operator for modeling volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrVolatileCast, "ptr_volatile_cast",
				"  (p : ptr<'a,'c,'v>, v : type<'nv>) -> ptr<'a,'c,'nv> {                       "
				"		return ( ref_volatile_cast(p.0, v), p.1 );   "
				"  }                                                                               "
		)

		/**
		 * Literals to support converting from/to pointers/integral data types.
		 */
		LANG_EXT_LITERAL(PtrFromIntegral, "ptr_from_integral", "('a, type<'b>) -> 'b")
		LANG_EXT_LITERAL(PtrToIntegral, "ptr_to_integral", "(ptr<'a, 'c, 'v>, type<'b>) -> 'b")

		// -- sub-referencing --

		/**
		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrNarrow, "ptr_narrow",
				"  (p : ptr<'a,'c,'v>, dp : datapath<'a,'b>) -> ptr<'b,'c,'v> {                 "
				"		return ptr_from_ref(ref_narrow(ptr_to_ref(p), dp));                        "
				"  }                                                                               "
		)

		/**
		 * The expand operation is the inverse operation of the narrow operation.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrExpand, "ptr_expand",
				"  (p : ptr<'b,'c,'v>, dp : datapath<'a,'b>) -> ptr<'a,'c,'v> {                 "
				"		return ptr_from_ref(ref_expand(ptr_to_ref(p), dp));                        "
				"  }                                                                               "
		)

		/**
		 * A derived operator providing access to an element in an array.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrArrayElement, "ptr_array_elem",
		    "(r : ptr<array<'a,'s>,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> { return ptr_narrow(r, dp_element(dp_root(type_lit(array<'a,'s>)),i)); }"
		)

		/**
		 * A derived reference navigation operator providing access to a member of a struct / union.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrMemberAccess, "ptr_member_access",
		    "(r : ptr<'a,'c,'v>, name : identifier, type : type<'b>) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_member(dp_root(type_lit('a)),name,type)); }"
		)

		/**
		 * A derived reference navigation operator providing access to a components of a tuple.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    PtrComponentAccess, "ptr_component_access",
		    "(r : ptr<'a,'c,'v>, pos : uint<8>, type : type<'b>) -> ptr<'b,'c,'v> { return ptr_narrow(r, dp_component(dp_root(type_lit('a)),pos,type)); }"
		)

		/**
		 * A derived reference-navigation operation providing an array view on a scalar.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrScalarToPtrArray, "ptr_scalar_to_ptr_array",
		    "(a : ptr<'a,'c,'v>) -> ptr<array<'a>,'c,'v> { return ptr_expand(a, dp_element(dp_root(type_lit(array<'a>)),0u)); }"
		)

		/**
		 * A derived operator accessing a element addressed by a pointer + some offset.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrSubscript, "ptr_subscript",
			"(p : ptr<'a,'c,'v>, i : int<8>) -> ref<'a,'c,'v> { return p.0[p.1 + i]; }"
		)

		/**
		 * A derived operator accessing a element addressed by a pointer.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrDeref, "ptr_deref",
			"(p : ptr<'a,'c,'v>) -> 'a { return ref_deref(ptr_to_ref(p)); }"
		)

		// -- null --

		/**
		 * A function generating a null pointer of the specified type.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
				PtrNull, "ptr_null",
				"(a : type<'a>, c : type<'c>, v : type<'v>) -> ptr<'a,'c,'v> { return ( ref_null(type_lit(array<'a,inf>),c,v), 0l ); }"
		)


		// -- comparison operators --

		/**
		 * An operator to compare two references on equality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			PtrEqual, "ptr_eq",
			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 == p2.1; }"
		)

		/**
		 * An operator to compare two references for inequality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(PtrNotEqual, "ptr_ne", "(a : ptr<'a,'c1,'v1>, b : ptr<'a,'c2,'v2>) -> bool { return !ptr_eq(a,b); }")


		LANG_EXT_DERIVED_WITH_NAME(
			PtrLessThan, "ptr_lt",
			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 < p2.1; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrLessEqual, "ptr_le",
			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 <= p2.1; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrGreaterEqual, "ptr_ge",
			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 >= p2.1; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrGreaterThan, "ptr_gt",
			"(p1 : ptr<'a,'c1,'v1>, p2 : ptr<'a,'c2,'v2>) -> bool { return ref_eq(p1.0,p2.0) && p1.1 > p2.1; }"
		)


		// -- pointer arithmetic --

		LANG_EXT_DERIVED_WITH_NAME(
			PtrAdd, "ptr_add",
			"(p : ptr<'a,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> { return ( p.0, p.1 + i ); }"
		)

		LANG_EXT_DERIVED_WITH_NAME(
			PtrSub, "ptr_sub",
			"(p : ptr<'a,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> { return ( p.0, p.1 - i ); }"
		)

		// pointer difference is not defined for unrelated pointers
		LANG_EXT_DERIVED_WITH_NAME(
			PtrDiff, "ptr_diff",
			"(l : ptr<'a,'c,'v>, r : ptr<'a,'c,'v>) -> int<8> { return l.1 - r.1; }"
		)

		LANG_EXT_DERIVED_WITH_NAME(PtrPostInc, "ptr_post_inc", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { var ptr<'a,'c,'v> temp = *p; p = ptr_add(*p, 1l); return temp; }")

		LANG_EXT_DERIVED_WITH_NAME(PtrPostDec, "ptr_post_dec", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { var ptr<'a,'c,'v> temp = *p; p = ptr_sub(*p, 1l); return temp; }")

		LANG_EXT_DERIVED_WITH_NAME(PtrPreInc, "ptr_pre_inc", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { p = ptr_add(*p, 1l); return *p; }")

		LANG_EXT_DERIVED_WITH_NAME(PtrPreDec, "ptr_pre_dec", "(p : ref<ptr<'a,'c,'v>>) -> ptr<'a,'c,'v> { p = ptr_sub(*p, 1l); return *p; }")


		// -- object array memory management --

		// allocates an array of some object type and calls constructor for each element
		LANG_EXT_DERIVED(PtrNewConstructedObjectArray, R"(
			(mem_type : type<'a>, size : uint<8>, constructor : 'mem_type::()) -> ptr<'mem_type,f,f> {
				var uint<inf> local_size = num_cast(size, type_lit(uint<inf>));
				auto arr = ptr_from_array(ref_new(type_lit(array<'mem_type,#local_size>)));
				for(uint<8> i = num_cast(0, type_lit(uint<8>))..size) {
					constructor(ptr_subscript(arr, num_cast(i,type_lit(int<8>))));
				}
				return arr;
			}
		)")

		// calls destructor for each element of constructed array and frees memory
		// black C++ magic, needs more thought to implement as derived (e.g. generate struct with size and ptr in allocation, navigate up from ptr here)
		LANG_EXT_LITERAL(PtrDeleteConstructedObjectArray, "ptr_delete_constructed_object_array", R"(
			(ptr<'mem_type,f,f>, ~mem_type::()) -> unit
		)")
	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle pointer types in a more user
	 * friendly way than its raw encoding.
	 */
	class PointerType {

		/**
		 * The type of element this pointer is addressing.
		 */
		TypePtr elementType;

		/**
		 * A marker indicating whether the referenced memory cell can be modified through this pointer or not (const).
		 */
		TypePtr mConst;

		/**
		 * A marker indicating whether the referenced memory cell might be concurrently modified or not.
		 */
		TypePtr mVolatile;

		/**
		 * Creates a new pointer addressing an element of the given type, being marked const and volatile as specified.
		 */
		PointerType(const TypePtr& elementType, const TypePtr& mConst, const TypePtr& mVolatile)
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
		operator TypePtr() const;


		// --- observers and mutators ---

		const TypePtr& getElementType() const;
		void setElementType(const TypePtr& type);

		bool isConst() const;
		void setConst(bool newState = true);

		bool isVolatile() const;
		void setVolatile(bool newState = true);
	};

	/**
	 * Tests whether the given node is a pointer type or an expression of a pointer type.
	 */
	bool isPointer(const NodePtr& node);

	/**
	 * Creates a new pointer type based on the given specification.
	 */
	TypePtr buildPtrType(const TypePtr& elementType, bool _const = false, bool _volatile = false);

	// constructors and conversions
	ExpressionPtr buildPtrNull(const TypePtr& type);
	ExpressionPtr buildPtrFromRef(const ExpressionPtr& refExpr, bool simplify = true);
	ExpressionPtr buildPtrToRef(const ExpressionPtr& ptrExpr, bool simplify = true);
	ExpressionPtr buildPtrFromArray(const ExpressionPtr& arrExpr);
	ExpressionPtr buildPtrToArray(const ExpressionPtr& ptrExpr);
	ExpressionPtr buildPtrFromIntegral(const ExpressionPtr& intExpr, const TypePtr& ptrType);
	ExpressionPtr buildPtrToIntegral(const ExpressionPtr& ptrExpr, const TypePtr& intType);
	ExpressionPtr buildPtrOfFunction(const ExpressionPtr& funExpr);

	// casts
	ExpressionPtr buildPtrCast(const ExpressionPtr& ptrExpr, bool newConst, bool newVolatile);
	ExpressionPtr buildPtrReinterpret(const ExpressionPtr& ptrExpr, const TypePtr& newElementType);

	// operations
	ExpressionPtr buildPtrDeref(const ExpressionPtr& ptrExpr);
	ExpressionPtr buildPtrSubscript(const ExpressionPtr& ptrExpr, const ExpressionPtr& subscriptExpr);
	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& lhs, const ExpressionPtr& rhs);
	ExpressionPtr buildPtrOperation(BasicGenerator::Operator op, const ExpressionPtr& ptrExpr);

	// object array management
	ExpressionPtr buildPtrNewConstructedObjectArray(const TypePtr& elemType, const ExpressionPtr& size, const ExpressionPtr& constructor);
	ExpressionPtr buildPtrDeleteConstructedObjectArray(const ExpressionPtr& ptrExpr, const ExpressionPtr& destructor);

} // end namespace lang
} // end namespace core
} // end namespace insieme
