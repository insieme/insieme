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

#include "insieme/core/lang/array.h"

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Extension ----------------------------


	/**
	 * An extension covering data paths. A data path describes the
	 * access path when navigating within an object allocated by a single
	 * memory allocation call (ref_alloc).
	 */
	class DatapathExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		DatapathExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		/**
		 * The root path is modeling the empty path -- the identity when utilized for narrow / expand operations.
		 */
		LANG_EXT_LITERAL(DataPathRoot, "dp_root", "(type<'a>) -> datapath<'a,'a>")

		/**
		 * The member path extends a given path by accessing a member field  of type 'c of a struct / union 'a.
		 */
		LANG_EXT_LITERAL(DataPathMember, "dp_member", "(datapath<'a,'b>, identifier, type<'c>) -> datapath<'a,'c>")

		/**
		 * An element access operation extends a given path by an access to an element of an array.
		 */
		LANG_EXT_LITERAL(DataPathElement, "dp_element", "(datapath<'a,array<'b,'s>>, int<8>) -> datapath<'a,'b>")

		/**
		 * A component access is extend a data path by accessing an element of a tuple type.
		 */
		LANG_EXT_LITERAL(DataPathComponent, "dp_component", "(datapath<'a,'b>, uint<8>, type<'c>) -> datapath<'a,'c>")

		/**
		 * A parent access path is moving a reference to a base class.
		 */
		LANG_EXT_LITERAL(DataPathParent, "dp_parent", "(datapath<'a,'b>, type<'c>) -> datapath<'a,'c>")
	};

	/**
	 * An extension covering the abstract reference type and all its
	 * associated operators.
	 */
	class ReferenceExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ReferenceExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:


		// import data-path extension for defined literals
		IMPORT_MODULE(DatapathExtension);

		// import array extension for defined literals
		IMPORT_MODULE(ArrayExtension);


		// ------------------ memory location ------------------------

		/**
		 * A type for a set of memory location qualifiers.
		 */
		LANG_EXT_TYPE(MemLoc, "memloc")

		/**
		 * A token representing the stack.
		 */
		LANG_EXT_LITERAL(MemLocStack, "mem_loc_stack", "memloc")

		/**
		 * A token representing the heap.
		 */
		LANG_EXT_LITERAL(MemLocHeap, "mem_loc_heap", "memloc")


		// -------------------- references ---------------------------

		/**
		 * The generic ref type template e.g. utilized as a reference for is-ref checks.
		 */
		LANG_EXT_TYPE_WITH_NAME(GenRef, "generic_ref_template", "ref<'a,'const,'volatile>");

		/**
		 * Add a type alias that maps all 'old' references to non-const, non-volatile references.
		 */
		TYPE_ALIAS("ref<'a>", "ref<'a,f,f>");


		// -- memory management --

		/**
		 * A literal for allocating memory.
		 */
		LANG_EXT_LITERAL(RefAlloc, "ref_alloc", "(type<'a>, memloc) -> ref<'a,f,f>")

		/**
		 * A literal to free memory.
		 */
		LANG_EXT_LITERAL(RefDelete, "ref_delete", "(ref<'a,f,'v>) -> unit")


		/**
		 * A built-in derived operator allocating memory on the stack.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefVar, "ref_var", "lambda ('a v) -> ref<'a,f,f> { decl auto r = ref_alloc(type_of(v), mem_loc_stack); r = v; return r; }")

		/**
		 * A built-in derived operator allocating memory on the heap.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefNew, "ref_new", "lambda ('a v) -> ref<'a,f,f> { decl auto r = ref_alloc(type_of(v), mem_loc_heap ); r = v; return r; }")


		// -- access --

		/**
		 * A literal to obtain the data stored in the memory location referenced by a reference.
		 */
		LANG_EXT_LITERAL(RefDeref, "ref_deref", "(ref<'a,'c,'v>) -> 'a")

		/**
		 * A literal to update the value stored in a memory location referenced by a reference.
		 */
		LANG_EXT_LITERAL(RefAssign, "ref_assign", "(ref<'a,f,'v>, 'a) -> unit")


		// -- casts --

		/**
		 * A reinterpret cast altering the actual interpretation of the referenced memory cell.
		 */
		LANG_EXT_LITERAL(RefReinterpret, "ref_reinterpret", "(ref<'a,'c,'v>,type<'b>) -> ref<'b,'c,'v>")

		/**
		 * A simpler reference cast merely altering the view on the otherwise untouched memory location. This
		 * is the basis for e.g. const or volatile casts.
		 */
		LANG_EXT_LITERAL(RefCast, "ref_cast", "(ref<'a,'c,'v>,type<'new_const>,type<'new_volatile>) -> ref<'a,'new_const,'new_volatile>")


		/**
		 * A specialization of the ref_cast operator for modeling const casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefConstCast, "ref_const_cast", "lambda (ref<'a,'c,'v> r, type<'nc> c) -> ref<'a,'nc,'v> { return ref_cast(r,c,type('v)); }")

		/**
		 * A specialization of the ref_cast operator for modeling volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefVolatileCast, "ref_volatile_cast",
		                           "lambda (ref<'a,'c,'v> r, type<'nv> v) -> ref<'a,'c,'nv> { return ref_cast(r,type('c),v); }")


		// -- sub-referencing --

		/**
		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
		 */
		LANG_EXT_LITERAL(RefNarrow, "ref_narrow", "(ref<'a,'c,'v>, datapath<'a,'b>) -> ref<'b,'c,'v>")

		/**
		 * The expand operation is the inverse operation of the narrow operation.
		 */
		LANG_EXT_LITERAL(RefExpand, "ref_expand", "(ref<'b,'c,'v>, datapath<'a,'b>) -> ref<'a,'c,'v>")


		/**
		 * A derived operator providing access to an element in an array.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    RefArrayElement, "ref_array_elem",
		    "lambda (ref<array<'a,'s>,'c,'v> r, int<8> i) -> ref<'a,'c,'v> { return ref_narrow(r, dp_element(dp_root(type(array<'a,'s>)),i)); }")

		/**
		 * A derived reference navigation operator providing access to a member of a struct / union.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    RefMemberAccess, "ref_member_access",
		    "lambda (ref<'a,'c,'v> r, identifier name, type<'b> type) -> ref<'b,'c,'v> { return ref_narrow(r, dp_member(dp_root(type('a)),name,type)); }")

		/**
		 * A derived reference navigation operator providing access to a components of a tuple.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    RefComponentAccess, "ref_component_access",
		    "lambda (ref<'a,'c,'v> r, uint<8> pos, type<'b> type) -> ref<'b,'c,'v> { return ref_narrow(r, dp_component(dp_root(type('a)),pos,type)); }")

		/**
		 * A derived reference-navigation operation providing an array view on a scalar.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefScalarToRefArray, "ref_scalar_to_ref_array",
		    "lambda (ref<'a,'c,'v> a) -> ref<array<'a>,'c,'v> { return ref_expand(a, dp_element(dp_root(type(array<'a>)),0u)); }")


		// -- null --

		/**
		 * A literal to create a null-reference pointing to no memory location. Such a reference
		 * must not be read or written.
		 */
		LANG_EXT_LITERAL(RefNull, "ref_null", "(type<'a>, type<'c>, type<'v>) -> ref<'a,'c,'v>")


		// -- operators --

		/**
		 * An operator to compare two references on equality.
		 */
		LANG_EXT_LITERAL(RefEqual, "ref_eq", "(ref<'a1,'c1,'v1>, ref<'a2,'c2,'v2>) -> bool")

		/**
		 * An operator to compare two references for inequality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefNotEqual, "ref_ne", "lambda (ref<'a1,'c1,'v1> a, ref<'a2,'c2,'v2> b) -> bool { return !ref_eq(a,b); }")

		/**
		 * A generic pre-order increment operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPreInc,  "gen_pre_inc",  "lambda (ref<'a,f,'v> v)->'a { v=*v+lit(\"1\":'a); return *v; }")

		/**
		 * A generic post-order increment operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPostInc, "gen_post_inc", "lambda (ref<'a,f,'v> v)->'a { decl auto tmp=*v; v=*v+lit(\"1\":'a); return tmp; }")

		/**
		 * A generic pre-order decrement operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPreDec,  "gen_pre_dec",  "lambda (ref<'a,f,'v> v)->'a { v=*v-lit(\"1\":'a); return *v; }")

		/**
		 * A generic post-order decrement operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPostDec, "gen_post_dec", "lambda (ref<'a,f,'v> v)->'a { decl auto tmp=*v; v=*v-lit(\"1\":'a); return tmp; }")


	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle reference types in a more user
	 * friendly way then its raw encoding.
	 */
	class ReferenceType {

		/**
		 * The type of data stored in the referenced memory location.
		 */
		TypePtr elementType;

		/**
		 * A marker indicating whether the referenced memory cell can be modified through this reference or not (const).
		 */
		bool mConst;

		/**
		 * A marker indicating whether the referenced memory cell might be concurrently modified or not.
		 */
		bool mVolatile;

		/**
		 * Creates a new reference type based on the given parameters.
		 */
		ReferenceType(const TypePtr& elementType, bool mConst, bool mVolatile)
			: elementType(elementType), mConst(mConst), mVolatile(mVolatile) {}

	  public:

		/**
		 * Tries to 'parse' the given node to obtain an instance of this wrapper type characterizing
		 * the given reference. The give node may either be a reference type itself or an expression
		 * of a reference type.
		 */
		ReferenceType(const NodePtr& node);

		ReferenceType(const ReferenceType&) = default;
		ReferenceType(ReferenceType&&) = default;

		ReferenceType& operator=(const ReferenceType&) = default;
		ReferenceType& operator=(ReferenceType&&) = default;


		// --- utilities ---

		/**
		 * A factory for reference types.
		 *
		 * see: buildRefType(..) function in enclosing name space
		 */
		static TypePtr create(const TypePtr& elementType, bool _const = false, bool _volatile = false);

		// an implicit converter of this wrapper to an IR type
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

	/**
	 * Determines whether a given node is a reference type or an expression of a reference type.
	 */
	bool isReference(const NodePtr& node);

	/**
	 * A factory function creating a reference type utilizing the given element type and flag combination.
	 */
	TypePtr buildRefType(const TypePtr& elementType, bool _const = false, bool _volatile = false);
		
	bool doReferencesDifferOnlyInQualifiers(const TypePtr& typeA, const TypePtr& typeB);
	
	ExpressionPtr buildRefCast(const ExpressionPtr& refExpr, const TypePtr& targetTy);

} // end namespace lang
} // end namespace core
} // end namespace insieme
