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

#include "insieme/core/lang/extension.h"

#include "insieme/core/lang/datapath.h"

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Extension ----------------------------

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

		/**
		 * A type alias for arrays with undefined size.
		 */
		// We need to define this alias here in order to avoid a cyclic dependency
		TYPE_ALIAS("array<'a>", "array<'a,inf>");


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


		// ------------------- reference kind ------------------------

		/**
		 * The marker for plain IR references.
		 */
		LANG_EXT_TYPE_WITH_NAME(MarkerPlain, "plain_marker", "plain");

		/**
		 * The marker for marking IR references as being C++ references.
		 */
		LANG_EXT_TYPE_WITH_NAME(MarkerCppReference, "reference_marker", "cpp_ref");

		/**
		 * The marker for marking IR references as being C++ r-value references.
		 */
		LANG_EXT_TYPE_WITH_NAME(MarkerCppRValueReference, "rvalue_reference_marker", "cpp_rref");

		/**
		 * The marker for qualified IR non-references.
		 */
		LANG_EXT_TYPE_WITH_NAME(MarkerQualified, "qualified_marker", "qualified");


		// -------------------- references ---------------------------

		/**
		 * The generic ref type template e.g. utilized as a reference for is-ref checks.
		 */
		LANG_EXT_TYPE_WITH_NAME(GenRef, "generic_ref_template", "ref<'a,'const,'volatile,'kind>");

		/**
		 * Add a type alias that maps all 'old' references to non-const, non-volatile references.
		 */
		TYPE_ALIAS("ref<'a>", "ref<'a,f,f,plain>");

		/**
		 * Add a type alias that maps all 'C' references to plain references.
		 */
		TYPE_ALIAS("ref<'a,'const,'volatile>", "ref<'a,'const,'volatile,plain>");

		/**
		* Add a type alias that maps all cpp_ref to the encoding of C++ references.
		*/
		TYPE_ALIAS("cpp_ref<'a,'const,'volatile>", "ref<'a,'const,'volatile,cpp_ref>");

		/**
		* Add a type alias that maps all cpp_rref to the encoding of C++ r-value references.
		*/
		TYPE_ALIAS("cpp_rref<'a,'const,'volatile>", "ref<'a,'const,'volatile,cpp_rref>");


		// -- memory management --

		/**
		 * A literal for allocating memory.
		 */
		LANG_EXT_LITERAL(RefAlloc, "ref_alloc", "(type<'a>, memloc) -> ref<'a,f,f>")

		/**
		 * A literal for referencing memory allocated in a surrounding declaration context.
		 */
		LANG_EXT_LITERAL(RefDecl, "ref_decl", "(type<ref<'a,'c,'v,'k>>) -> ref<'a,'c,'v,'k>")

		/**
		 * A literal to free memory.
		 */
		LANG_EXT_LITERAL(RefDelete, "ref_delete", "(ref<'a,f,'v>) -> unit")

		/**
		 * A built-in derived operator allocating memory on the stack.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefTemp, "ref_temp", "(t : type<'a>) -> ref<'a,f,f> { return ref_alloc(t, mem_loc_stack); }")

		/**
		 * A built-in derived operator allocating memory on the stack and initializing it.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefTempInit, "ref_temp_init", "(v : 'a) -> ref<'a,f,f> { auto r = ref_temp(type_lit('a)); r = v; return r; }")

		/**
		 * A built-in derived operator allocating memory on the heap.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefNew, "ref_new", "(t : type<'a>) -> ref<'a,f,f> { return ref_alloc(t, mem_loc_heap ); }")

		/**
		 * A built-in derived operator allocating memory on the heap and initializing it.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefNewInit, "ref_new_init", "(v : 'a) -> ref<'a,f,f> { auto r = ref_new(type_lit('a)); r = v; return r; }")

		/**
		 * A built-in abstract operator obtaining references to functions.
		 */
		LANG_EXT_LITERAL(RefOfFunction, "ref_of_function", "('a) -> ref<'a,t,f>")

		// -- access --

		/**
		 * A literal to obtain the data stored in the memory location referenced by a reference.
		 */
		LANG_EXT_LITERAL(RefDeref, "ref_deref", "(ref<'a,'c,'v,'k>) -> 'a")

		/**
		 * A literal to update the value stored in a memory location referenced by a reference.
		 */
		LANG_EXT_LITERAL(RefAssign, "ref_assign", "(ref<'a,f,'v,'k>, 'a) -> unit")


		// -- casts --

		/**
		 * A reinterpret cast altering the actual interpretation of the referenced memory cell.
		 */
		LANG_EXT_LITERAL(RefReinterpret, "ref_reinterpret", "(ref<'a,'c,'v,'k>, type<'b>) -> ref<'b,'c,'v,'k>")

		/**
		 * A simpler reference cast merely altering the view on the otherwise untouched memory location. This
		 * is the basis for e.g. const or volatile casts.
		 */
		LANG_EXT_LITERAL(RefCast, "ref_cast", "(ref<'a,'c,'v,'k>, type<'new_const>, type<'new_volatile>, type<'new_kind>) -> ref<'a,'new_const,'new_volatile,'new_kind>")

		/**
		 * A cast to navigate to a parent struct.
		 */
		LANG_EXT_DERIVED(RefParentCast, "(r: ref<'a,'c,'v,'k>, t: type<'b>) -> ref<'b,'c,'v,'k> { return ref_narrow(r, dp_parent(dp_root(type_lit('a)), t)); }")


		/**
		 * A specialization of the ref_cast operator for modeling const casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefConstCast, "ref_const_cast",
			                       "(r : ref<'a,'c,'v,'k>, c : type<'nc>) -> ref<'a,'nc,'v,'k> { return ref_cast(r, c, type_lit('v), type_lit('k)); }")

		/**
		 * A specialization of the ref_cast operator for modeling volatile casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefVolatileCast, "ref_volatile_cast",
			                       "(r : ref<'a,'c,'v,'k>, v : type<'nv>) -> ref<'a,'c,'nv,'k> { return ref_cast(r, type_lit('c), v, type_lit('k)); }")

		/**
		 * A specialization of the ref_cast operator for modeling kind casts.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefKindCast, "ref_kind_cast",
			                       "(r : ref<'a,'c,'v,'k>, k : type<'nk>) -> ref<'a,'c,'v,'nk> { return ref_cast(r, type_lit('c), type_lit('v), k); }")


		// -- sub-referencing --

		/**
		 * The narrow operation is obtaining a reference to a sub-object within a referenced object.
		 */
		LANG_EXT_LITERAL(RefNarrow, "ref_narrow", "(ref<'a,'c,'v,'k>, datapath<'a,'b>) -> ref<'b,'c,'v,'k>")

		/**
		 * The expand operation is the inverse operation of the narrow operation.
		 */
		LANG_EXT_LITERAL(RefExpand, "ref_expand", "(ref<'b,'c,'v,'k>, datapath<'a,'b>) -> ref<'a,'c,'v,'k>")

		/**
		 * A derived operator providing access to an element in an array.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    RefArrayElement, "ref_array_elem",
		    "(r : ref<array<'a,'s>,'c,'v,plain>, i : int<8>) -> ref<'a,'c,'v, plain> { return ref_narrow(r, dp_element(dp_root(type_lit(array<'a,'s>)), i)); }")

		/**
		 * A derived reference navigation operator providing access to a member of a struct / union.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
			RefMemberAccess, "ref_member_access",
			"(r : ref<'a,'c,'v,'k>, name : identifier, type : type<'b>) -> ref<'b,'c,'v, plain> {                    "
			"    return ref_kind_cast(ref_narrow(r, dp_member(dp_root(type_lit('a)), name, type)), type_lit(plain)); "
			"}")

		/**
		 * A derived reference navigation operator providing access to a components of a tuple.
		 */
		LANG_EXT_DERIVED_WITH_NAME(
		    RefComponentAccess, "ref_component_access",
		    "(r : ref<'a,'c,'v,'k>, pos : uint<8>, type : type<'b>) -> ref<'b,'c,'v,'k> { return ref_narrow(r, dp_component(dp_root(type_lit('a)), pos, type)); }")

		/**
		 * A derived reference-navigation operation providing an array view on a scalar.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefScalarToRefArray, "ref_scalar_to_ref_array",
		    "(a : ref<'a,'c,'v,plain>) -> ref<array<'a>,'c,'v,plain> { return ref_expand(a, dp_element(dp_root(type_lit(array<'a>)), 0u)); }")


		// -- null --

		/**
		 * A literal to create a null-reference pointing to no memory location. Such a reference
		 * must not be read or written.
		 */
		LANG_EXT_LITERAL(RefNull, "ref_null", "(type<'a>, type<'c>, type<'v>) -> ref<'a,'c,'v,plain>")


		// -- operators --

		/**
		 * An operator to compare two references on equality.
		 */
		LANG_EXT_LITERAL(RefEqual, "ref_eq", "(ref<'a1,'c1,'v1,'k1>, ref<'a2,'c2,'v2,'k2>) -> bool")

		/**
		 * An operator to compare two references for inequality.
		 */
		LANG_EXT_DERIVED_WITH_NAME(RefNotEqual, "ref_ne", "(a : ref<'a1,'c1,'v1,'k1>, b : ref<'a2,'c2,'v2,'k2>) -> bool { return !ref_eq(a, b); }")

		/**
		 * A generic pre-order increment operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPreInc,  "gen_pre_inc",  "(v : ref<'a,f,'v,'k>)->'a { v=*v+lit(\"1\":'a); return *v; }")

		/**
		 * A generic post-order increment operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPostInc, "gen_post_inc", "(v : ref<'a,f,'v,'k>)->'a { auto tmp=*v; v=*v+lit(\"1\":'a); return tmp; }")

		/**
		 * A generic pre-order decrement operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPreDec,  "gen_pre_dec",  "(v : ref<'a,f,'v,'k>)->'a { v=*v-lit(\"1\":'a); return *v; }")

		/**
		 * A generic post-order decrement operator.
		 */
		LANG_EXT_DERIVED_WITH_NAME(GenPostDec, "gen_post_dec", "(v : ref<'a,f,'v,'k>)->'a { auto tmp=*v; v=*v-lit(\"1\":'a); return tmp; }")


	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle reference types in a more user
	 * friendly way then its raw encoding.
	 */
	class ReferenceType {

	public:

		/**
		 * An enumeration of the supported reference types.
		 */
		enum class Kind {
			Plain, CppReference, CppRValueReference, Qualified, Undefined
		};

	private:

		/**
		 * The type of data stored in the referenced memory location.
		 */
		TypePtr elementType;

		/**
		 * A marker indicating whether the referenced memory cell can be modified through this reference or not (const).
		 */
		TypePtr mConst;

		/**
		 * A marker indicating whether the referenced memory cell might be concurrently modified or not.
		 */
		TypePtr mVolatile;

		/**
		 * The kind of reference this type is representing.
		 */
		TypePtr kind;

		/**
		 * Creates a new reference type based on the given parameters.
		 */
		ReferenceType(const TypePtr& elementType, const TypePtr& mConst, const TypePtr& mVolatile, const TypePtr& kind)
			: elementType(elementType), mConst(mConst), mVolatile(mVolatile), kind(kind) {}

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
		static GenericTypePtr create(const TypePtr& elementType, bool _const = false, bool _volatile = false, const Kind& kind = Kind::Plain);

		static GenericTypePtr create(const TypePtr& elementType, const TypePtr& _const, const TypePtr& _volatile, const Kind& kind = Kind::Plain);

		// an implicit converter of this wrapper to an IR type
		operator GenericTypePtr() const {
			return toType();
		}

		// --- observers and mutators ---

		const TypePtr& getElementType() const {
			return elementType;
		}

		void setElementType(const TypePtr& type) {
			assert_true(type);
			elementType = type;
		}

		bool isConst() const;

		void setConst(bool newState = true);

		bool isVolatile() const;

		void setVolatile(bool newState = true);

		Kind getKind() const;

		void setKind(const Kind& kind);

		bool isPlain() const;

		bool isCppReference() const;

		bool isCppRValueReference() const;

		bool isQualified() const;

		GenericTypePtr toType() const;

	};

	/**
	 * Determines whether a given node is a reference type or an expression of a reference type of any kind.
	 */
	bool isReference(const NodePtr& node);

	/**
	 * Determines whether a given node is a reference type or an expression of a reference type to a certain kind.
	 */
	bool isReferenceTo(const NodePtr& node, const TypePtr& type);

	/**
	 * Determines whether a given node is a reference type or an expression of a plain reference type.
	 */
	bool isPlainReference(const NodePtr& node);

	/**
	 * Determines whether a given node is a reference type or an expression of a C++ reference type.
	 */
	bool isCppReference(const NodePtr& node);

	/**
	 * Determines whether a given node is a reference type or an expression of a C++ R-Value reference type.
	 */
	bool isCppRValueReference(const NodePtr& node);

	/**
	 * Determines whether a given node is a qualified type or an expression of a qualified type.
	 */
	bool isQualifiedReference(const NodePtr& node);

	/**
	 * Determines whether a given node is an assignment operation.
	 */
	bool isAssignment(const NodePtr& node);

	/**
	 * Determines the reference kind represented by the given input type literal
	 */
	ReferenceType::Kind getReferenceKind(const TypePtr& typeLitType);
	ReferenceType::Kind getReferenceKind(const ExpressionPtr& expression);

	/**
	 * A factory function creating a reference type utilizing the given element type and flag combination.
	 */
	TypePtr buildRefType(const TypePtr& elementType, bool _const = false, bool _volatile = false, const ReferenceType::Kind& kind = ReferenceType::Kind::Plain);

	bool doReferenceQualifiersDiffer(const TypePtr& typeA, const TypePtr& typeB);

	bool doReferencesDifferOnlyInQualifiers(const TypePtr& typeA, const TypePtr& typeB);

	ExpressionPtr buildRefDeref(const ExpressionPtr& refExpr);

	ExpressionPtr buildRefCast(const ExpressionPtr& refExpr, const TypePtr& targetTy);
	ExpressionPtr buildRefKindCast(const ExpressionPtr& refExpr, ReferenceType::Kind newKind);
	ExpressionPtr buildRefParentCast(const ExpressionPtr& refExpr, const TypePtr& targetTy);
	ExpressionPtr buildRefReinterpret(const ExpressionPtr& refExpr, const TypePtr& targetTy);

	bool isAnyRefCast(const NodePtr& node);
	ExpressionPtr removeSurroundingRefCasts(const ExpressionPtr& node);

	ExpressionPtr buildRefTemp(const TypePtr& type);
	ExpressionPtr buildRefNull(const TypePtr& type);
	ExpressionPtr buildRefDecl(const TypePtr& type);

} // end namespace lang
} // end namespace core
} // end namespace insieme
