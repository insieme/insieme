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


	// --------------------- Extension ----------------------------

	/**
	 * An extension covering two types to model boolean properties.
	 */
	class BooleanMarkerExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		BooleanMarkerExtension(core::NodeManager& manager)
			: core::lang::Extension(manager) {}

	public:

		LANG_EXT_TYPE_WITH_NAME(True, "true_marker", "t");

		LANG_EXT_TYPE_WITH_NAME(False, "false_marker", "f");

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
		ReferenceExtension(core::NodeManager& manager)
			: core::lang::Extension(manager) {}

	public:


		// ------------------ memory location ------------------------

//		/**
//		 * A type for a set of memory location qualifiers.
//		 */
//		LANG_EXT_TYPE(MemLoc, "memloc")
//
//		/**
//		 * A token representing the stack.
//		 */
//		LANG_EXT_LITERAL(MemLocStack, "memloc_stack", "memloc")
//
//		/**
//		 * A token representing the heap.
//		 */
//		LANG_EXT_LITERAL(MemLocHeap, "memloc_heap", "memloc")


		// -------------------- references ---------------------------

		/**
		 * The generic type template e.g. utilized as a reference for is-ref checks.
		 */
		LANG_EXT_TYPE_WITH_NAME(GenRef, "generic_ref_template", "ref<'a,'const,'volatile>");

//		/**
//		 * A literal for allocating memory.
//		 */
//		LANG_EXT_LITERAL(RefAlloc,    	"ref_alloc",    		"(type<'a>, memloc) -> ref<'a,f,f>")

		/*
		LITERAL(RefNull,        "ref_null",             "ref<any>")
		LITERAL(NullFunc,       "func_null",            "(type<'a>)->'a")
		LITERAL(RefDelete, 		"ref_delete", 			"(ref<'a>) -> unit")
		LITERAL(RefAssign, 		"ref_assign", 			"(sink<'a>,'a) -> unit")
		LITERAL(RefDeref,  		"ref_deref",  			"(src<'a>) -> 'a")

		LITERAL(SrcToRef,		"src_to_ref",			"(src<'a>) -> ref<'a>")
		LITERAL(SinkToRef,		"sink_to_ref",			"(sink<'a>) -> ref<'a>")

		LITERAL(RefToInt,		"ref_to_int",			"(ref<'a>) -> uint<8>")
		LITERAL(IntToRef,		"int_to_ref",			"(int<16>, type<'a>) -> ref<'a>")

		DERIVED(RefVar,  			"ref_var",    	    "lambda ('a v) -> ref<'a> { decl auto r = ref_alloc(  type_of(v), memloc_stack); r = v; return r; }")
		DERIVED(RefNew,  			"ref_new",    	    "lambda ('a v) -> ref<'a> { decl auto r = ref_alloc(  type_of(v), memloc_heap ); r = v; return r; }")
		DERIVED(RefLoc,  			"ref_loc",    	    "lambda ('a v) -> ref<'a> { decl auto r = ref_alloc(  type_of(v), memloc_local); r = v; return r; }")
		DERIVED(RefIsNull,   		"ref_is_null",	    "lambda (ref<'a> r) -> bool { return ref_eq(r, ref_null); }")
		DERIVED(FuncIsNull,   		"func_is_null",	    "lambda ('a f) -> bool { return f == func_null(lit('a)); }")

		GROUP(AllocOp, RefVar, RefNew, RefLoc)

		LITERAL(RefReinterpret, "ref_reinterpret",      "(ref<'a>, type<'b>) -> ref<'b>")
		LITERAL(RefNarrow, 		"ref_narrow",			"(ref<'a>, datapath, type<'b>) -> ref<'b>")
		LITERAL(RefExpand,		"ref_expand",			"(ref<'a>, datapath, type<'b>) -> ref<'b>")

		LITERAL(SrcReinterpret, "src_reinterpret",      "(src<'a>, type<'b>) -> src<'b>")
		LITERAL(SrcNarrow, 		"src_narrow",			"(src<'a>, datapath, type<'b>) -> src<'b>")
		LITERAL(SrcExpand,		"src_expand",			"(src<'a>, datapath, type<'b>) -> src<'b>")

		LITERAL(RefToSrc, "ref_src_cast",      "(ref<'a>) -> src<'a>")

		 */

	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle reference types in a more user
	 * friendly way then its raw encoding.
	 */
	class ReferenceType {

		TypePtr elementType;

		bool mConst;

		bool mVolatile;

		ReferenceType(const TypePtr& elementType, bool mConst, bool mVolatile)
			: elementType(elementType), mConst(mConst), mVolatile(mVolatile) {}

	public:

		ReferenceType(const NodePtr& node);

		ReferenceType(const ReferenceType&) = default;
		ReferenceType(ReferenceType&&) = default;

		ReferenceType& operator=(const ReferenceType&) = default;
		ReferenceType& operator=(ReferenceType&&) = default;


		// --- utilities ---

		static bool isReferenceType(const NodePtr& node);

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


	static inline bool isReference(const NodePtr& node) {
		if (auto expr = node.isa<ExpressionPtr>()) {
			return isReference(expr->getType());
		}
		return node && ReferenceType::isReferenceType(node);
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme
