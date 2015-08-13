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
	 * An extension covering the abstract array type and all its
	 * associated operators.
	 */
	class ArrayExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ArrayExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		// -------------------- arrays ---------------------------

		/**
		 * The generic array type template e.g. utilized as a reference for is-array checks.
		 */
		LANG_EXT_TYPE_WITH_NAME(GenArray, "generic_array_template", "array<'a,'s>");

		/**
		 * A literal to create a (partially) initialized array instance.
		 */
		LANG_EXT_LITERAL(ArrayCreate, "array_create", "(type<'elem>, type<'size>, list<'elem>) -> array<'elem,'size>")


		//		// Arrays -------------------------------------------------------------------------------------------------------------
		//
		//		GROUP(ArrayOp, ArrayCreate1D, ArrayCreateND, ArraySubscript1D, ArraySubscriptND, ArrayRefElem1D, ArrayRefElemND, ArrayRefProjection1D,
		// ArrayRefProjectionND)
		//
		//		LITERAL(ArrayCreate1D, 		"array_create_1D", 		"(type<'elem>, uint<8>) -> array<'elem,1>")
		//		LITERAL(ArrayCreateND, 		"array_create_ND", 		"(type<'elem>, vector<uint<8>,'n>) -> array<'elem,'n>")
		//
		//		LITERAL(ArraySubscript1D,      "array_subscript_1D",   "(array<'elem,1>, uint<8>) -> 'elem")
		//		LITERAL(ArraySubscriptND,      "array_subscript_ND",   "(array<'elem,'n>, vector<uint<8>,'n>) -> 'elem")
		//
		//		DERIVED(ArrayRefElem1D,     "array_ref_elem_1D",    "lambda (ref<array<'elem,1>> a, uint<8> i) -> ref<'elem> { return ref_narrow(a,
		// dp_element(dp_root, i), lit('elem)); }")
		//		LITERAL(ArrayRefElemND, 	"array_ref_elem_ND", 	"(ref<array<'elem,'n>>, vector<uint<8>,'n>) -> ref<'elem>")
		//
		//		LITERAL(ArrayRefProjection1D, "array_ref_projection_1D",
		//									  "(ref<array<'elem,1>>,uint<8>,uint<8>) -> ref<array<'elem,1>>")
		//		LITERAL(ArrayRefProjectionND, "array_ref_projection_ND",
		//									  "(ref<array<'elem,'n>>,vector<uint<8>,'n>,vector<uint<8>,'n>) -> ref<array<'elem,'n>>")
		//
		//		LITERAL(ArrayRefDistance, 		"array_ref_distance", 	"(ref<array<'elem,1>>, ref<array<'elem,1>>) -> int<8>")
		//		DERIVED(ScalarToArray, 			"scalar_to_array", 		"lambda (ref<'a> a) -> ref<array<'a,1>> { return ref_expand(a, dp_element(dp_root,0u),
		// lit(array<'a,1>)); }")
		//
		//		DERIVED(ArrayReduce, "array_reduce",
		//			"	lambda (ref<array<'a,1>> data, uint<8> size, ('b,'a)->'b op, 'b init)->'b {"
		//			"		decl ref<'b> res = var(init);"
		//			"		for(uint<8> i = 0ul .. size) {"
		//			"			res = op(*res, *(data[i]));"
		//			"		}"
		//			"		return *res;"
		//			"	}")
		//
		//		// Arrays and Vectors -------------------------------------------------------------------------------------------------
		//
		//		GROUP(SubscriptOperator, ArraySubscript1D, ArraySubscriptND, VectorSubscript, ArrayRefElem1D, ArrayRefElemND, VectorRefElem)
	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle reference types in a more user
	 * friendly way then its raw encoding.
	 */
	class ArrayType {
		TypePtr elementType;

		ExpressionPtr size;

		ArrayType(const TypePtr& elementType, const ExpressionPtr& size) : elementType(elementType), size(size) {}

	  public:
		ArrayType(const NodePtr& node);

		ArrayType(const ArrayType&) = default;
		ArrayType(ArrayType&&) = default;

		ArrayType& operator=(const ArrayType&) = default;
		ArrayType& operator=(ArrayType&&) = default;


		// --- utilities ---

		static bool isArrayType(const NodePtr& node);

		static bool isFixedSizedArrayType(const NodePtr& node);

		static bool isVariableSizedArrayType(const NodePtr& node);

		static bool isUnknownSizedArrayType(const NodePtr& node);

		static GenericTypePtr create(const TypePtr& elementType, const ExpressionPtr& size = ExpressionPtr());

		operator GenericTypePtr() const;

		// --- observers and mutators ---

		const TypePtr& getElementType() const {
			return elementType;
		}

		void setElementType(const TypePtr& type) {
			assert_true(type) << "Type must not be null!";
			elementType = type;
		}

		const ExpressionPtr& getSize() const {
			return size;
		}

		void setSize(const LiteralPtr& size) {
			this->size = size;
		}

		void setSize(const VariablePtr& size) {
			this->size = size;
		}

		bool isConstSize() const {
			return size.isa<LiteralPtr>();
		}

		bool isVariableSize() const {
			return !isConstSize();
		}
	};

	// --------------------- utilities -------------------

	static inline bool isArray(const NodePtr& node) {
		if(auto expr = node.isa<ExpressionPtr>()) { return isArray(expr->getType()); }
		return node && ArrayType::isArrayType(node);
	}

	static inline bool isFixedSizedArray(const NodePtr& node) {
		if(auto expr = node.isa<ExpressionPtr>()) { return isFixedSizedArray(expr->getType()); }
		return node && ArrayType::isFixedSizedArrayType(node);
	}

	static inline bool isVariableSizedArray(const NodePtr& node) {
		if(auto expr = node.isa<ExpressionPtr>()) { return isVariableSizedArray(expr->getType()); }
		return node && ArrayType::isVariableSizedArrayType(node);
	}

	static inline bool isUnknownSizedArray(const NodePtr& node) {
		if(auto expr = node.isa<ExpressionPtr>()) { return isUnknownSizedArray(expr->getType()); }
		return node && ArrayType::isUnknownSizedArrayType(node);
	}

	static inline TypePtr getArrayElementType(const NodePtr& node) {
		assert_true(isArray(node));
		if(auto expr = node.isa<ExpressionPtr>()) { return getArrayElementType(expr->getType()); }
		return ArrayType(node.as<GenericTypePtr>()).getElementType();
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
