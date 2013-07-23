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

#pragma once

#include <vector>

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/extension.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {
namespace encoder {

	using std::vector;

	// -- list representation within the IR --

	/**
	 * A language extension introducing the primitives required to encode lists of elements.
	 * Lists are represented within the IR using cons and empty tokens.
	 */
	class ListExtension : public core::lang::Extension {

	public:

		/**
		 * The family name of the list data type.
		 */
		static string LIST_TYPE_NAME;

		/**
		 * The empty literal of type (type<'a>)->list<'a> allowing the creation of an
		 * empty list of a certain type.
		 */
		const core::LiteralPtr empty;

		/**
		 * The cons operator of type ('a,list<'a>)->list<'a> extending a given list
		 * by an additional head-element.
		 */
		const core::LiteralPtr cons;

	private:

		/**
		 * Language extensions should only be constructed by the node manager.
		 */
		friend class core::NodeManager;

		/**
		 * A constructor for this language extension creating a new instance based on the
		 * given node manager.
		 *
		 * @param manager the manager to base all node types within resulting extension on
		 */
		ListExtension(core::NodeManager& manager);

	};

	/**
	 * A utility allowing to test whether the given type is a list type.
	 *
	 * @param type the type to be tested
	 */
	bool isListType(const core::TypePtr& type);

	/**
	 * A utility allowing to obtain the element type of the given list type.
	 *
	 * @param listType the type representing a list type from which the element type should be extracted
	 * @return a reference to the contained element type
	 */
	const core::TypePtr getElementType(const core::TypePtr& listType);

	/**
	 * Creates a list type maintaining elements of the given element type.
	 *
	 * @param elementType the type to be maintained by the resulting list type.
	 * @return the requested list type maintained by the same manager as the given element type.
	 */
	const core::TypePtr getListType(const core::TypePtr& elementType);



	namespace detail {

		//  -- some utility functors --


		/**
		 * A generic functor creating the IR type of an encoded list.
		 *
		 * @tparam E the element value type of the list to be encoded
		 * @tparam C the converter used for encoding the list element type
		 */
		template<typename E, typename C = Converter<E>>
		struct create_list_type {
			core::TypePtr operator()(NodeManager& manager) const {
				return GenericType::get(manager, ListExtension::LIST_TYPE_NAME, toVector(typename C::type_factory()(manager)));
			}
		};

		/**
		 * A generic functor testing whether a given expression is a valid
		 * encoding of a vector of type E.
		 *
		 * @tparam E the value type of the vector expected to be encoded
		 * @tparam C the expected converter used to encode the vector
		 */
		template<typename E, typename C = Converter<E>>
		struct is_list {
			bool operator()(const core::ExpressionPtr& expr) const {
				const ListExtension& ext = expr->getNodeManager().getLangExtension<ListExtension>();

				// check step case
				if (expr->getNodeType() != core::NT_CallExpr) {
					return false;
				}

				// check the call
				CallExprPtr call = static_pointer_cast<const core::CallExpr>(expr);

				// lists can only be composed using cons and empty
				const auto& fun = call->getFunctionExpr();
				if (*fun == *ext.empty) {
					return true;
				}
				if (*fun == *ext.cons) {
					// check element type encoding + rest of the list
					return typename C::is_encoding_of()(call->getArgument(0)) && (*this)(call->getArgument(1));
				}

				// it is not a list
				return false;
			}
		};

		/**
		 * A generic functor encoding a vector of E elements into an IR expression.
		 *
		 * @tparam E the value type of the vector to be encoded
		 * @tparam C the converter used for encoding element types
		 */
		template<typename E, typename C=Converter<E>>
		struct encode_list {

			core::ExpressionPtr operator()(NodeManager& manager, const vector<E>& list) const {

				// obtain some useful values
				create_list_type<E,C> factory;
				core::TypePtr listType = factory(manager);
				core::TypePtr elementType = getTypeFor<E, C>(manager);

				IRBuilder builder(manager);
				const ListExtension& ext = manager.getLangExtension<ListExtension>();

				// create terminal token
				core::ExpressionPtr typeToken = builder.getTypeLiteral(elementType);
				core::ExpressionPtr res = builder.callExpr(listType, ext.empty, toVector(typeToken));

				// append remaining tokens back to front
				for(auto it = list.rbegin(); it != list.rend(); ++it) {
					core::ExpressionPtr head = toIR<E,C>(manager, *it);
					res = builder.callExpr(listType, ext.cons, head, res);
				}

				return res;
			}
		};

		/**
		 * A specialized version of the encode_list struct capable of encoding lists of
		 * expression pointers.
		 */
		template<>
		struct encode_list<ExpressionPtr, DirectExprConverter> {

			core::ExpressionPtr operator()(NodeManager& manager, const vector<ExpressionPtr>& list) const {

				// obtain some useful values
				core::TypePtr elementType = (list.empty()) ? manager.getLangBasic().getUnit() : list[0].getType();
				core::TypePtr listType = GenericType::get(manager, ListExtension::LIST_TYPE_NAME, toVector(elementType));

				IRBuilder builder(manager);
				const ListExtension& ext = manager.getLangExtension<ListExtension>();

				// create terminal token
				core::ExpressionPtr typeToken = builder.getTypeLiteral(elementType);
				core::ExpressionPtr res = builder.callExpr(listType, ext.empty, toVector(typeToken));

				// append remaining tokens back to front
				for(auto it = list.rbegin(); it != list.rend(); ++it) {
					core::ExpressionPtr head = toIR<ExpressionPtr,DirectExprConverter>(manager, *it);
					res = builder.callExpr(listType, ext.cons, head, res);
				}

				return res;
			}
		};

		/**
		 * A generic functor decoding a vector of E elements into an IR expression.
		 *
		 * @tparam E the value type of the vector to be decoded
		 * @tparam C the converter used for decoding element types
		 */
		template<typename E, typename C = Converter<E>>
		struct decode_list {

			/**
			 * A end-recursive implementation of the decoding operation.
			 *
			 * @param list the expression to be decoded
			 * @param res the resulting vector (will be incrementally constructed)
			 */
			vector<E>& toList(const core::ExpressionPtr& list, vector<E>& res) const {
				const ListExtension& ext = list->getNodeManager().getLangExtension<ListExtension>();

				CallExprPtr call = static_pointer_cast<const CallExpr>(list);
				if (*call->getFunctionExpr() == *ext.empty) {
					return res;
				}
				res.push_back(toValue<E,C>(call->getArgument(0)));
				return toList(call->getArgument(1), res);
			}

			vector<E> operator()(const core::ExpressionPtr& list) const {
				assert((is_list<E,C>()(list)) && "Can only convert lists to lists!");
				vector<E> res;
				toList(list, res);
				return res;
			}
		};


	}

	// define encoder / decoder according to the encoder framework

	/**
	 * Defines a list converter functor allowing to customize the encoding of the element type.
	 *
	 * @tparam E the element type within the list
	 * @tparam C the converter to be used for encoding element types
	 */
	template<typename E, typename C = Converter<E>>
	struct ListConverter : public Converter<vector<E>, detail::create_list_type<E,C>, detail::encode_list<E,C>, detail::decode_list<E,C>, detail::is_list<E,C>> {};

	/**
	 * Defines a list converter functor customized to encode vectors of expressions directly into lists
	 * of expressions within the IR without wrapping up expressions.
	 */
	struct DirectExprListConverter : public ListConverter<ExpressionPtr, DirectExprConverter> {};

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of vectors using default element type converters.
	 */
	template<typename E>
	struct type_factory<vector<E>> : public detail::create_list_type<E> {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of vectors using default element type converters.
	 */
	template<typename E>
	struct value_to_ir_converter<vector<E>> : public detail::encode_list<E> {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of vectors using default element type converters.
	 */
	template<typename E>
	struct ir_to_value_converter<vector<E>> : public detail::decode_list<E> {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of vectors using default element type converters.
	 */
	template<typename E>
	struct is_encoding_of<vector<E>> : public detail::is_list<E> { };



	// ---- make the direct expression encoding the default for the type vector<ExpressionPtr>

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of expression lists.
	 */
	template<>
	struct type_factory<vector<ExpressionPtr>> : public detail::create_list_type<ExpressionPtr, DirectExprConverter> {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of expression lists.
	 */
	template<>
	struct value_to_ir_converter<vector<ExpressionPtr>> : public detail::encode_list<ExpressionPtr, DirectExprConverter> {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of expression lists.
	 */
	template<>
	struct ir_to_value_converter<vector<ExpressionPtr>> : public detail::decode_list<ExpressionPtr, DirectExprConverter> {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of expression lists.
	 */
	template<>
	struct is_encoding_of<vector<ExpressionPtr>> : public detail::is_list<ExpressionPtr, DirectExprConverter> { };


} // end namespace encoder
} // end namespace core
} // end namespace insieme
