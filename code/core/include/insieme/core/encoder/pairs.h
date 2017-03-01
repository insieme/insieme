/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <utility>

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/extension.h"


namespace insieme {
namespace core {
namespace encoder {

	using std::pair;

	// -- pair representation within the IR --

	/**
	 * A language extension introducing the primitives required to encode pairs of elements.
	 */
	class PairExtension : public core::lang::Extension {
	  public:
		/**
		 * The family name of the pair data type.
		 */
		static string PAIR_TYPE_NAME;

		/**
		 * The pair operator of type ('a,'b)->pair<'a,'b> combining two values
		 * to typesafe a pair of values.
		 */
		const core::LiteralPtr pair;

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
		PairExtension(core::NodeManager& manager);
	};

	/**
	 * A utility allowing to test whether the given type is a pair type.
	 *
	 * @param type the type to be tested
	 */
	bool isPairType(const core::TypePtr& type);

	/**
	 * A utility allowing to obtain the first element type of the given pair type.
	 *
	 * @param pairType the type representing a pair type from which the element type should be extracted
	 * @return a reference to the contained element type
	 */
	const core::TypePtr getFirstElementType(const core::TypePtr& pairType);

	/**
	 * A utility allowing to obtain the second element type of the given pair type.
	 *
	 * @param pairType the type representing a pair type from which the element type should be extracted
	 * @return a reference to the contained element type
	 */
	const core::TypePtr getSecondElementType(const core::TypePtr& pairType);

	/**
	 * Creates a pair type maintaining elements of the given element types.
	 *
	 * @param first the type of the first element within the resulting pair type
	 * @param second the type of the second element within the resulting pair type
	 * @return the requested pair type maintained by the same manager as the first element type.
	 */
	const core::TypePtr getPairType(const core::TypePtr& first, const core::TypePtr& second);


	namespace detail {

		//  -- some utility functors --


		/**
		 * A generic functor creating the IR type of an encoded pair.
		 *
		 * @tparam F the first element type of the list to be encoded
		 * @tparam S the second element type of the list to be encoded
		 * @tparam CF the converter used for encoding the first element type
		 * @tparam CS the converter used for encoding the second element type
		 */
		template <typename F, typename S, typename CF = Converter<F>, typename CS = Converter<S>>
		struct create_pair_type {
			core::TypePtr operator()(NodeManager& manager) const {
				return GenericType::get(manager, PairExtension::PAIR_TYPE_NAME,
				                        toVector(typename CF::type_factory()(manager), typename CS::type_factory()(manager)));
			}
		};

		/**
		 * A generic functor testing whether a given expression is a valid
		 * encoding of a pair of element types F and S.
		 *
		 * @tparam F the first element type of the list to be encoded
		 * @tparam S the second element type of the list to be encoded
		 * @tparam CF the converter used for encoding the first element type
		 * @tparam CS the converter used for encoding the second element type
		 */
		template <typename F, typename S, typename CF = Converter<F>, typename CS = Converter<S>>
		struct is_pair {
			bool operator()(const core::ExpressionPtr& expr) const {
				const PairExtension& ext = expr->getNodeManager().getLangExtension<PairExtension>();

				// check step case
				if(expr->getNodeType() != core::NT_CallExpr) { return false; }

				// check the call
				CallExprPtr call = expr.as<CallExprPtr>();

				// lists can only be composed using cons and empty
				const auto& fun = call->getFunctionExpr();
				if(*fun == *ext.pair) {
					// check encoding of element types
					return typename CF::is_encoding_of()(call->getArgument(0)) && typename CS::is_encoding_of()(call->getArgument(1));
				}

				// it is not a valid pair
				return false;
			}
		};

		/**
		 * A generic functor encoding a pair of elements into an IR expression.
		 *
		 * @tparam F the first element type of the list to be encoded
		 * @tparam S the second element type of the list to be encoded
		 * @tparam CF the converter used for encoding the first element type
		 * @tparam CS the converter used for encoding the second element type
		 */
		template <typename F, typename S, typename CF = Converter<F>, typename CS = Converter<S>>
		struct encode_pair {
			core::ExpressionPtr operator()(NodeManager& manager, const pair<F, S>& pair) const {
				// obtain some useful values
				create_pair_type<F, S, CF, CS> factory;
				core::TypePtr pairType = factory(manager);

				IRBuilder builder(manager);
				const PairExtension& ext = manager.getLangExtension<PairExtension>();

				// create encoding of pair
				return builder.callExpr(pairType, ext.pair, toIR<F, CF>(manager, pair.first), toIR<S, CS>(manager, pair.second));
			}
		};

		/**
		 * A generic functor decoding a pair of elements from an IR expression.
		 *
		 * @tparam F the first element type of the list to be encoded
		 * @tparam S the second element type of the list to be encoded
		 * @tparam CF the converter used for encoding the first element type
		 * @tparam CS the converter used for encoding the second element type
		 */
		template <typename F, typename S, typename CF = Converter<F>, typename CS = Converter<S>>
		struct decode_pair {
			pair<F, S> operator()(const core::ExpressionPtr& pair) const {
				assert((is_pair<F, S, CF, CS>()(pair)) && "Can only convert pairs to pairs!");
				CallExprPtr call = pair.as<CallExprPtr>();
				return std::make_pair(toValue<F, CF>(call->getArgument(0)), toValue<S, CS>(call->getArgument(1)));
			}
		};
	}

	// define encoder / decoder according to the encoder framework

	/**
	 * Defines a pair converter functor allowing to customize the encoding of the element type.
	 *
	 * @tparam F the first element type of the list to be encoded
	 * @tparam S the second element type of the list to be encoded
	 * @tparam CF the converter used for encoding the first element type
	 * @tparam CS the converter used for encoding the second element type
	 */
	template <typename F, typename S, typename CF = Converter<F>, typename CS = Converter<S>>
	struct PairConverter : public Converter<pair<F, S>, detail::create_pair_type<F, S, CF, CS>, detail::encode_pair<F, S, CF, CS>,
	                                        detail::decode_pair<F, S, CF, CS>, detail::is_pair<F, S, CF, CS>> {};

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of pairs using default element type converters.
	 */
	template <typename F, typename S>
	struct type_factory<pair<F, S>> : public detail::create_pair_type<F, S> {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of pairs using default element type converters.
	 */
	template <typename F, typename S>
	struct value_to_ir_converter<pair<F, S>> : public detail::encode_pair<F, S> {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of pairs using default element type converters.
	 */
	template <typename F, typename S>
	struct ir_to_value_converter<pair<F, S>> : public detail::decode_pair<F, S> {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of pairs using default element type converters.
	 */
	template <typename F, typename S>
	struct is_encoding_of<pair<F, S>> : public detail::is_pair<F, S> {};


} // end namespace encoder
} // end namespace core
} // end namespace insieme
