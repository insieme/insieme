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
 */

#pragma once

#include <utility>

#include "insieme/core/encoder/maps.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace encoder {

	// -- pointer maps representation within the IR --

	namespace detail {

		//  -- some utility functors --

		/**
		 * A generic functor encoding a map of elements into an IR expression.
		 *
		 * @tparam K the key type of the map to be encoded
		 * @tparam V the value type of the map to be encoded
		 * @tparam CK the converter used for encoding the key type
		 * @tparam CV the converter used for encoding the value type
		 */
		template <typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
		struct encode_pointer_map {
			core::ExpressionPtr operator()(NodeManager& manager, const utils::map::PointerMap<K, V>& map) const {
				// convert map into list of pairs
				vector<pair<K, V>> list;
				for(auto cur : map) {
					list.push_back(cur);
				}

				// use combination of list and pair converter
				return typename ListConverter<pair<K, V>, PairConverter<K, V, CK, CV>>::value_to_ir_converter()(manager, list);
			}
		};

		/**
		 * A generic functor decoding a pair of elements from an IR expression.
		 *
		 * @tparam K the key type of the map to be encoded
		 * @tparam V the value type of the map to be encoded
		 * @tparam CK the converter used for encoding the key type
		 * @tparam CV the converter used for encoding the value type
		 */
		template <typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
		struct decode_pointer_map {
			utils::map::PointerMap<K, V> operator()(const core::ExpressionPtr& expr) const {
				assert((is_map<K, V, CK, CV>()(expr)) && "Can only convert maps to maps!");

				// convert map into list
				vector<pair<K, V>> list = typename ListConverter<pair<K, V>, PairConverter<K, V, CK, CV>>::ir_to_value_converter()(expr);

				// convert list into a map
				utils::map::PointerMap<K, V> res;
				for(auto cur : list) {
					res.insert(cur);
				}
				return res;
			}
		};
	}

	// and pointer maps

	/**
	 * Defines a pair converter functor allowing to customize the encoding of the element type.
	 *
	 * @tparam K the key type of the map to be encoded
	 * @tparam V the value type of the map to be encoded
	 * @tparam CK the converter used for encoding the key type
	 * @tparam CV the converter used for encoding the value type
	 */
	template <typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
	struct PointerMapConverter : public Converter<utils::map::PointerMap<K, V>, detail::create_map_type<K, V, CK, CV>, detail::encode_pointer_map<K, V, CK, CV>,
	                                              detail::decode_pointer_map<K, V, CK, CV>, detail::is_map<K, V, CK, CV>> {};

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of maps using default element type converters.
	 */
	template <typename K, typename V>
	struct type_factory<utils::map::PointerMap<K, V>> : public detail::create_map_type<K, V> {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of maps using default element type converters.
	 */
	template <typename K, typename V>
	struct value_to_ir_converter<utils::map::PointerMap<K, V>> : public detail::encode_pointer_map<K, V> {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of maps using default element type converters.
	 */
	template <typename K, typename V>
	struct ir_to_value_converter<utils::map::PointerMap<K, V>> : public detail::decode_pointer_map<K, V> {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of maps using default element type converters.
	 */
	template <typename K, typename V>
	struct is_encoding_of<utils::map::PointerMap<K, V>> : public detail::is_map<K, V> {};


} // end namespace encoder
} // end namespace core
} // end namespace insieme
