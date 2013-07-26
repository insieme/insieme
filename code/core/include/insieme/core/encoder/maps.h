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

#include <utility>

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/encoder/pairs.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace encoder {

	using std::map;

	// -- maps representation within the IR --

	namespace detail {

		//  -- some utility functors --


		/**
		 * A generic functor creating the IR type of an encoded map.
		 *
		 * @tparam K the key type of the map to be encoded
		 * @tparam V the value type of the map to be encoded
		 * @tparam CK the converter used for encoding the key type
		 * @tparam CV the converter used for encoding the value type
		 */
		template<typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
		struct create_map_type {
			core::TypePtr operator()(NodeManager& manager) const {
				// use combination of list and pair converter
				return typename ListConverter<pair<K,V>, PairConverter<K,V,CK,CV>>::type_factory()(manager);
			}
		};

		/**
		 * A generic functor testing whether a given expression is a valid
		 * encoding of a pair of element types F and S.
		 *
		 * @tparam K the key type of the map to be encoded
		 * @tparam V the value type of the map to be encoded
		 * @tparam CK the converter used for encoding the key type
		 * @tparam CV the converter used for encoding the value type
		 */
		template<typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
		struct is_map {
			bool operator()(const core::ExpressionPtr& expr) const {
				// use combination of list and pair converter
				return typename ListConverter<pair<K,V>, PairConverter<K,V,CK,CV>>::is_encoding_of()(expr);
			}
		};

		/**
		 * A generic functor encoding a map of elements into an IR expression.
		 *
		 * @tparam K the key type of the map to be encoded
		 * @tparam V the value type of the map to be encoded
		 * @tparam CK the converter used for encoding the key type
		 * @tparam CV the converter used for encoding the value type
		 */
		template<typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
		struct encode_map {

			core::ExpressionPtr operator()(NodeManager& manager, const map<K,V>& map) const {
				// convert map into list of pairs
				vector<pair<K,V>> list;
				for(auto cur : map) { list.push_back(cur); }

				// use combination of list and pair converter
				return typename ListConverter<pair<K,V>, PairConverter<K,V,CK,CV>>::value_to_ir_converter()(manager, list);
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
		template<typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
		struct decode_map {

			map<K,V> operator()(const core::ExpressionPtr& expr) const {
				assert((is_map<K,V,CK,CV>()(expr)) && "Can only convert maps to maps!");

				// convert map into list
				vector<pair<K,V>> list = typename ListConverter<pair<K,V>, PairConverter<K,V,CK,CV>>::ir_to_value_converter()(expr);

				// convert list into a map
				map<K,V> res;
				for(auto cur : list) {
					res.insert(cur);
				}
				return res;
			}
		};


	}

	// define encoder / decoder according to the encoder framework

	/**
	 * Defines a pair converter functor allowing to customize the encoding of the element type.
	 *
	 * @tparam K the key type of the map to be encoded
	 * @tparam V the value type of the map to be encoded
	 * @tparam CK the converter used for encoding the key type
	 * @tparam CV the converter used for encoding the value type
	 */
	template<typename K, typename V, typename CK = Converter<K>, typename CV = Converter<V>>
	struct MapConverter : public Converter<map<K,V>, detail::create_map_type<K,V,CK,CV>, detail::encode_map<K,V,CK,CV>, detail::decode_map<K,V,CK,CV>, detail::is_map<K,V,CK,CV>> {};

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of maps using default element type converters.
	 */
	template<typename K, typename V>
	struct type_factory<map<K,V>> : public detail::create_map_type<K,V> {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of maps using default element type converters.
	 */
	template<typename K, typename V>
	struct value_to_ir_converter<map<K,V>> : public detail::encode_map<K,V> {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of maps using default element type converters.
	 */
	template<typename K, typename V>
	struct ir_to_value_converter<map<K,V>> : public detail::decode_map<K,V> {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of maps using default element type converters.
	 */
	template<typename K, typename V>
	struct is_encoding_of<map<K,V>> : public detail::is_map<K,V> { };


} // end namespace encoder
} // end namespace core
} // end namespace insieme
