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

#include <utility>

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/encoder/lists.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace encoder {

	using std::set;

	// -- sets representation within the IR --

	namespace detail {

		//  -- some utility functors --


		/**
		 * A generic functor creating the IR type of an encoded set.
		 *
		 * @tparam K the key type of the set to be encoded
		 * @tparam CK the converter used for encoding the key type
		 */
		template <typename K, typename CK = Converter<K>>
		struct create_set_type {
			core::TypePtr operator()(NodeManager& manager) const {
				// use list converter
				return typename ListConverter<K, Converter<K>>::type_factory()(manager);
			}
		};

		/**
		 * A generic functor testing whether a given expression is a valid
		 * encoding of a set
		 *
		 * @tparam K the key type of the set to be encoded
		 * @tparam CK the converter used for encoding the key type
		 */
		template <typename K, typename CK = Converter<K>>
		struct is_set {
			bool operator()(const core::ExpressionPtr& expr) const {
				// use list converter
				return typename ListConverter<K, Converter<K>>::is_encoding_of()(expr);
			}
		};

		/**
		 * A generic functor encoding a set of elements into an IR expression.
		 *
		 * @tparam K the key type of the set to be encoded
		 * @tparam CK the converter used for encoding the key type
		 */
		template <typename K, typename CK = Converter<K>>
		struct encode_set {
			core::ExpressionPtr operator()(NodeManager& manager, const set<K>& set) const {
				// convert set into list
				vector<K> list;
				for(auto cur : set) {
					list.push_back(cur);
				}

				// use list converter
				return typename ListConverter<K, Converter<K>>::value_to_ir_converter()(manager, list);
			}
		};

		/**
		 * A generic functor decoding a set of elements from an IR expression.
		 *
		 * @tparam K the key type of the set to be encoded
		 * @tparam CK the converter used for encoding the key type
		 */
		template <typename K, typename CK = Converter<K>>
		struct decode_set {
			set<K> operator()(const core::ExpressionPtr& expr) const {
				assert((is_set<K, CK>()(expr)) && "Can only convert sets to sets!");

				// convert set into list
				vector<K> list = typename ListConverter<K, Converter<K>>::ir_to_value_converter()(expr);

				// convert list into a set
				set<K> res;
				for(auto cur : list) {
					res.insert(cur);
				}
				return res;
			}
		};
	}

	// define encoder / decoder according to the encoder framework

	/**
	 * Defines a converter functor allowing to customize the encoding of the element type.
	 *
	 * @tparam K the key type of the set to be encoded
	 * @tparam CK the converter used for encoding the key type
	 */
	template <typename K, typename CK = Converter<K>>
	struct SetConverter
		: public Converter<set<K>, detail::create_set_type<K, CK>, detail::encode_set<K, CK>, detail::decode_set<K, CK>, detail::is_set<K, CK>> {};

	/**
	 * A partial template specialization for the type_factory struct to support the encoding
	 * of sets using default element type converters.
	 */
	template <typename K>
	struct type_factory<set<K>> : public detail::create_set_type<K> {};

	/**
	 * A partial template specialization for the value_to_ir_converter struct to support the encoding
	 * of sets using default element type converters.
	 */
	template <typename K>
	struct value_to_ir_converter<set<K>> : public detail::encode_set<K> {};

	/**
	 * A partial template specialization for the ir_to_value_converter struct to support the encoding
	 * of sets using default element type converters.
	 */
	template <typename K>
	struct ir_to_value_converter<set<K>> : public detail::decode_set<K> {};

	/**
	 * A partial template specialization for the is_encoding_of struct to support the encoding
	 * of sets using default element type converters.
	 */
	template <typename K>
	struct is_encoding_of<set<K>> : public detail::is_set<K> {};


} // end namespace encoder
} // end namespace core
} // end namespace insieme
