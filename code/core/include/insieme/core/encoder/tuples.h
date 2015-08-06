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

#include <tuple>

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/lang/extension.h"


namespace insieme {
namespace core {
namespace encoder {

using std::tuple;

// -- tuples representation within the IR --

namespace detail {

//  -- some utility functors --

namespace {

template<int x = 1>			// a dummy parameter to make this one a template function
vector<TypePtr> toTypeList(NodeManager& manager) {
	return vector<TypePtr>();
}

template<typename First, typename ... T>
vector<TypePtr> toTypeList(NodeManager& manager) {
	vector<TypePtr> res = toTypeList<T...>(manager);
	res.insert(res.begin(), typename Converter<First>::type_factory()(manager));
	return res;
}
}

/**
 * A generic functor creating the IR type of an encoded tuple.
 *
 * @tparam T the element types of the tuple
 */
template<typename ... T>
struct create_tuple_type {
	core::TypePtr operator()(NodeManager& manager) const {
		// this should work, yet not implemented in GCC 4.6.3
//				return TupleType::get(manager, toVector(typename Converter<T...>::type_factory()(manager)));

		// this is the dirty, manual fall-back ...
		return TupleType::get(manager, toTypeList<T...>(manager));
	}
	create_tuple_type() {}
};

namespace {

template<typename Iter>
bool checkEncoding(const Iter& begin, const Iter& end) {
	return begin == end;
}

template<typename Iter, typename First, typename ... T>
bool checkEncoding(const Iter& begin, const Iter& end) {
	return begin != end &&
	       typename Converter<First>::is_encoding_of()(*begin) &&
	       checkEncoding<Iter, T...>(begin+1, end);
}

}

/**
 * A generic functor testing whether a given expression is a valid
 * encoding of a tuple of elements of types T.
 *
 * @tparam T the element types of the tuple
 */
template<typename ... T>
struct is_tuple {
	bool operator()(const core::ExpressionPtr& expr) const {
	
		// needs to be a tuple expression
		if(expr->getNodeType() != core::NT_TupleExpr) {
			return false;
		}
		
		// check components of tuple
		TupleExprPtr tuple = expr.as<TupleExprPtr>();
		return checkEncoding<decltype(tuple->getExpressions().begin()), T...>(tuple->getExpressions().begin(), tuple->getExpressions().end());
	}
	is_tuple() {}
};

namespace {

template<int pos, typename ... T>
struct pack {
	void operator()(NodeManager& mgr, const tuple<T...>& tuple, vector<ExpressionPtr>& res) const {
		res[pos-1] = toIR(mgr, std::get<pos-1>(tuple));
		pack<pos-1,T...>()(mgr, tuple, res);
	}
	pack() {}
};

template<typename ... T>
struct pack<0, T...> {
	void operator()(NodeManager& mgr, const tuple<T...>& tuple, vector<ExpressionPtr>& res) const {}
	pack() {}
};

template<typename ... T>
vector<ExpressionPtr> encodeTuple(NodeManager& manager, const tuple<T...>& tuple) {
	vector<ExpressionPtr> res(sizeof...(T));
	pack<sizeof...(T), T...>()(manager, tuple, res);
	//pack<sizeof...(T)>(manager, tuple, res);
	return res;
}

}

/**
 * A generic functor encoding a tuple of elements into an IR expression.
 *
 * @tparam T the element types of the tuple
 */
template<typename ... T>
struct encode_tuple {
	core::ExpressionPtr operator()(NodeManager& manager, const tuple<T...>& tuple) const {
		// convert tuple into tuple expression using helper function
		auto ret = IRBuilder(manager).tupleExpr(encodeTuple(manager, tuple));
		core::lang::markAsBuiltIn(ret);
		return ret;
	}
	encode_tuple() {}
};

namespace {

template<int pos, typename ... T>
struct unpack {
	void operator()(const vector<ExpressionPtr>& encoded, tuple<T...>& tuple) const {
		std::get<pos-1>(tuple) = toValue<typename std::tuple_element<pos-1, std::tuple<T...>>::type>(encoded[pos-1]);
		unpack<pos-1,T...>()(encoded, tuple);
	}
	unpack() {}
};

template<typename ... T>
struct unpack<0, T...> {
	void operator()(const vector<ExpressionPtr>& encoded, tuple<T...>& tuple) const {}
	unpack() {}
};

template<typename ... T>
tuple<T...> decodeTuple(const vector<ExpressionPtr>& encoded) {
	tuple<T...> res;
	unpack<sizeof...(T),T...>()(encoded, res);
	return res;
}

}

/**
 * A generic functor decoding a tuple of elements from an IR expression.
 *
 * @tparam T the element types of the tuple
 */
template<typename ... T>
struct decode_tuple {
	tuple<T...> operator()(const core::ExpressionPtr& expr) const {
		assert((is_tuple<T...>()(expr)) && "Can only convert tuples to tuples!");
		return decodeTuple<T...>(expr.as<TupleExprPtr>()->getExpressions()->getExpressions());
	}
	decode_tuple() {}
};


}

// define encoder / decoder according to the encoder framework

/**
 * Defines a tuple converter functor allowing to customize the encoding of the element type.
 *
 * @tparam T the element types of the tuple
 */
template<typename ... T>
struct TupleConverter : public
	Converter<tuple<T...>, detail::create_tuple_type<T...>, detail::encode_tuple<T...>, detail::decode_tuple<T...>, detail::is_tuple<T...>> {};
	
/**
 * A partial template specialization for the type_factory struct to support the encoding
 * of tuples using default element type converters.
 */
template<typename ... T>
struct type_factory<tuple<T...>> : public detail::create_tuple_type<T...> {
	type_factory() {}
};

/**
 * A partial template specialization for the value_to_ir_converter struct to support the encoding
 * of tuples using default element type converters.
 */
template<typename ... T>
struct value_to_ir_converter<tuple<T...>> : public detail::encode_tuple<T...> {
	value_to_ir_converter() {}
};

/**
 * A partial template specialization for the ir_to_value_converter struct to support the encoding
 * of tuples using default element type converters.
 */
template<typename ... T>
struct ir_to_value_converter<tuple<T...>> : public detail::decode_tuple<T...> {
	ir_to_value_converter() {}
};

/**
 * A partial template specialization for the is_encoding_of struct to support the encoding
 * of tuples using default element type converters.
 */
template<typename ... T>
struct is_encoding_of<tuple<T...>> : public detail::is_tuple<T...> {
	is_encoding_of() {}
};


} // end namespace encoder
} // end namespace core
} // end namespace insieme
