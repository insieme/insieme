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

#include <memory>
#include <tuple>

namespace insieme { namespace utils { namespace pipeline {

namespace impl {

template <class ...Classes>
struct last;

template <class Head1, class Head2, class ...Tail>
struct last<Head1, Head2, Tail...> {
	typedef typename last<Head2, Tail...>::value value;
};

template <class Head>
struct last<Head> {
	typedef Head value;
};


template <class ...Classes>
struct first;

template <class Head, class ...Tail>
struct first<Head, Tail...> {
	typedef Head value;
};

template <class Tuple, size_t TupleId>
struct invoker {

	template <class ...Args>
	inline typename lambda_traits<typename std::tuple_element<std::tuple_size<Tuple>::value-1,Tuple>::type>::result_type
	operator()(const Tuple& t, const Args&... args) const {
		return invoker<Tuple,TupleId-1>()( t, std::get<std::tuple_size<Tuple>::value-TupleId>(t)(args...) );
	}
};

template <class Tuple>
struct invoker<Tuple,1> {

	template <class ...Args>
	inline typename lambda_traits<typename std::tuple_element<std::tuple_size<Tuple>::value-1,Tuple>::type>::result_type
	operator()(const Tuple& t, const Args& ... args) const {
		return std::get<std::tuple_size<Tuple>::value-1>(t)(args...);
	}
};

} // end details namespace

template <class... Functors>
struct Pipeline: public std::tuple<Functors...> {
	
	Pipeline(const Functors&... funcs) : std::tuple<Functors...>( funcs... ) { }

	template <class ... Args>
	typename std::enable_if<
		size_of<typename lambda_traits<typename impl::first<Functors...>::value>::argument_types>::value == sizeof...(Args),
		typename lambda_traits<typename impl::last<Functors...>::value>::result_type
	>::type
	operator()(const Args&... args) const { 
		return impl::invoker<std::tuple<Functors...>,sizeof...(Functors)>()(*this, args...);
	}

};

} } } // end insieme::utils::pipeline namespace 
