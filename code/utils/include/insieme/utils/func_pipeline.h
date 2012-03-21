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
#include <typeinfo>
#include <iostream>

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
struct Pipeline {
	
	typedef typename lambda_traits<typename impl::first<Functors...>::value>::argument_types	argument_types; 
	typedef typename lambda_traits<typename impl::last<Functors...>::value>::result_type		result_type;

	Pipeline(const Functors&... funcs) : tup(std::make_tuple( funcs... )) { }

	template <class ... Args>
//	typename std::enable_if<
//		std::is_same<typename lambda_traits<typename impl::first<Functors...>::value>::argument_types, type_list<Args...>>::value,
		typename lambda_traits<typename impl::last<Functors...>::value>::result_type
//	>::type
	operator()(Args... args) const { 
		return impl::invoker<std::tuple<Functors...>,sizeof...(Functors)>()(tup, args...);
	}

private:
	std::tuple<Functors...> tup;
};


template <class Op, class F1, class F2, class Enable=void>
struct Reduction2;

template <class Op, class F1, class F2>
struct Reduction2<Op,F1,F2,
	typename std::enable_if<
		std::is_same<
			type_list<
				typename lambda_traits<F1>::result_type,
				typename lambda_traits<F2>::result_type
			>, 
			typename lambda_traits<Op>::argument_types
		>::value>::type> 
{
	typedef typename lambda_traits<Op>::result_type result_type;

	typedef type_list<
		typedef lambda_traits<F1>::argument_type, 
		typedef lambda_traits<F2>::argument_type
	> argument_types;

	Reduction2(const Op& op, const F1& f1, const F2& f2) : tup( std::make_tuple(op,f1,f2) ) { }

	template <class Arg1, class Arg2>
	typename std::enable_if<
		std::is_same<typename lambda_traits<F1>::argument_types, type_list<Arg1>>::value && 
		std::is_same<typename lambda_traits<F2>::argument_types, type_list<Arg2>>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2));
	}
	
	template <class Arg1, class Arg2>
	typename std::enable_if<
		std::is_same<typename lambda_traits<F1>::argument_types, type_list<const Arg1&>>::value && 
		std::is_same<typename lambda_traits<F2>::argument_types, type_list<const Arg2&>>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2));
	}

private:
	std::tuple<Op,F1,F2> tup;
	
};

// Sepcialization for const ref members
template <class Op, class F1, class F2>
struct Reduction2<Op,F1,F2,
	typename std::enable_if<
		std::is_same<
			type_list<
				const typename lambda_traits<F1>::result_type&,
				const typename lambda_traits<F2>::result_type&
			>, 
			typename lambda_traits<Op>::argument_types
		>::value>::type> 
{
	typedef typename lambda_traits<Op>::result_type result_type;

	typedef type_list<
		typedef lambda_traits<F1>::argument_type, 
		typedef lambda_traits<F2>::argument_type
	> argument_types;

	Reduction2(const Op& op, const F1& f1, const F2& f2) : tup( std::make_tuple(op,f1,f2) ) { }

	template <class Arg1, class Arg2>
	typename std::enable_if<
		std::is_same<
			typename lambda_traits<F1>::argument_types, 
			type_list<Arg1>
		>::value && 
		std::is_same<
			typename lambda_traits<F2>::argument_types, 
			type_list<Arg2>
		>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2));
	}

	template <class Arg1, class Arg2>
	typename std::enable_if<
		std::is_same<
			typename lambda_traits<F1>::argument_types, 
			type_list<const Arg1&>
		>::value && 
		std::is_same<
			typename lambda_traits<F2>::argument_types, 
			type_list<const Arg2&>
		>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2));
	}
private:
	std::tuple<Op,F1,F2> tup;
	
};


template <class Op, class F1, class F2, class F3, class Enable=void>
class Reduction3;

template <class Op, class F1, class F2, class F3>
struct Reduction3<Op, F1, F2, F3, 
	typename std::enable_if<
		std::is_same<
			type_list<
				typename lambda_traits<F1>::result_type,
				typename lambda_traits<F2>::result_type,
				typename lambda_traits<F3>::result_type
			>, 	
			typename lambda_traits<Op>::argument_types
		>::value
	>::type> 
{
	
	typedef typename lambda_traits<Op>::result_type result_type;

	typedef type_list<
		typedef lambda_traits<F1>::argument_type, 
		typedef lambda_traits<F2>::argument_type,
		typedef lambda_traits<F3>::argument_type
	> argument_types;

	Reduction3(const Op& op, const F1& f1, const F2& f2, const F3& f3) : 
		tup( std::make_tuple(op,f1,f2,f3) ) { }

	template <class Arg1, class Arg2, class Arg3>
	typename std::enable_if<
		std::is_same<typename lambda_traits<F1>::argument_types, type_list<Arg1>>::value && 
		std::is_same<typename lambda_traits<F2>::argument_types, type_list<Arg2>>::value && 
		std::is_same<typename lambda_traits<F3>::argument_types, type_list<Arg3>>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2, const Arg3& arg3) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2), std::get<3>(tup)(arg3));
	}

	template <class Arg1, class Arg2, class Arg3>
	typename std::enable_if<
		std::is_same<typename lambda_traits<F1>::argument_types, type_list<const Arg1&>>::value && 
		std::is_same<typename lambda_traits<F2>::argument_types, type_list<const Arg2&>>::value && 
		std::is_same<typename lambda_traits<F3>::argument_types, type_list<const Arg3&>>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2, const Arg3& arg3) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2), std::get<3>(tup)(arg3));
	}

private:
	std::tuple<Op,F1,F2,F3> tup;
};


template <class Op, class F1, class F2, class F3>
struct Reduction3<Op,F1,F2,F3, 
	typename std::enable_if<
		std::is_same<
			type_list<
				const typename lambda_traits<F1>::result_type&,
				const typename lambda_traits<F2>::result_type&,
				const typename lambda_traits<F3>::result_type&
			>, 
			typename lambda_traits<Op>::argument_types>::value
		>::type>
{
	typedef typename lambda_traits<Op>::result_type result_type;

	typedef type_list<
		typename typedef lambda_traits<F1>::argument_type, 
		typename typedef lambda_traits<F2>::argument_type,
		typename typedef lambda_traits<F3>::argument_type
	> argument_types;

	Reduction3(const Op& op, const F1& f1, const F2& f2, const F3& f3) : 
		tup( std::make_tuple(op,f1,f2,f3) ) { }

	template <class Arg1, class Arg2, class Arg3>
	typename std::enable_if<
		std::is_same<typename lambda_traits<F1>::argument_types, type_list<Arg1>>::value && 
		std::is_same<typename lambda_traits<F2>::argument_types, type_list<Arg2>>::value && 
		std::is_same<typename lambda_traits<F3>::argument_types, type_list<Arg3>>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2, const Arg3& arg3) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2), std::get<3>(tup)(arg3));
	}

	template <class Arg1, class Arg2, class Arg3>
	typename std::enable_if<
		std::is_same<typename lambda_traits<F1>::argument_types, type_list<const Arg1&>>::value && 
		std::is_same<typename lambda_traits<F2>::argument_types, type_list<const Arg2&>>::value && 
		std::is_same<typename lambda_traits<F3>::argument_types, type_list<const Arg3&>>::value,
		typename lambda_traits<Op>::result_type
	>::type
	operator()(const Arg1& arg1, const Arg2& arg2, const Arg3& arg3) const { 
		return std::get<0>(tup)(std::get<1>(tup)(arg1), std::get<2>(tup)(arg2), std::get<3>(tup)(arg3));
	}

private:
	std::tuple<Op,F1,F2,F3> tup;
};

} } } // end insieme::utils::pipeline namespace 

template <class ... Args>
struct lambda_traits< insieme::utils::pipeline::Pipeline<Args...> > {
	typedef typename insieme::utils::pipeline::Pipeline<Args...>::result_type result_type;
	typedef typename insieme::utils::pipeline::Pipeline<Args...>::argument_types argument_types;
};

template <class Op, class F1, class F2>
struct lambda_traits< insieme::utils::pipeline::Reduction2<Op,F1,F2> > {
	typedef typename insieme::utils::pipeline::Reduction2<Op,F1,F2>::result_type result_type;
	typedef typename insieme::utils::pipeline::Reduction2<Op,F1,F2>::argument_types argument_types;
};

template <class Op, class F1, class F2, class F3>
struct lambda_traits< insieme::utils::pipeline::Reduction3<Op,F1,F2,F3> > {
	typedef typename insieme::utils::pipeline::Reduction3<Op,F1,F2,F3>::result_type result_type;
	typedef typename insieme::utils::pipeline::Reduction3<Op,F1,F2,F3>::argument_types argument_types;
};
