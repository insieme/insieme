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
#include <future>

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

// Check whether the list of functors are compatible with each other.
// In other words we make sure that return type of function Fi is compatible with input arguments of
// function Ti+1
template <class ... Fs>
struct check_compability;

template <class First, class Second, class ... Fs>
struct check_compability<First,Second,Fs...> { 

	enum { value = 
		(std::is_same<
			type_list<typename lambda_traits<First>::result_type>, 
			typename lambda_traits<Second>::argument_types
		>::value || 
		std::is_same<
			type_list<const typename lambda_traits<First>::result_type&>, 
			typename lambda_traits<Second>::argument_types
		>::value) && check_compability<Second,Fs...>::value 
	};

};

template <class SecondLast, class Last>
struct check_compability<SecondLast,Last> { 

	enum { value =
		std::is_same<
			type_list<typename lambda_traits<SecondLast>::result_type>, 
			typename lambda_traits<Last>::argument_types
		>::value ||
		std::is_same<
			type_list<const typename lambda_traits<SecondLast>::result_type&>, 
			typename lambda_traits<Last>::argument_types
		>::value
	};

};

template <class Last>
struct check_compability<Last> { 

	enum { value = true };

};

/**
 * Static Function Tuple Argument Unpacking
 *
 * This recursive template unpacks the tuple parameters into
 * variadic template arguments until we reach the count of 0 where the function
 * is called with the correct parameters
 *
 * @tparam N Number of tuple arguments to unroll
 *
 * @ingroup g_util_tuple
 */
template < uint S, uint N>
struct apply_func
{
  template < typename Func, typename... ArgsT, typename... Args >
  static typename lambda_traits<Func>::result_type applyTuple( const Func& f,
                       const std::tuple<ArgsT...>& t,
                       Args... args )
  {
    return apply_func<S,N-1>::applyTuple( f, t, std::get<S+N-1>( t ), args... );
  }
};

//-----------------------------------------------------------------------------

/**
 * Static Function Tuple Argument Unpacking End Point
 *
 * This recursive template unpacks the tuple parameters into
 * variadic template arguments until we reach the count of 0 where the function
 * is called with the correct parameters
 *
 * @ingroup g_util_tuple
 */
template <uint S>
struct apply_func<S,0>
{
  template < typename Func, typename... ArgsT, typename... Args >
  static typename lambda_traits<Func>::result_type applyTuple( const Func& f,
                       const std::tuple<ArgsT...>& /* t */,
                       Args... args )
  {
	return f( args... );
  }
};

//-----------------------------------------------------------------------------

/**
 * Static Function Call Forwarding Using Tuple Pack Parameters
 */
// Actual apply function
template < unsigned Start, typename Func, typename... ArgsT>
 typename lambda_traits<Func>::result_type applyTuple( const Func& f, std::tuple<ArgsT...> const& t )
{
   return apply_func<Start, 
	   		size_of<typename lambda_traits<Func>::argument_types>::value
		  >::applyTuple( f, t );
}

} // end details namespace


template <class Enable, class... Fs>
struct Pipeline;

template <class ... Fs>
struct Pipeline<
	// typename std::enable_if<impl::check_compability<Fs...>::value>::type,
	void,
	Fs...> 
{
	typedef typename impl::first<Fs...>::value first;
	typedef typename impl::last<Fs...>::value  last;

	typedef typename lambda_traits<first>::argument_types argument_types; 
	typedef typename lambda_traits<last>::result_type	  result_type;

	Pipeline(const Fs&... funcs) : tup(std::make_tuple( funcs... )) { }

	template <class ... Args>
	typename std::enable_if<
		std::is_same<argument_types,type_list<Args...>>::value ||
		std::is_same<argument_types,type_list<const Args&...>>::value,
		result_type
	>::type
	operator()(const Args&... args) const { 
		return impl::invoker<std::tuple<Fs...>,sizeof...(Fs)>()(tup, args...);
	}

	template <class ... Args>
	typename std::enable_if<
		std::is_same<argument_types,type_list<Args&...>>::value,
		result_type
	>::type
	operator()(Args&... args) const { 
		return impl::invoker<std::tuple<Fs...>,sizeof...(Fs)>()(tup, args...);
	}

	// Specialization for pipeline starting with 0 arguments 
	result_type operator()() const { 
		return impl::invoker<std::tuple<Fs...>,sizeof...(Fs)>()(tup);
	}


private:
	std::tuple<Fs...> tup;
};

// utility functions to create pipelines
template <class ... Fs>
Pipeline<void,Fs...> makePipeline(const Fs&... fs) { return Pipeline<void,Fs...>(fs...); }


template <class Op, class F1, class F2, class Enable=void>
struct Reduction;

template <class Op, class F1, class F2>
struct Reduction<Op,F1,F2,
	typename std::enable_if<
		std::is_same<
			type_list<typename lambda_traits<F1>::result_type,typename lambda_traits<F2>::result_type>,
			typename lambda_traits<Op>::argument_types
		>::value ||
		std::is_same<
			type_list<const typename lambda_traits<F1>::result_type&, const typename lambda_traits<F2>::result_type&>, 
			typename lambda_traits<Op>::argument_types
		>::value>::type> 
{
	typedef typename lambda_traits<Op>::result_type result_type;

	typedef typename lambda_traits<F1>::argument_types f1_args;
	typedef typename lambda_traits<F2>::argument_types f2_args;
	typedef typename concat<f1_args,f2_args>::type argument_types;

	Reduction(const Op& op, const F1& f1, const F2& f2) : tup( std::make_tuple(op,f1,f2) ) { }

	template <class... Args>
	result_type operator()(const Args&... args) const { 
		auto tArg = std::tie(args...);

		auto lhsH = std::async([&](void) { return impl::applyTuple<0>( std::get<1>(tup), tArg ); });
		auto rhsH = std::async([&](void) { return impl::applyTuple<size_of<f1_args>::value>( std::get<2>(tup), tArg ); });

		return std::get<0>(tup)( lhsH.get(), rhsH.get() );
	}

private:
	std::tuple<Op,F1,F2> tup;
};


template <class Op, class F1, class F2>
Reduction<Op,F1,F2> makeReduction(const Op& op, const F1& f1, const F2& f2) { 
	return Reduction<Op,F1,F2>(op,f1,f2); 
}

template <class Op, class F1, class F2, class F3, class ... Fs>
auto makeReduction(const Op& op, const F1& f1, const F2& f2, const F3& f3, const Fs&... fs) -> 
Reduction<Op,F1,decltype(makeReduction(op,f2,f3,fs...))> 
{ 
	return Reduction<Op, F1, decltype(makeReduction(op,f2,f3,fs...))>(op, f1, makeReduction(op,f2,f3,fs...)); 
}

} } } // end insieme::utils::pipeline namespace 

template <class ... Args>
struct lambda_traits< insieme::utils::pipeline::Pipeline<void,Args...> > {
	typedef typename insieme::utils::pipeline::Pipeline<void,Args...>::result_type result_type;
	typedef typename insieme::utils::pipeline::Pipeline<void,Args...>::argument_types argument_types;
};

template <class Op, class F1, class F2>
struct lambda_traits< insieme::utils::pipeline::Reduction<Op,F1,F2> > {
	typedef typename insieme::utils::pipeline::Reduction<Op,F1,F2>::result_type result_type;
	typedef typename insieme::utils::pipeline::Reduction<Op,F1,F2>::argument_types argument_types;
};

