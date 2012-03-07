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

#include <tuple>
#include <functional>
#include <algorithm>
#include <memory>
#include <iostream>

#include "insieme/utils/container_utils.h"

/**
 * This class implements an utility which is a function pipeline. A Pipeline is formed by a 
 * number of stages N interleaved by N+1 buffers utilized to store the input/output results 
 * of the pipeline.
 */

namespace insieme { namespace utils { 

template <unsigned RetPos, unsigned... ArgPos>
class InOut{ };

typedef std::function<void (void)> LazyFuncType;

/**
 * A Function is an operation within the pipeline. The function takes a sequence of unsigned 
 * template parameters with the following semantics. The first param (RetPos) is the index of
 * the return type of this function inside the Output tuple passed to the constructor of this 
 * object. ArgsPos are the positions relative to the Input tuple which represent where the 
 * arguments of this function are taken from.
 */
template <unsigned RetPos, unsigned... ArgsPos>
struct Function : public LazyFuncType {

	template <class Input, class Output>
	Function(Input& inBuf, Output& outBuf, 
		const std::function<
			typename std::tuple_element<RetPos,Output>::type 
				( const typename std::tuple_element<ArgsPos, Input>::type&...)>& functor) :
	LazyFuncType 
		( [&inBuf, &outBuf, functor] 
			(void) -> void { std::get<RetPos>(outBuf) = functor(std::get<ArgsPos>(inBuf)...); }
		) { }
};

template <class Functor1, class Functor2, class... Tail>
void lazy(const Functor1& first, const Functor2& second, const Tail&... tail) { 
	// do it in parallel? 
	first();
	lazy(second, tail...); 
}
template <class Functor>
void lazy(const Functor& head) { head(); }


template <class RetTy>
struct Functional : std::function<RetTy& (void)> { 
	
	Functional(){ }

	template <class FuncTy>
	Functional(const FuncTy& f) : std::function<RetTy& (void)>(f) { }

};

/**
 * A Stage is 1 step in the pipeline. It contains a number of functions which are all insisting on
 * the same buffers. 
 */
template <class InTuple, class OutTuple>
struct Stage : public Functional<OutTuple> {

	typedef InTuple  in_buff;
	typedef std::shared_ptr<InTuple>  in_buff_ptr;

	typedef OutTuple out_buff;
	typedef std::shared_ptr<OutTuple> out_buff_ptr;

	Stage(): inBuf( std::make_shared<in_buff>() ), outBuf( std::make_shared<out_buff>() ) { }

	Stage(const in_buff_ptr& in, const out_buff_ptr& out) : 
		inBuf(in ? in : std::make_shared<in_buff>()), 
		outBuf(out ? out : std::make_shared<out_buff>() ) { }

	template <class FuncTy, unsigned RetPos, unsigned... ArgPos>
	void add(const InOut<RetPos, ArgPos...>& pos, const FuncTy& f) {

		std::function<typename std::tuple_element<RetPos,out_buff>::type 
					( const typename std::tuple_element<ArgPos,in_buff>::type&...)> func = f;

		functors.push_back( Function<RetPos, ArgPos...>(*inBuf, *outBuf, func) ); 
	}

	// This could be executed in parallel FIXME
	OutTuple& operator()() const { 
		std::cout << "InBuff:" << *inBuf << std::endl;
		std::for_each(functors.begin(), functors.end(), 
				[](const LazyFuncType& cur) { cur(); });
		std::cout << "OutBuff:" << *outBuf << std::endl;
		return *outBuf;
	}

	// Accessors for output buffer
	out_buff& out_buffer() { return *outBuf; }
	const out_buff& out_buffer() const { return *outBuf; }

	out_buff_ptr& out_buffer_ptr() { return outBuf; }
	const out_buff_ptr& out_buffer_ptr() const { return outBuf; }

	// Accessors for input buffer
	in_buff& in_buffer() { return *inBuf; }
	const in_buff& in_buffer() const { return *inBuf; }
	
	in_buff_ptr& in_buffer_ptr() { return inBuf; }
	const in_buff_ptr& in_buffer_ptr() const { return inBuf; }

private:
	in_buff_ptr inBuf;
	out_buff_ptr outBuf;

	std::vector<LazyFuncType> functors;
};

namespace details {

// Return the last Type of a variadic template 
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

// Returns the first type of a variadic template 
template <class ...Classes>
struct head;

template <class Head, class ...Tail>
struct head<Head, Tail...> {
	typedef Head value;
};

template <class RetTy, class Functor>
RetTy lazy(Functor& head) { 
	auto v = head(); 
	std::cout << std::get<0>(v) << std::endl;
	std::cout << std::get<0>(head.out_buffer()) << std::endl;
	return head.out_buffer();
}

template <class RetTy, class Functor1, class Functor2, class... Tail>
RetTy lazy(Functor1& first, Functor2& second, Tail&... tail) { 
	first();
	return details::lazy<RetTy, Functor2, Tail...>(second, tail...); 
}

} // end anonymous namespace 

template <class InTuple, class OutTuple>
struct Pipeline : public Functional<OutTuple> {

	typedef OutTuple out_buff;
	typedef std::shared_ptr<out_buff> out_buff_ptr;
	
	template <class... Stages, 
		typename std::enable_if< 
			std::is_same<out_buff, typename details::last<Stages...>::value::out_buff>::value &&
			std::is_same<InTuple, typename details::head<Stages...>::value::in_buff>::value,
		bool>::type = 0
	>
	Pipeline(Stages&... stages) : 
		Functional<out_buff>( std::bind(details::lazy<out_buff&, Stages&...>, stages...) ) { }

};

// Operator overloads 

template <class InTuple, class InnerTuple, class OutTuple>
Pipeline<InTuple, OutTuple> operator>>(Stage<InTuple,InnerTuple>& s1, Stage<InnerTuple,OutTuple>& s2) {
	// Bind the pipelines
	s2.in_buffer_ptr() = s1.out_buffer_ptr();
	return Pipeline<InTuple, OutTuple>(s1, s2);
}

template <class InTuple, class InnerTuple, class OutTuple>
Pipeline<InTuple, OutTuple> operator>>(Pipeline<InTuple,InnerTuple>& s1, Stage<InnerTuple,OutTuple>& s2) {
	return Pipeline<InTuple, OutTuple>(s1, s2);
}

template <class InTuple, class InnerTuple, class OutTuple>
Pipeline<InTuple, OutTuple> operator>>(Stage<InTuple,InnerTuple>& s1, Pipeline<InnerTuple,OutTuple>& s2) {
	return Pipeline<InTuple, OutTuple>(s1, s2);
}

template <class InTuple, class InnerTuple, class OutTuple>
Pipeline<InTuple, OutTuple> operator>>(Pipeline<InTuple,InnerTuple>& s1, Pipeline<InnerTuple,OutTuple>& s2) {
	return Pipeline<InTuple, OutTuple>(s1, s2);
}

} } // end insieme::utils namespace




