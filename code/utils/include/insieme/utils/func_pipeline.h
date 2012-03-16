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
#include <future>

#include <boost/utility.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/printable.h"

/**
 * This class implements an utility which is a function pipeline. A Pipeline is formed by a 
 * number of stages N interleaved by N+1 buffers utilized to store the input/output results 
 * of the pipeline.
 */

namespace insieme { namespace utils { 

template <unsigned RetPos, unsigned... ArgPos>
class InOut { };

typedef std::function<void (void)> LazyFuncType;

template <class RetTy>
struct Functional : std::function<const RetTy& (void)> { 
	
	Functional(){ }

	template <class FuncTy>
	Functional(const FuncTy& f) : std::function<const RetTy& (void)>(f) { }

};
/**
 * A Action is an operation within the pipeline. The function takes a sequence of unsigned 
 * template parameters with the following semantics. The first param (RetPos) is the index of
 * the return type of this function inside the Output tuple passed to the constructor of this 
 * object. ArgsPos are the positions relative to the Input tuple which represent where the 
 * arguments of this function are taken from.
 */
template <unsigned RetPos, unsigned... ArgsPos>
struct Action : public LazyFuncType {

	template <class Input, class Output>
	Action(Input& inBuf, Output& outBuf, 
		const std::function<
			typename std::tuple_element<RetPos,Output>::type
				( const typename std::tuple_element<ArgsPos, Input>::type&...)>& functor) :
	LazyFuncType 
		( [&inBuf, &outBuf, functor] 
			(void) -> void { std::get<RetPos>(outBuf) = functor(std::get<ArgsPos>(inBuf)...); }
		) {  }
};

template <class InTuple, class OutTuple>
class Pipeline;

/**
 * A Stage is 1 step in the pipeline. It contains a number of functions which are all insisting on
 * the same buffers. 
 */
template <class InTuple, class OutTuple>
struct Stage : public boost::noncopyable, utils::Printable {

	typedef InTuple  in_buff;
	typedef std::shared_ptr<InTuple>  in_buff_ptr;

	typedef OutTuple out_buff;
	typedef std::shared_ptr<OutTuple> out_buff_ptr;

	Stage(): 
		inBuf( std::make_shared<in_buff>() ), 
		outBuf( std::make_shared<out_buff>() ) { }

	template <class... Units>
	Stage(const Units&... units) :
		inBuf( std::make_shared<in_buff>() ), 
		outBuf( std::make_shared<out_buff>() ), 
		functors ( { std::bind(units)... } ) { }

	Stage(const in_buff_ptr& in, const out_buff_ptr& out) : 
		inBuf(in ? in : std::make_shared<in_buff>()), 
		outBuf(out ? out : std::make_shared<out_buff>() ) { }

	template <class FuncTy, unsigned RetPos, unsigned... ArgPos>
	void add(const InOut<RetPos, ArgPos...>& pos, const FuncTy& f) {

		std::function<typename std::tuple_element<RetPos,out_buff>::type 
					( const typename std::tuple_element<ArgPos,in_buff>::type&...)> func = f;

		functors.push_back( Action<RetPos, ArgPos...>(*inBuf, *outBuf, func) ); 
	}

	OutTuple& operator()() const { 
		//std::cout << "InBuff:" << *inBuf << std::endl;
		std::vector<std::future<void>> handles;
		std::for_each(functors.begin(), functors.end(), [&handles](const LazyFuncType& cur) { 
				// Run each action in the stage in parallel
				handles.emplace_back( std::async(cur) );
			});
		std::for_each(handles.begin(), handles.end(), [&](const std::future<void>& h){ h.wait(); });
		// std::cout << "OutBuff:" << *outBuf << std::endl;
		return *outBuf;
	}

	template <class... Args, 
		typename std::enable_if< std::is_same<std::tuple<Args...>,InTuple>::value, bool>::type = 0
	>
	OutTuple& operator()(const Args&... args) const {
		*inBuf = std::make_tuple(args...);
		return (*this)();
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

	std::ostream& printTo(std::ostream& out) const {
		return out << "Pipeline.Stage[" << 
			std::tuple_size<InTuple>::value << ":" << std::tuple_size<OutTuple>::value <<
		"] - F(" << functors.size() << ")";
	}

private:
	in_buff_ptr  inBuf;
	out_buff_ptr outBuf;

	std::vector<LazyFuncType> functors;
};

template <class InTuple, class OutTuple, template <class, class> class Functor>
struct FunctorPtr : public std::shared_ptr<Functor<InTuple,OutTuple>>, utils::Printable {
	
	typedef Functor<InTuple,OutTuple> value_type;

	FunctorPtr(const std::shared_ptr<value_type>& other) : 
		std::shared_ptr<value_type>(other) { }

	OutTuple& operator()() const { return (*(*this))(); }

	template <class... Args, 
		typename std::enable_if< std::is_same<std::tuple<Args...>,InTuple>::value, bool>::type = 0
	>
	OutTuple& operator()(const Args&... args) const {
		(*this)->in_buffer() = std::make_tuple(args...);
		return (*(*this))();
	}

	std::ostream& printTo(std::ostream& out) const { return out << *(*this); }

};

template <class InTuple, class OutTuple>
FunctorPtr<InTuple,OutTuple,Stage> makeStage() { 
	return std::make_shared<Stage<InTuple,OutTuple>>(); 
}


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
const RetTy& lazy(Functor head) { 
	head(); 
	return head->out_buffer();
}

template <class RetTy, class Functor1, class Functor2, class... Tail>
const RetTy& lazy(Functor1 first, Functor2 second, Tail... tail) { 
	first();
	return details::lazy<RetTy, Functor2, Tail...>(second, tail...); 
}

} // end anonymous namespace 



template <class InTuple, class OutTuple>
struct Pipeline : public Functional<OutTuple>, utils::Printable, boost::noncopyable {

	typedef OutTuple out_buff;
	typedef std::shared_ptr<out_buff> out_buff_ptr;

	typedef InTuple in_buff;
	typedef std::shared_ptr<in_buff> in_buff_ptr;
	
	template <class... Stages, 
		typename std::enable_if< 
			std::is_same<out_buff, typename details::last<Stages...>::value::value_type::out_buff>::value &&
			std::is_same<in_buff, typename details::head<Stages...>::value::value_type::in_buff>::value,
		bool>::type = 0
	>
	Pipeline(Stages&... stages) :
		Functional<out_buff>( std::bind(details::lazy<out_buff, Stages&...>, stages...) ),
		inBuf( std::get<0>(std::tuple<Stages&...>(std::ref(stages)...))->in_buffer_ptr() ),
		outBuf( std::get<sizeof...(Stages)-1>(std::tuple<Stages&...>(std::ref(stages)...))->out_buffer_ptr() ) { }

	OutTuple& operator()() const { 
		static_cast<const std::function<const OutTuple& (void)>&>(*this)();
		return *outBuf;
	}

	template <class... Args, 
		typename std::enable_if< std::is_same<std::tuple<Args...>,InTuple>::value, bool>::type = 0
	>
	OutTuple& operator()(const Args&... args) const {
		*inBuf = std::make_tuple(args...);
		return (*this)();
	}

	out_buff& out_buffer() { return *outBuf; }
	const out_buff& out_buffer() const { return *outBuf; }
	out_buff_ptr& out_buffer_ptr() { return outBuf; }

	in_buff& in_buffer() { return *inBuf; }
	const in_buff& in_buffer_ptr() const { return *inBuf; }

	in_buff_ptr& in_buffer_ptr() { return inBuf; }

	std::ostream& printTo(std::ostream& out) const {
		return out << "Pipeline[" << std::tuple_size<InTuple>::value << ":" 
				   << std::tuple_size<OutTuple>::value << "]";
	}

private:
	in_buff_ptr&  inBuf;
	out_buff_ptr& outBuf;
};


namespace {

template <class InTuple, class OutTuple, unsigned Id, unsigned Displ, unsigned Rest>
struct add_in_connector {

	inline void operator()(const FunctorPtr<InTuple,OutTuple,Stage>& f) const {
		f->add( InOut<Id,Displ+Id>(), id<typename std::tuple_element<Id,OutTuple>::type>() );
		add_in_connector<InTuple,OutTuple,Id+1,Displ,Rest-1>()(f);
	}
};

template <class InTuple, class OutTuple, unsigned Id, unsigned Displ>
struct add_in_connector<InTuple,OutTuple,Id,Displ,0> {

	inline void operator()(const FunctorPtr<InTuple,OutTuple,Stage>& f) const {
		f->add( InOut<Id,Displ+Id>(), id<typename std::tuple_element<Id,OutTuple>::type>() );
	}
};

template <class InTuple, class OutTuple, unsigned Id, unsigned Displ, unsigned Rest>
struct add_out_connector {

	inline void operator()(const FunctorPtr<InTuple,OutTuple,Stage>& f) const {
		f->add( InOut<Displ+Id,Id>(), id<typename std::tuple_element<Id,InTuple>::type>() );
		add_out_connector<InTuple,OutTuple,Id+1,Displ,Rest-1>()(f);
	}
};

template <class InTuple, class OutTuple, unsigned Id, unsigned Displ>
struct add_out_connector<InTuple,OutTuple,Id,Displ,0> {

	inline void operator()(const FunctorPtr<InTuple,OutTuple,Stage>& f) const {
		f->add( InOut<Displ+Id,Id>(), id<typename std::tuple_element<Id,InTuple>::type>() );
	}
};

} // end anonymous namespace

// Operator overloads 

// Build a pipeline by appending stages or other pipelines 
template <class InTuple, class InnerTuple, class OutTuple, 
	 template <class,class> class Unit1, template <class,class> class Unit2
>
FunctorPtr<InTuple, OutTuple, Pipeline> 
operator>>(const FunctorPtr<InTuple,InnerTuple,Unit1>& s1, const FunctorPtr<InnerTuple,OutTuple,Unit2>& s2) {
	// Bind the pipelines
	
	FunctorPtr<InnerTuple,InnerTuple,Stage> conn(
		std::make_shared<Stage<InnerTuple,InnerTuple>>(s1->out_buffer_ptr(), s2->in_buffer_ptr())
	);

	add_in_connector<InnerTuple,InnerTuple,0,0,std::tuple_size<InnerTuple>::value-1>()(conn);

	auto&& s =  FunctorPtr<InTuple,OutTuple,Pipeline>(
			std::make_shared<Pipeline<InTuple,OutTuple>>(s1, conn, s2)
		);

	return s;
}

template <class... InTuple1, class... OutTuple1, class... InTuple2, class... OutTuple2,
	 template <class,class> class Unit1, template <class,class> class Unit2
>
FunctorPtr<std::tuple<InTuple1...,InTuple2...>, std::tuple<OutTuple1...,OutTuple2...>, Stage> 
operator|(const FunctorPtr<std::tuple<InTuple1...>,std::tuple<OutTuple1...>,Unit1>& s1, 
 		   const FunctorPtr<std::tuple<InTuple2...>,std::tuple<OutTuple2...>,Unit2>& s2) {
	// Bind the pipelines

	typedef std::tuple<InTuple1...,InTuple2...> InBuff;
	typedef std::tuple<OutTuple1...,OutTuple2...> OutBuff;

	auto&& ss1 = makeStage<InBuff, std::tuple<InTuple1...>>();
	auto&& se1 = makeStage<std::tuple<OutTuple1...>,OutBuff>();
	auto&& ss2 = makeStage<InBuff, std::tuple<InTuple2...>>();
	auto&& se2 = makeStage<std::tuple<OutTuple2...>,OutBuff>();

	auto&& p1 = ss1 >> s1 >> se1;
	auto&& p2 = ss2 >> s2 >> se2;

	FunctorPtr<InBuff,OutBuff,Stage>&& s = std::make_shared<Stage<InBuff,OutBuff>>(p1,p2);

	p1->in_buffer_ptr() = s->in_buffer_ptr();
	p2->in_buffer_ptr() = s->in_buffer_ptr();

	se1->out_buffer_ptr() = s->out_buffer_ptr();
	se2->out_buffer_ptr() = s->out_buffer_ptr();

	add_in_connector<InBuff, std::tuple<InTuple1...>, 0, 0, sizeof...(InTuple1)-1>()(ss1);
	add_in_connector<InBuff, std::tuple<InTuple2...>, 0, sizeof...(InTuple1), sizeof...(InTuple2)-1>()(ss2);

	add_out_connector<std::tuple<OutTuple1...>, OutBuff, 0, 0, sizeof...(OutTuple1)-1>()(se1);
	add_out_connector<std::tuple<OutTuple2...>, OutBuff, 0, sizeof...(OutTuple1), sizeof...(OutTuple2)-1>()(se2);

	return s;
}

// Define macros which implements binary operators. Argument of binary operations (which might be
// Actions as well) are executed in parallel.



} } // end insieme::utils namespace

namespace std {
#define OPERATOR(SYM, FUNC) \
template <class InTuple1, class InTuple2, class OutTuple, \
	 template <class,class> class Unit1, template <class,class> class Unit2 \
> \
insieme::utils::FunctorPtr<std::tuple<InTuple1,InTuple2>, std::tuple<OutTuple>, insieme::utils::Pipeline>  \
operator SYM( \
		const insieme::utils::FunctorPtr<std::tuple<InTuple1>,std::tuple<OutTuple>,Unit1>& s1, \
		const insieme::utils::FunctorPtr<std::tuple<InTuple2>,std::tuple<OutTuple>,Unit2>& s2) { \
	insieme::utils::FunctorPtr<std::tuple<OutTuple,OutTuple>,std::tuple<OutTuple>,insieme::utils::Stage>&& s =  \
		std::make_shared<insieme::utils::Stage<std::tuple<OutTuple,OutTuple>,std::tuple<OutTuple>>>(); \
	s->add( insieme::utils::InOut<0,0,1>(), std::FUNC<OutTuple>() ); \
	return (s1 | s2) >> s; \
}

OPERATOR(+,plus)
OPERATOR(-,minus)
OPERATOR(/,divides)
OPERATOR(%,modulus)
OPERATOR(*,multiplies)

#undef OPERATOR
}


