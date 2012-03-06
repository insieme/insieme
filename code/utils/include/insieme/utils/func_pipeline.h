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

/**
 * This class implements an utility which is a function pipeline. A Pipeline is formed by a 
 * number of stages N interleaved by N+1 buffers utilized to store the input/output results 
 * of the pipeline.
 */

namespace insieme { namespace utils {

/**
 * A Function is an operation within the pipeline. The function takes a sequence of unsigned 
 * template parameters with the following semantics. The first param (RetPos) is the index of
 * the return type of this function inside the Output tuple passed to the constructor of this 
 * object. ArgsPos are the positions relative to the Input tuple which represent where the 
 * arguments of this function are taken from.
 */
template <unsigned RetPos, unsigned... ArgsPos>
struct Function : public std::function<void (void)> {

	template <class Input, class Output>
	Function(Input& inBuf, Output& outBuf, 
		const std::function<
			typename std::tuple_element<RetPos,Output>::type 
				( const typename std::tuple_element<ArgsPos, Input>::type&...)>& functor) :
	std::function<void (void)> 
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


/**
 * A Stage is 1 step in the pipeline. It contains a number of functions which are all insisting on
 * the same buffers. 
 */
struct Stage : public std::function<void (void)>  {

	template <class... Functors>
	Stage(const Functors&... func) : 
		std::function<void (void)>(std::bind(&lazy<Functors...>, func...)) { }
};

class Pipeline : public std::function<void (void)> {

	template <class... Stages>
	Pipeline(const Stages&... stages) : 
		std::function<void (void)>(std::bind(&lazy<Stages...>, stages...)) { }
};

} } // end insieme::utils namespace

