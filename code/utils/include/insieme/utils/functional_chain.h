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

namespace insieme {
namespace utils {

	namespace {

		template<typename ... Funs> struct chain_fun;

		template<>
		struct chain_fun<> {
			chain_fun() {};

			template<typename ... Params>
			void operator()(const Params& ... param) const { }
		};

		template<typename F, typename ... Funs>
		struct chain_fun<F,Funs...> {

			F f;
			chain_fun<Funs...> rest;

			chain_fun(const F& f, const Funs& ... fs) : f(f), rest(chain_fun<Funs...>(fs...)) {}

			template<typename ... Params>
			void operator()(const Params& ... param) const {
				f(param...);
				rest(param...);
			}
		};

		template<typename F1, typename ... Fs>
		chain_fun<F1,Fs...> make_chain(const F1& f1, const Fs& ... fs) {
			return chain_fun<F1,Fs...>(f1,fs...);
		}

	}


	template<typename ... Funs>
	chain_fun<Funs...> chain(const Funs& ... funs) {
		return make_chain(funs...);
	}

} // end namespace utils
} // end namespace insieme
