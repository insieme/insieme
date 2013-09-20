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

namespace insieme {
namespace utils {

	namespace {

		template<typename F1, typename F2, typename ... Params>
		struct chain_fun {
			F1 f1; F2 f2;

			chain_fun(const F1& f1, const F2& f2) : f1(f1), f2(f2) {}

			void operator()(const Params& ... param) const {
				f1(param...);
				f2(param...);
			}
		};


		template<typename F1, typename F2, typename R, typename C, typename ... Params>
		chain_fun<F1,F2,Params...> make_chain(const F1& f1, const F2& f2, R(C::* dummy)(Params...)) {
			return chain_fun<F1,F2,Params...>(f1,f2);
		}

		template<typename F1, typename F2, typename R, typename C, typename ... Params>
		chain_fun<F1,F2,Params...> make_chain(const F1& f1, const F2& f2, R(C::* dummy)(Params...) const) {
			return chain_fun<F1,F2,Params...>(f1,f2);
		}

		template<typename F1, typename F2, typename F = decltype(&F1::operator())>
		auto make_chain(const F1& f1, const F2& f2) -> decltype(make_chain(f1, f2, (F)(0))) {
			return make_chain(f1,f2, &F1::operator());
		}

	}


	template<typename F1>
	F1 chain(const F1& f1) { return f1; }

	template<typename F1, typename F2, typename ... Rest>
	auto chain(const F1& f1, const F2& f2, const Rest& ... rest) -> decltype(make_chain(f1,chain(f2, rest...))) {
		return make_chain(f1,chain(f2, rest...));
	}

} // end namespace utils
} // end namespace insieme
