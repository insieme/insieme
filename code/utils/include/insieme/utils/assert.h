/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

/**
* This header file defines a set of macros to define more readable and flexible assertions within
* program code. Also, macros supporting the declaration of variables only required for checking
* assertions are supported. As all assertions, in case the macro NDEBUG is defined, they will be
* ignored. In those cases, variables declared using the 'assert_decl' macro will not be declared.
*/

#include <iostream>
#include <functional>

#include "insieme/utils/debug/backtrace.h"

#define __xstr(a) __insieme_utils_str(a)
#define __insieme_utils_str(a) #a

#if defined(NDEBUG)

#define _assert_ignore                                                                                                                                         \
        if(false) std::cerr << ""

#define assert_decl(_DECL) ((void)0)
#define assert_true(_COND) _assert_ignore
#define assert_eq(_a, _b) _assert_ignore
#define assert_ne(_a, _b) _assert_ignore
#define assert_lt(_a, _b) _assert_ignore
#define assert_le(_a, _b) _assert_ignore
#define assert_gt(_a, _b) _assert_ignore
#define assert_ge(_a, _b) _assert_ignore
#define assert_fail() _assert_ignore
#define assert_pred1(_a, _b) _assert_ignore
#define assert_not_pred1(_a, _b) _assert_ignore
#define assert_pred2(_a, _b, _c) _assert_ignore
#define assert_not_pred2(_a, _b, _c) _assert_ignore

#else
#include <iostream>

#include "insieme/utils/unused.h"

namespace insieme {
namespace utils {

	// allows to provide a lambda that adds printed information when an assertion occurs
	void setAssertExtraInfoPrinter(std::function<void(void)> printer);
	// clears the extra information printer lambda
	void clearAssertExtraInfoPrinter();

	namespace detail {

		extern thread_local std::function<void(void)> extraAssertInformationPrinter;

		struct LazyAssertion {
			bool value;
			LazyAssertion(bool value) : value(value) {}
			~LazyAssertion();
			operator bool() const {
				return !value;
			}
		};

	} // end namespace detail
} // end namespace utils
} // end namespace insieme

#define assert_decl(_DECL) _DECL

#define assert_true(_COND)                                                                                                                                     \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((bool)(_COND)))                                                                       \
	std::cerr << "\nAssertion " #_COND " of " __FILE__ ":" __xstr(__LINE__) " failed!\n"

#define assert_eq(_A, _B)                                                                                                                                      \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((_A) == (_B)))                                                                        \
	std::cerr << "\nAssertion " #_A " == " #_B " of " __FILE__ ":" __xstr(__LINE__) " failed!\n\t" #_A " = " << (_A) << "\n\t" #_B " = " << (_B) << "\n"

#define assert_ne(_A, _B)                                                                                                                                      \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((_A) != (_B)))                                                                        \
	std::cerr << "\nAssertion " #_A " != " #_B " of " __FILE__ ":" __xstr(__LINE__) " failed!\n\t" #_A " = " << (_A) << "\n\t" #_B " = " << (_B) << "\n"

#define assert_lt(_A, _B)                                                                                                                                      \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((_A) < (_B)))                                                                         \
	std::cerr << "\nAssertion " #_A " < " #_B " of " __FILE__ ":" __xstr(__LINE__) " failed!\n\t" #_A " = " << (_A) << "\n\t" #_B " = " << (_B) << "\n"

#define assert_le(_A, _B)                                                                                                                                      \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((_A) <= (_B)))                                                                        \
	std::cerr << "\nAssertion " #_A " <= " #_B " of " __FILE__ ":" __xstr(__LINE__) " failed!\n\t" #_A " = " << (_A) << "\n\t" #_B " = " << (_B) << "\n"

#define assert_gt(_A, _B)                                                                                                                                      \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((_A) > (_B)))                                                                         \
	std::cerr << "\nAssertion " #_A " > " #_B " of " __FILE__ ":" __xstr(__LINE__) " failed!\n\t" #_A " = " << (_A) << "\n\t" #_B " = " << (_B) << "\n"

#define assert_ge(_A, _B)                                                                                                                                      \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((_A) >= (_B)))                                                                        \
	std::cerr << "\nAssertion " #_A " >= " #_B " of " __FILE__ ":" __xstr(__LINE__) " failed!\n\t" #_A " = " << (_A) << "\n\t" #_B " = " << (_B) << "\n"

#define assert_fail()                                                                                                                                          \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion(false)) std::cerr << "\nAssertion failed in " __FILE__ ":" __xstr(__LINE__) " - "

#define assert_pred1(_P, _A)                                                                                                                                   \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((bool)((_P)(_A))))                                                                    \
	std::cerr << "\nAssertion " #_P "(" #_A ") with " #_A " = " << (_A) << " in " __FILE__ ":" __xstr(__LINE__) " failed!\n"

#define assert_not_pred1(_P, _A)                                                                                                                               \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion(!(bool)((_P)(_A))))                                                                   \
	std::cerr << "\nAssertion !" #_P "(" #_A ") with " #_A " = " << (_A) << " in " __FILE__ ":" __xstr(__LINE__) " failed!\n"

#define assert_pred2(_P, _A, _B)                                                                                                                               \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion((bool)((_P)(_A, _B))))                                                                \
	std::cerr << "\nAssertion " #_P "(" #_A ", " #_B ") with\n " #_A " = " << (_A) << "\n " #_B " = " << (_B)                                                  \
	          << "\n in " __FILE__ ":" __xstr(__LINE__) " failed!\n"

#define assert_not_pred2(_P, _A, _B)                                                                                                                           \
	if(__insieme_unused auto __x = insieme::utils::detail::LazyAssertion(!(bool)((_P)(_A, _B))))                                                               \
	std::cerr << "\nAssertion !" #_P "(" #_A ", " #_B ") with\n " #_A " = " << (_A) << "\n " #_B " = " << (_B)                                                 \
	          << "\n in " __FILE__ ":" __xstr(__LINE__) " failed!\n"

#endif

// ------ derived definitions ------

#define assert_false(_COND) assert_true(!(_COND))
#define assert_not_implemented() assert_fail() << "Not implemented functionality in " __FILE__ ":" __xstr(__LINE__) "\n"
