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

#include <boost/lexical_cast.hpp>
#include <boost/type_traits/is_integral.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_convertible.hpp>
#include <boost/type_traits/is_array.hpp>
#include <boost/type_traits/is_floating_point.hpp>

#include "insieme/utils/character_escaping.h"

namespace {
	template <typename ElemT>
	struct HexTo {
		ElemT value;
		operator ElemT() const {
			return value;
		}
		friend std::istream& operator>>(std::istream& in, HexTo& out) {
			in >> std::hex >> out.value;
			return in;
		}
	};

	template <typename ElemT>
	struct OctTo {
		ElemT value;
		operator ElemT() const {
			return value;
		}
		friend std::istream& operator>>(std::istream& in, OctTo& out) {
			in >> std::oct >> out.value;
			return in;
		}
	};

	template <class RetTy, class InTy, int Case>
	struct numeric_cast_impl {
		static RetTy convert(const std::string& in);
	};

	template <class RetTy, class InTy>
	struct numeric_cast_impl<RetTy, InTy, 0> {
		static RetTy convert(const InTy& in) {
			return boost::lexical_cast<RetTy>(in);
		}
	};

	/**
	 * Called to convert from an input of string type (or string literal char [N]&) to an integral type
	 */
	template <class RetTy, class InTy>
	struct numeric_cast_impl<RetTy, InTy, 1> {
		static RetTy convert(const std::string& in) {
			// if character
			if(in.front() == '\'' && in.back() == '\'') {
				assert_true(in.size() == 3 || in.size() == 4) << "Expected single char within string, but length was " << in.size();
				return insieme::utils::escapedStringToChar(in.substr(1, in.size()-2));
			}
			// special handling for 0u, 0ul and '\0'
			if(in == "0u" || in == "0ul" || in == "0l" || in == "0ll" || in == "0ull") { return static_cast<RetTy>(0); }
			// special handling for -0u and -0ul and -0l
			if(in == "-0u" || in == "-0ul" || in == "-0l" || in == "-0ll" || in == "-0ull") { return static_cast<RetTy>(0); }

			// convert hexadecimal numbers
			if(in.compare(0, 2, "0x") == 0 || in.compare(0, 3, "-0x") == 0 || in.compare(0, 2, "0X") == 0 || in.compare(0, 3, "-0X") == 0) {
				return boost::lexical_cast<HexTo<RetTy>>(in);
			}
			// convert octal numbers
			if(in.compare(0, 1, "0") == 0 || in.compare(0, 2, "-0") == 0) { return boost::lexical_cast<OctTo<RetTy>>(in); }

			// Now we clear the suffix of the literal
			const char* str = in.c_str();
			std::size_t size = in.size();
			while(size > 0 && (str[size - 1] == 'l' || str[size - 1] == 'L' || str[size - 1] == 'u' || str[size - 1] == 'U')) {
				size--;
			}

			// get cleared value
			std::string cleared(str, str + size);

			// convert to target type
			return boost::lexical_cast<RetTy>(cleared);
		}
	};

	/**
	 * Called to convert from an input of string type (or string literal char [N]&) to an integral type
	 */
	template <class RetTy, class InTy>
	struct numeric_cast_impl<RetTy, InTy, 2> {
		static RetTy convert(const std::string& in) {
			if(in[in.size() - 1] == 'f' || in[in.size() - 1] == 'F') {
				// treats as a float type
				return boost::lexical_cast<float>(std::string(in.begin(), in.end() - 1));
			}

			return boost::lexical_cast<RetTy>(in);
		}
	};

	template <class RetTy, class InTy>
	struct numeric_cast_impl<RetTy, InTy, 2>;

	/**
	 * We are overloading the conversion of float and double literals to append the trailing '.0' if it is not there.
	 */
	template <>
	struct numeric_cast_impl<std::string, double, 0> {
		static std::string convert(double d) {
			auto res = boost::lexical_cast<std::string>(d);
			if(find(res.begin(), res.end(), '.') == res.end()) { return res + ".0"; }
			return res;
		}
	};
	template <>
	struct numeric_cast_impl<std::string, float, 0> {
		static std::string convert(float f) {
			return numeric_cast_impl<std::string, double, 0>::convert(f);
		}
	};


} // end anonymous namespace

namespace insieme {
namespace utils {

	template <class RetTy, class InTy>
	RetTy numeric_cast(const InTy& in) {
		return numeric_cast_impl<RetTy, InTy,
		                         boost::mpl::if_<
		                             // if the input if a string or a char literal we have to overwrite the boost::lexical_cast behavior
		                             // in a way hexadecimal and octal integer digits are correctly handled and furthermore, the floating point
		                             // suffixes f, F, l, L as well
		                             boost::mpl::bool_<boost::is_same<std::string, InTy>::value || boost::is_convertible<InTy, std::string>::value>,
		                             typename boost::mpl::if_<boost::is_integral<RetTy>,
		                                                      // call the function which handle integral digits
		                                                      boost::mpl::integral_c<size_t, 1>,
		                                                      typename boost::mpl::if_<boost::is_floating_point<RetTy>,
		                                                                               // call the function which handle floating digits
		                                                                               boost::mpl::integral_c<size_t, 2>,
		                                                                               // otherwise boost::lexical_cast will be used
		                                                                               boost::mpl::integral_c<size_t, 0>>::type>::type,
		                             // use boost::lexical_cast for everything else
		                             boost::mpl::integral_c<size_t, 0>>::type::value>::convert(in);
	}

} // End utils namespace
} // End insieme namespace
