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

#include <boost/lexical_cast.hpp>
#include <boost/type_traits/is_integral.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_array.hpp>

namespace {
template <typename ElemT>
struct HexTo {
    ElemT value;
    operator ElemT() const { return value;}
    friend std::istream& operator>>(std::istream& in, HexTo& out) {
        in >> std::hex >> out.value;
        return in;
    }
};

template <typename ElemT>
struct OctTo {
    ElemT value;
    operator ElemT() const { return value;}
    friend std::istream& operator>>(std::istream& in, OctTo& out) {
        in >> std::oct >> out.value;
        return in;
    }
};
}

namespace insieme {
namespace utils {

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

template <class RetTy>
struct numeric_cast_impl<RetTy, std::string, 1> {

	static RetTy convert(const std::string& in) {
		// convert hexadecimal numbers
		if( in.compare(0, 2, "0x") == 0 || in.compare(0, 3, "-0x") == 0 || in.compare(0, 2, "0X") == 0 || in.compare(0, 3, "-0X") == 0 )
			return boost::lexical_cast<HexTo<RetTy>>( in );
		// convert octal numbers
		if( in.compare(0, 1, "0") == 0 || in.compare(0, 2, "-0") == 0)
			return boost::lexical_cast<OctTo<RetTy>>( in );
		return boost::lexical_cast<RetTy>( in );
	}

};

template <class RetTy, class InTy>
struct numeric_cast_impl<RetTy, InTy, 2>;

template <class RetTy, class InTy>
RetTy numeric_cast(const InTy& in) {
	return numeric_cast_impl<RetTy, InTy,
			boost::mpl::if_<
				boost::is_array<InTy>,
				boost::mpl::integral_c<size_t,2>,
					typename boost::mpl::if_<
					 	boost::mpl::bool_<boost::is_integral<RetTy>::value && boost::is_same<std::string, InTy>::value>,
							boost::mpl::integral_c<size_t,1>,
							boost::mpl::integral_c<size_t,0>>::type>::type::value>::convert(in);

}

template <class RetTy, class InTy>
struct numeric_cast_impl<RetTy, InTy, 2> {

	static RetTy convert(const InTy& in) {
		return numeric_cast<RetTy>(std::string(in));
	}

};

//char numeric_cast(const std::string& in) { return numeric_cast<short>(in); }
// unsigned char numeric_cast(const std::string& in) { return numeric_cast<unsigned short>(in, 0); }

} // End utils namespace
} // End insieme namespace
