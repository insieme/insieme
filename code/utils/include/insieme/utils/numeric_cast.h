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
#include <boost/type_traits/is_convertible.hpp>
#include <boost/type_traits/is_array.hpp>
#include <boost/type_traits/is_floating_point.hpp>

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
		// convert hexadecimal numbers
		if( in.compare(0, 2, "0x") == 0 || in.compare(0, 3, "-0x") == 0 || in.compare(0, 2, "0X") == 0 || in.compare(0, 3, "-0X") == 0 )
			return boost::lexical_cast<HexTo<RetTy>>( in );
		// convert octal numbers
		if( in.compare(0, 1, "0") == 0 || in.compare(0, 2, "-0") == 0)
			return boost::lexical_cast<OctTo<RetTy>>( in );

		// Now we check the suffix of the literal 
		bool isUnsigned = false;
		
		if((in[in.size()-2] == 'l' && in[in.size()-1] == 'l') || 
		   (in[in.size()-2] == 'L' && in[in.size()-1] == 'L') ) 
		{
			// treats as a long double
			return boost::lexical_cast<long long>(std::string(in.begin(), in.end()-2));
		}

		if(in[in.size()-2] == 'u' || in[in.size()-2] == 'U') 
		{
			isUnsigned = true;
		}

		if(in[in.size()-1] == 'l' || in[in.size()-1] == 'L') {
			// treats as a long double
			if (isUnsigned)
				return boost::lexical_cast<unsigned long>(std::string(in.begin(), in.end()-2));
			
			return boost::lexical_cast<long>(std::string(in.begin(), in.end()-1)); 
		}

		if(in[in.size()-1] == 'u' || in[in.size()-1] == 'U') {
			return boost::lexical_cast<unsigned int>(std::string(in.begin(), in.end()-1));
		}

		return boost::lexical_cast<RetTy>( in );
	}

};

/**
 * Called to convert from an input of string type (or string literal char [N]&) to an integral type
 */
template <class RetTy, class InTy>
struct numeric_cast_impl<RetTy, InTy, 2> {

	static RetTy convert(const std::string& in) {

		if(in[in.size()-1] == 'f' || in[in.size()-1] == 'F') {
			// treats as a float type
			return boost::lexical_cast<float>(std::string(in.begin(), in.end()-1));
		}

		return boost::lexical_cast<RetTy>(in);
	}

};

template <class RetTy, class InTy>
struct numeric_cast_impl<RetTy, InTy, 2>;

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
				boost::mpl::bool_<boost::is_same<std::string, InTy>::value || boost::is_convertible<InTy,std::string>::value>,
				typename boost::mpl::if_<boost::is_integral<RetTy>,
					// call the function which handle integral digits
					boost::mpl::integral_c<size_t,1>,
					typename boost::mpl::if_<boost::is_floating_point<RetTy>,
						// call the function which handle floating digits
						boost::mpl::integral_c<size_t,2>,
						// otherwise boost::lexical_cast will be used
						boost::mpl::integral_c<size_t,0>
					>::type
				>::type,
				// use boost::lexical_cast for everything else
				boost::mpl::integral_c<size_t,0>
			>::type::value>::convert(in);

}

} // End utils namespace
} // End insieme namespace
