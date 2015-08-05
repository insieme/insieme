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

/**
 * A type trait to determine whether a given type T can be printed to some output stream.
 */
template<typename T>
class is_printable {
	typedef char yes;
	typedef long no;
	
	template<class E> static E& lvalue_of_type();
	template<class E> static E  rvalue_of_type();
	
	// two implementations of the test - as templates to utilize the SFINA mechanism
	//   - the first one will only be supported in cases where T is printable
	//   - the latter is always supported, yet of lower priority in the overloading
	template <typename C> static yes test(char(*)[sizeof(lvalue_of_type<std::ostream>() << rvalue_of_type<C>())]);
	template <typename C> static no  test(...);
	
public:

	// define the resulting constant by testing which test method is called
	enum { value = sizeof(test<T>(0)) == sizeof(yes) };
	
};

} // end namespace utils
} // end namespace insieme
