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

#include "insieme/analysis/dfa/problem.h"

namespace insieme {
namespace analysis {
namespace dfa {

typename ConstantPropagation::Base::value_type 
ConstantPropagation::meet(const typename ConstantPropagation::Base::value_type& lhs, 
						  const typename ConstantPropagation::Base::value_type& rhs) const 
{
	typedef typename ConstantPropagation::Base::value_type element_type;
	typedef dfa::Value<core::LiteralPtr> ConstantType;

	std::cout << "Meet (" << lhs << ", " << rhs << ") -> " << std::flush;
	
	/** 
	 * Given 2 dataflow values associated to a variable, returns the new dataflow value 
	 * after the meet operator is applied according to the following table:
	 *
	 * ---------------------
	 * TOP ^ x      = x 
	 * TOP ^ TOP    = TOP
	 * x   ^ y      = BOTTOM
	 * x   ^ BOTTOM = BOTTOM
	 * x   ^ x      = x 
	 *----------------------
	 */
	auto eval = [](const ConstantType& lhs, const ConstantType& rhs) -> ConstantType {

		if (lhs == dfa::top) { return rhs; }
		if (rhs == dfa::top) { return lhs; }

		if (lhs == dfa::bottom || rhs == dfa::bottom) { 
			return dfa::bottom; 
		}

		if (lhs == rhs ) { return lhs; }

		return dfa::bottom;
	};

	element_type ret;
	element_type::const_iterator lhs_it = lhs.begin(), rhs_it = rhs.begin(), it, end;

	while(lhs_it != lhs.end() && rhs_it != rhs.end()) {
		if(std::get<0>(*lhs_it) == std::get<0>(*rhs_it)) {
			ret.insert( std::make_tuple(std::get<0>(*lhs_it), 
						eval(std::get<1>(*lhs_it), std::get<1>(*rhs_it))) 
					  );
			++lhs_it; ++rhs_it;
			continue;
		}
		if (*lhs_it < *rhs_it) { ret.insert( *(lhs_it++) ); continue; }
		if (*lhs_it > *rhs_it) { ret.insert( *(rhs_it++) ); continue; }
	}

	// Take care of the remaining elements which have to be written back to the result 
	std::tie(it,end) = lhs_it == lhs.end() ? 
						 std::make_tuple(rhs_it, rhs.end()) : 
						 std::make_tuple(lhs_it, lhs.end());

	while( it != end) { ret.insert( *(it++) ); }
	std::cout << ret << std::endl;

	return ret;
}


} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
