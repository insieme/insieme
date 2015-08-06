/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#include <gtest/gtest.h>

#include "insieme/utils/constraint/constraints.h"

namespace insieme {
namespace utils {
namespace constraint {

TEST(Constraint, Basic) {

	typedef TypedSetVariable<int> Set;
	
	// simple set tests
	Set a = 1;
	Set b = 2;
	Set c = 3;
	
	EXPECT_EQ("v1", toString(a));
	
	EXPECT_EQ("v1 sub v2", toString(*subset(a,b)));
	
	EXPECT_EQ("5 in v1 => v2 sub v3", toString(*subsetIf(5,a,b,c)));
	
	EXPECT_EQ("|v1| > 5 => v2 sub v3", toString(*subsetIfBigger(a,5,b,c)));
	
	// constraint test
	Constraints problem = {
		elem(3, a),
		subset(a,b),
		subsetIf(5,a,b,c),
		subsetIfBigger(a,5,b,c),
		subsetIfReducedBigger(a, 3, 2, b, c)
	};
	
	EXPECT_EQ("{3 in v1,v1 sub v2,5 in v1 => v2 sub v3,|v1| > 5 => v2 sub v3,|v1 - {3}| > 2 => v2 sub v3}", toString(problem));
	
}


} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
