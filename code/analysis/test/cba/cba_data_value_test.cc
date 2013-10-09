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

#include <gtest/gtest.h>

#include "insieme/analysis/cba/framework/data_value.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(DataValue, ElementData) {

		// just some simple data path handling

		typedef data_ptr<int>::type Data;

		Data a = elements(1,2,3);
		Data b = elements(3,4);
		Data c = element(0);

		EXPECT_EQ("{1,2,3}", toString(a));
		EXPECT_EQ("{3,4}", toString(b));
		EXPECT_EQ("{0}", toString(c));

	}

	TEST(DataValue, StructData) {

		// just some simple data path handling

		typedef data_ptr<int>::type Data;

		NodeManager mgr;
		IRBuilder builder(mgr);

		StringValuePtr nA = builder.stringValue("a");
		StringValuePtr nB = builder.stringValue("b");

		Data a = structData(member(nA, elements(0)), member(nB, elements(1,2)));
		Data b = structData(member(nB, elements(0)), member(nA, elements(1,2)));
		Data c = structData(member(nB, elements(1,2)), member(nA, elements(0)));

		EXPECT_EQ("{a={0},b={1,2}}", toString(a));
		EXPECT_EQ("{a={1,2},b={0}}", toString(b));
		EXPECT_EQ("{a={0},b={1,2}}", toString(c));

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
