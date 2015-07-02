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

#include <gtest/gtest.h>

#include <fstream>

#include "insieme/transform/cba_based/constant_propagation.h"

#include "insieme/core/ir_builder.h"

#include "insieme/analysis/cba/analysis.h"

namespace insieme {
namespace transform {
namespace cba_based {


	TEST(CBA_Transform, ConstProp) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto code = builder.parseStmt(
			"{"
			"	let int = int<4>;"
			"	decl ref<int> a = var(3);"
			"	decl ref<int> b = var(9 + (a * 5));"
			"	decl ref<int> c;"
			"	"
			"	c = b * 4;"
			"	if (c > 10) {"
			"		c = c - 10;"
			"	}"
			"	return c * (60 + a);"
			"}"
		);

		EXPECT_TRUE(code);

		// run constant-propagation on this code fragment
		auto res = propagateConstants(core::NodeAddress(code)).getRootNode();

		EXPECT_PRED2(containsSubString, toString(res), "if(true)");
		EXPECT_PRED2(containsSubString, toString(res), "return 5418;");
	}


} // end namespace filter
} // end namespace transform
} // end namespace insieme


