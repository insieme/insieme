/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/haskell/interface.h"

#include "../common/boolean_analysis_test.inc"

namespace insieme {
namespace analysis {

	/**
	 * Run the boolean value tests using the haskell backend.
	 */
	INSTANTIATE_TYPED_TEST_CASE_P(Haskell, BooleanValue, HaskellEngine);


	bool isTrue(const StatementAddress& stmt) {
		return isTrue<HaskellEngine>(stmt.as<ExpressionAddress>());
	}

	bool isFalse(const StatementAddress& stmt) {
		return isFalse<HaskellEngine>(stmt.as<ExpressionAddress>());
	}

	TEST(AdvancedBooleanAnalysis, FunctionReferences) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto stmt = builder.parseStmt(
				"def always = ()->bool {"
				"	return true;"
				"};"
				""
				"def never = ()->bool {"
				"	return false;"
				"};"
				""
				"{"
				"	always();"
				"	never();"
				"	"
				"	var ref<()->bool> f = always;"
				"	(*f)();"
				"	f = never;"
				"	(*f)();"
				"	"
				"	var ref<()->bool> g = ref_of_function(always);"
				"	(*g)();"
				""
				"}"
		).as<CompoundStmtPtr>();

		auto comp = CompoundStmtAddress(stmt);

		EXPECT_TRUE(isTrue(comp[0]));
		EXPECT_TRUE(isFalse(comp[1]));

		EXPECT_TRUE(isTrue(comp[3]));
		EXPECT_TRUE(isFalse(comp[5]));

		EXPECT_TRUE(isTrue(comp[7]));


	}

} // end namespace analysis
} // end namespace insieme

