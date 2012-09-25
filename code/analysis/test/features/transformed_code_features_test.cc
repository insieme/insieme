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

#include "insieme/analysis/features/code_features.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/transform/polyhedral/transformations.h"
#include "insieme/transform/rulebased/transformations.h"

namespace insieme {
namespace analysis {
namespace features {

	using namespace core;

	TEST(FeatureAggregation, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		// load some code sample ...
		auto forStmt = builder.parseStmt(
				"for( int<4> i = 10 .. 50 : 1) {"
				"	i%1;"
				"	for(int<4> j = 5 .. 25 : 1) {"
				"       i%j;"
				"	}"
				"}").as<ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		insieme::transform::TransformationPtr t;
		core::NodePtr before, after;

		// try unrolling
		t = insieme::transform::rulebased::makeLoopUnrolling(4);

		before = forStmt;
		after = t->apply(NodePtr(forStmt));

		EXPECT_EQ((100+1)*100, countOps(before, basic.getSignedIntMod(), FA_Weighted));
		EXPECT_EQ(((100+1)*100)*2, countOps(after, basic.getSignedIntMod(), FA_Weighted));

		EXPECT_EQ((20+1)*40, countOps(before, basic.getSignedIntMod(), FA_Real));
		EXPECT_EQ((20+1)*40, countOps(after, basic.getSignedIntMod(), FA_Real));

		EXPECT_EQ((20+1)*40, countOps(before, basic.getSignedIntMod(), FA_Polyhedral));
		EXPECT_EQ((20+1)*40, countOps(after, basic.getSignedIntMod(), FA_Polyhedral));


		// try loop interchange
		t = insieme::transform::polyhedral::makeLoopInterchange(0,1);

		before = forStmt;
		after = t->apply(NodePtr(forStmt));

		// loop interchange should not change any metric
		EXPECT_EQ((100+1)*100, countOps(before, basic.getSignedIntMod(), FA_Weighted));
		EXPECT_EQ((100+1)*100, countOps(after, basic.getSignedIntMod(), FA_Weighted));

		EXPECT_EQ((20+1)*40, countOps(before, basic.getSignedIntMod(), FA_Real));
		EXPECT_EQ((20+1)*40, countOps(after, basic.getSignedIntMod(), FA_Real));

		EXPECT_EQ((20+1)*40, countOps(before, basic.getSignedIntMod(), FA_Polyhedral));
		EXPECT_EQ((20+1)*40, countOps(after, basic.getSignedIntMod(), FA_Polyhedral));


		// try loop interchange
		t = insieme::transform::polyhedral::makeLoopStripMining(1, 10);

		before = forStmt;
		after = t->apply(NodePtr(forStmt));

		// this should make some metrics unusable (since some loop boundaries contain variables now)
		EXPECT_EQ((100+1)*100, countOps(before, basic.getSignedIntMod(), FA_Weighted));
		EXPECT_EQ((100*100/10+1)*100, countOps(after, basic.getSignedIntMod(), FA_Weighted));

		EXPECT_EQ((20+1)*40, countOps(before, basic.getSignedIntMod(), FA_Real));
		EXPECT_EQ((20*100/10+1)*40, countOps(after, basic.getSignedIntMod(), FA_Real));

		EXPECT_EQ((20+1)*40, countOps(before, basic.getSignedIntMod(), FA_Polyhedral));
		EXPECT_EQ((20+1)*40, countOps(after, basic.getSignedIntMod(), FA_Polyhedral));

		// get number of operations

	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
