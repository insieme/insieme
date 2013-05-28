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

#include "insieme/analysis/polyhedral/scop.h"

namespace insieme {
namespace analysis {
namespace features {

	bool isFormula(const core::ExpressionPtr& expr) {
		try {
			core::arithmetic::toFormula(expr);
			return true;
		} catch (const core::arithmetic::NotAFormulaException& nafe) {}
		return false;
	}

	using namespace core;

	TEST(FeatureAggregation, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

		// load some code sample ...
		auto forStmt = builder.parseStmt(
			"for(uint<4> i = 10u .. 50u : 1u) {"
			"	v[i];"
			"	for(uint<4> j = 5u .. 25u : 1u) {"
			"		if ( j < 10u ) {"
			"			v[i+j]; v[i+j];"
			"		} else {"
			"			v[i-j]; v[i-j];"
			"		};"
			"	};"
			"}", symbols).as<ForStmtPtr>();

		EXPECT_TRUE(forStmt);
//		EXPECT_TRUE(scop::ScopRegion::toScop(forStmt));

		EXPECT_PRED1(isFormula, forStmt->getStart());
		EXPECT_PRED1(isFormula, forStmt->getEnd());
		EXPECT_PRED1(isFormula, forStmt->getStep());

		// check number of various ops
		EXPECT_EQ(0, countOps(forStmt, basic.getSignedIntAdd()));
		EXPECT_EQ(20100, countOps(forStmt, basic.getArrayRefElem1D()));
		EXPECT_EQ(10000, countOps(forStmt, basic.getUnsignedIntAdd()));
		EXPECT_EQ(10000, countOps(forStmt, basic.getUnsignedIntSub()));

		// check the three types of aggregators
		EXPECT_EQ(5, countOps(forStmt, basic.getArrayRefElem1D(), FA_Static));
		EXPECT_EQ(2, countOps(forStmt, basic.getUnsignedIntAdd(), FA_Static));
		EXPECT_EQ(2, countOps(forStmt, basic.getUnsignedIntSub(), FA_Static));

		EXPECT_EQ(2*100*100 + 100, 	countOps(forStmt, basic.getArrayRefElem1D(), FA_Weighted));
		EXPECT_EQ(2/2*100*100, 		countOps(forStmt, basic.getUnsignedIntAdd(), FA_Weighted));
		EXPECT_EQ(2/2*100*100, 		countOps(forStmt, basic.getUnsignedIntSub(), FA_Weighted));

		EXPECT_EQ(2*20*40 + 40, 	countOps(forStmt, basic.getArrayRefElem1D(), FA_Real));
		EXPECT_EQ(2/2*20*40, 		countOps(forStmt, basic.getUnsignedIntAdd(), FA_Real));
		EXPECT_EQ(2/2*20*40, 		countOps(forStmt, basic.getUnsignedIntSub(), FA_Real));

		EXPECT_EQ(2*20*40 + 40,		countOps(forStmt, basic.getArrayRefElem1D(), FA_Polyhedral));
		EXPECT_EQ(2*5*40, 			countOps(forStmt, basic.getUnsignedIntAdd(), FA_Polyhedral));
		EXPECT_EQ(2*15*40, 			countOps(forStmt, basic.getUnsignedIntSub(), FA_Polyhedral));
	}

	TEST(FeatureAggregation, Complex) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
		symbols["M"] = builder.variable(builder.parseType("uint<4>"));
		symbols["N"] = builder.variable(builder.parseType("uint<4>"));

		// load some code sample ...
		auto forStmt = builder.parseStmt(
			"for(uint<4> i = 10u .. M : 1u) {"
			"	v[i];"
			"	for(uint<4> j = 5u .. N : 1u) {"
			"		if ( j < 10u ) {"
			"			v[i+j]; v[i+j];"
			"		} else {"
			"			v[i-j]; v[i-j];"
			"		}"
			"	}"
			"}", symbols).as<ForStmtPtr>();


		EXPECT_TRUE(forStmt);
//		EXPECT_TRUE(scop::ScopRegion::toScop(forStmt));

		EXPECT_PRED1(isFormula, forStmt->getStart());
		EXPECT_PRED1(isFormula, forStmt->getEnd());
		EXPECT_PRED1(isFormula, forStmt->getStep());

		// check number of various ops
		EXPECT_EQ(0, countOps(forStmt, basic.getSignedIntAdd()));
		EXPECT_EQ(20100, countOps(forStmt, basic.getArrayRefElem1D()));
		EXPECT_EQ(10000, countOps(forStmt, basic.getUnsignedIntAdd()));
		EXPECT_EQ(10000, countOps(forStmt, basic.getUnsignedIntSub()));

		// check the three types of aggregators
		EXPECT_EQ(5, countOps(forStmt, basic.getArrayRefElem1D(), FA_Static));
		EXPECT_EQ(2, countOps(forStmt, basic.getUnsignedIntAdd(), FA_Static));
		EXPECT_EQ(2, countOps(forStmt, basic.getUnsignedIntSub(), FA_Static));

		EXPECT_EQ(2*100*100 + 100, 	countOps(forStmt, basic.getArrayRefElem1D(), FA_Weighted));
		EXPECT_EQ(2/2*100*100, 		countOps(forStmt, basic.getUnsignedIntAdd(), FA_Weighted));
		EXPECT_EQ(2/2*100*100, 		countOps(forStmt, basic.getUnsignedIntSub(), FA_Weighted));

		EXPECT_EQ(2*100*100 + 100, 	countOps(forStmt, basic.getArrayRefElem1D(), FA_Real));
		EXPECT_EQ(2/2*100*100, 		countOps(forStmt, basic.getUnsignedIntAdd(), FA_Real));
		EXPECT_EQ(2/2*100*100, 		countOps(forStmt, basic.getUnsignedIntSub(), FA_Real));

		EXPECT_EQ(2*5*90+2*90*90+90,	countOps(forStmt, basic.getArrayRefElem1D(), FA_Polyhedral));
		EXPECT_EQ(2*5*90, 				countOps(forStmt, basic.getUnsignedIntAdd(), FA_Polyhedral));
		EXPECT_EQ(2*90*90,	 			countOps(forStmt, basic.getUnsignedIntSub(), FA_Polyhedral));
	}

	TEST(OperatorStatistics, Basic) {

		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		auto op1 = basic.getRefDeref();
		auto op2 = basic.getRefAlloc();

		OperatorStatistic stat1;

		// should be empty
		EXPECT_TRUE(stat1.empty());

		// add something
		stat1[op1] = 3;
		EXPECT_EQ(1u, stat1.size());

		EXPECT_EQ(3u, stat1[op1]);
		EXPECT_EQ(0u, stat1[op2]);

		stat1 *= 2;

		EXPECT_EQ(6u, stat1[op1]);
		EXPECT_EQ(0u, stat1[op2]);

		stat1 = stat1 * (1.0/3);

		EXPECT_EQ(2u, stat1[op1]);
		EXPECT_EQ(0u, stat1[op2]);

		OperatorStatistic stat2;
		stat2[op2] = 4;
		EXPECT_EQ(0u, stat2[op1]);
		EXPECT_EQ(4u, stat2[op2]);

		stat2[op1] = 5;
		EXPECT_EQ(5u, stat2[op1]);
		EXPECT_EQ(4u, stat2[op2]);


		OperatorStatistic tmp = stat1 + stat2;
		EXPECT_EQ(7u, tmp[op1]);
		EXPECT_EQ(4u, tmp[op2]);

	}


	TEST(OperatorStatistic, Extractor) {

		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

		// load some code sample ...
		auto forStmt = builder.parseStmt(
			"for(uint<4> i = 10u .. 50u : 1u) {"
			"	v[i];"
			"	for(uint<4> j = 5u .. 25u : 1u) {"
			"		if ( j < 10u ) {"
			"			v[i+j]; v[i+j];"
			"		} else {"
			"			v[i-j]; v[i-j];"
			"		};"
			"	};"
			"}", symbols).as<ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		auto op1 = basic.getArrayRefElem1D();
		auto op2 = basic.getUnsignedIntAdd();
		auto op3 = basic.getUnsignedIntSub();
		auto op4 = basic.getRefNull();

		auto res = getOpStats(forStmt, FA_Static);

		EXPECT_EQ(res[op1], countOps(forStmt, op1, FA_Static));
		EXPECT_EQ(res[op2], countOps(forStmt, op2, FA_Static));
		EXPECT_EQ(res[op3], countOps(forStmt, op3, FA_Static));
		EXPECT_EQ(res[op4], countOps(forStmt, op4, FA_Static));


		res = getOpStats(forStmt, FA_Weighted);

		EXPECT_EQ(res[op1], countOps(forStmt, op1, FA_Weighted));
		EXPECT_EQ(res[op2], countOps(forStmt, op2, FA_Weighted));
		EXPECT_EQ(res[op3], countOps(forStmt, op3, FA_Weighted));
		EXPECT_EQ(res[op4], countOps(forStmt, op4, FA_Weighted));

		res = getOpStats(forStmt, FA_Real);

		EXPECT_EQ(res[op1], countOps(forStmt, op1, FA_Real));
		EXPECT_EQ(res[op2], countOps(forStmt, op2, FA_Real));
		EXPECT_EQ(res[op3], countOps(forStmt, op3, FA_Real));
		EXPECT_EQ(res[op4], countOps(forStmt, op4, FA_Real));

	}

	TEST(FeatureValues, Basic) {

		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		auto op1 = basic.getRefDeref();
		auto op2 = basic.getRefNew();

		FeatureValues v1;

		// should be empty
		EXPECT_TRUE(v1.empty());

		// add something
		v1.push_back(3);
		EXPECT_EQ("[3]", toString(v1));

		v1 *= 2;
		EXPECT_EQ("[6]", toString(v1));

		v1 = v1 * (1.0/3);
		EXPECT_EQ("[2]", toString(v1));

		FeatureValues v2;
		v2.push_back(4);
		EXPECT_EQ("[4]", toString(v2));
		EXPECT_EQ("[6]", toString(v1 + v2));

		v2.push_back(7);
		EXPECT_EQ("[4,7]", toString(v2));
		EXPECT_EQ("[6,7]", toString(v1 + v2));

	}

	TEST(CodeFeatures, SimpleFeatures) {

		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

		// load some code sample ...
		auto forStmt = builder.parseStmt(
			"for(uint<4> i = 10u .. 50u : 1u) {"
			"	v[i];"
			"	for(uint<4> j = 5u .. 25u : 1u) {"
			"		if ( j < 10u ) {"
			"			v[i+j]; v[i+j];"
			"		} else {"
			"			v[i-j]; v[i-j];"
			"		};"
			"	};"
			"}", symbols).as<ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		core::ExpressionPtr intAdd = basic.getSignedIntAdd();
		core::ExpressionPtr intSub = basic.getSignedIntSub();

		core::ExpressionPtr uintAdd = basic.getUnsignedIntAdd();
		core::ExpressionPtr uintSub = basic.getUnsignedIntSub();

		// try a single feature
		auto featureA = SimpleCodeFeatureSpec(toVector(uintAdd, uintSub), FA_Static);
		EXPECT_EQ(countOps(forStmt, basic.getUnsignedIntAdd(), FA_Static) + countOps(forStmt, basic.getUnsignedIntSub(), FA_Static), evalFeature(forStmt, featureA));


		// try a list of features
		auto featureB = SimpleCodeFeatureSpec(toVector(intAdd, intSub));
		auto features = toVector(featureA, featureB);

		// try multiple features
		vector<simple_feature_value_type> values = evalFeatures(forStmt, features);

		ASSERT_EQ(features.size(), values.size());

		EXPECT_EQ(evalFeature(forStmt, featureA), values[0]);
		EXPECT_EQ(evalFeature(forStmt, featureB), values[1]);



		// test multiple aggregation strategies
		features.clear();
		features.push_back(SimpleCodeFeatureSpec(intAdd, FA_Static));
		features.push_back(SimpleCodeFeatureSpec(intAdd, FA_Weighted));
		features.push_back(SimpleCodeFeatureSpec(intAdd, FA_Real));
		features.push_back(SimpleCodeFeatureSpec(intAdd, FA_Polyhedral));

		features.push_back(SimpleCodeFeatureSpec(uintAdd, FA_Weighted));
		features.push_back(SimpleCodeFeatureSpec(uintAdd, FA_Real));
		features.push_back(SimpleCodeFeatureSpec(uintAdd, FA_Polyhedral));


		values = evalFeatures(forStmt, features);
		EXPECT_EQ(values[0], evalFeature(forStmt, SimpleCodeFeatureSpec(intAdd, FA_Static)));
		EXPECT_EQ(values[1], evalFeature(forStmt, SimpleCodeFeatureSpec(intAdd, FA_Weighted)));
		EXPECT_EQ(values[2], evalFeature(forStmt, SimpleCodeFeatureSpec(intAdd, FA_Real)));
		EXPECT_EQ(values[3], evalFeature(forStmt, SimpleCodeFeatureSpec(intAdd, FA_Polyhedral)));

		EXPECT_EQ(values[4], evalFeature(forStmt, SimpleCodeFeatureSpec(uintAdd, FA_Weighted)));
		EXPECT_EQ(values[5], evalFeature(forStmt, SimpleCodeFeatureSpec(uintAdd, FA_Real)));
		EXPECT_EQ(values[6], evalFeature(forStmt, SimpleCodeFeatureSpec(uintAdd, FA_Polyhedral)));

	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme
