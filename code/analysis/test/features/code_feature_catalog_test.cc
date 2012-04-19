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

#include "insieme/core/parser/ir_parse.h"

#include "insieme/analysis/features/code_feature_catalog.h"

namespace insieme {
namespace analysis {
namespace features {

using namespace core;

	TEST(CodeFeatureCatalog, Basic) {

		const FeatureCatalog& catalog = getFullCodeFeatureCatalog();

		EXPECT_NE(0u, catalog.size());
	}

	TEST(CodeFeatureCatalog, Extracting) {
		NodeManager mgr;
		parse::IRParser parser(mgr);

		// load some code sample ...
		auto stmt = parser.parseStatement(""
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i));"
				"	for(decl uint<4>:j = 5 .. 25 : 1) {"
				"		if ( (j < 10 ) ) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		} else {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i-j)));"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i-j)));"
				"		};"
				"	};"
				"}");



		FeatureCatalog	catalog = getFullCodeFeatureCatalog();
		catalog.addAll(features::getFullCodeFeatureCatalog());

		std::vector<features::FeaturePtr> features;

		features.push_back(catalog.getFeature("SCF_NUM_any_all_OPs_real"));            // 0
		features.push_back(catalog.getFeature("SCF_NUM_any_all_OPs_static"));          // 1
		features.push_back(catalog.getFeature("SCF_IO_NUM_any_read/write_OPs_real"));  // 2
		features.push_back(catalog.getFeature("SCF_NUM_function_calls_weighted"));     // 3
		features.push_back(catalog.getFeature("SCF_NUM_variables_lambda_static"));     // 4
		features.push_back(catalog.getFeature("SCF_NUM_branches_lambda_real"));        // 5

		std::vector<features::Value> values = analysis::features::extractFrom(stmt, features);

		EXPECT_EQ(1600,  analysis::features::getValue<double>(values.at(0)));
		EXPECT_EQ(5,     analysis::features::getValue<double>(values.at(1)));
		EXPECT_EQ(0,     analysis::features::getValue<double>(values.at(2)));
		EXPECT_EQ(40100, analysis::features::getValue<double>(values.at(3)));
		EXPECT_EQ(17,    analysis::features::getValue<double>(values.at(4)));
		EXPECT_EQ(800,   analysis::features::getValue<double>(values.at(5)));
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
